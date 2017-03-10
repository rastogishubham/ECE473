(define (propositions-in formula)
 (cond ((symbol? formula) (list formula))
       ((boolean? formula) '())
       ((and (list? formula) (not (null? formula)))
	(case (first formula)
	 ((not) (if (= (length formula) 2)
		    (propositions-in (second formula))
		    (panic "Unrecognized formula")))
	 ((and) (reduce unionq (map propositions-in (rest formula)) '()))
	 ((or) (reduce unionq (map propositions-in (rest formula)) '()))
	 (else (panic "Unrecognized formula"))))
       (else (panic "Unrecognized formula"))))

(define (all-truth-assignments propositions)
 (if (null? propositions)
     '(())
     (let ((truth-assignments (all-truth-assignments (rest propositions))))
      (append (map (lambda (truth-assignment)
		    (cons (list (first propositions) #t) truth-assignment))
		   truth-assignments)
	      (map (lambda (truth-assignment)
		    (cons (list (first propositions) #f) truth-assignment))
		   truth-assignments)))))

(define (lookup-proposition proposition truth-assignment)
 (cond ((null? truth-assignment) (panic "Proposition not in truth assignment"))
       ((eq? proposition (first (first truth-assignment)))
	(second (first truth-assignment)))
       (else (lookup-proposition proposition (rest truth-assignment)))))

(define (boolean-evaluate formula truth-assignment)
 (cond ((symbol? formula) (lookup-proposition formula truth-assignment))
       ((boolean? formula) formula)
       ((and (list? formula) (not (null? formula)))
	(case (first formula)
	 ((not) (if (= (length formula) 2)
		    (not (boolean-evaluate (second formula) truth-assignment))
		    (panic "Unrecognized formula")))
	 ((and) (every (lambda (formula)
			(boolean-evaluate formula truth-assignment))
		       (rest formula)))
	 ((or) (some (lambda (formula)
		      (boolean-evaluate formula truth-assignment))
		     (rest formula)))
	 (else (panic "Unrecognized formula"))))
       (else (panic "Unrecognized formula"))))

(define (truth-table formula)
 (map (lambda (truth-assignment)
       (list truth-assignment (boolean-evaluate formula truth-assignment)))
      (all-truth-assignments (propositions-in formula))))

(define *simplify-rules*
 '(((+) -~-> 0)
   ((+ e) -~-> e)
   ((+ e 0) -~-> e)
   ((+ 0 e) -~-> e)
   ((+ e1 e2 e3 e...) -~-> (+ e1 (+ e2 (+ e3 e...))))
   ((- e) -~-> (* -1 e))
   ((- e1 e2) -~-> (+ e1 (- e2)))
   ((- e1 e2 e3 e...) -~-> (- e1 (+ e2 e3 e...)))
   ((*) -~-> 1)
   ((* e) -~-> e)
   ((* e 0) -~-> 0)
   ((* 0 e) -~-> 0)
   ((* e 1) -~-> e)
   ((* 1 e) -~-> e)
   ((* e1 e2 e3 e...) -~-> (* e1 (* e2 (* e3 e...))))
   ((/ e) -~-> (expt e -1))
   ((/ e1 e2) -~-> (* e1 (/ e2)))
   ((/ e1 e2 e3 e...) -~-> (/ e1 (* e2 e3 e...)))
   ((expt e 1) -~-> e)
   ((sqrt e) -~-> (expt e 0.5))
   ((and e1 e1) -~> (and e1))))

(define (simplify e) (rewrite *rules* e))

;;; A rule is:
;;;  (e1 -~-> e2) where e1 and e2 are patterns

;;; A pattern is:
;;;  a pattern variable,
;;;  an expression constant, or
;;;  (p1 ... pn) where p1 ... pn are patterns

;;; (match p e) ==> either #f or a list of bindings

;;; A binding is:
;;;  (p e) where p is a pattern variable and e is an expression

;;; (rewrite '(+ (* 2 (expt x 3)) 0) '(((+ e 0) -~-> e)))

(define (pattern-variable? p) (member p '(phi phi1 phi2 phi3)))

(define (pattern-list-variable? p) (member p '(phi... phi1... phi2... phi3...)))

(define (inconsistent-binding? b1 b2)
 (and (eq? (first b1) (first b2)) (not (equal? (second b1) (second b2)))))

(define (inconsistent-bindings? r1 r2)
 (some (lambda (b1) (some (lambda (b2) (inconsistent-binding? b1 b2)) r2)) r1))

(define (merge-results-of-match r1 r2)
 (if (or (eq? r1 #f) (eq? r2 #f) (inconsistent-bindings? r1 r2))
     #f
     (append r1 r2)))


(define (match pattern expression)
(cond ((pattern-variable? pattern) (list (list pattern expression)))
   ((pattern-list-variable? pattern)
      (panic "Pattern list variable not at end of list"))
   ((and (list? pattern)
      (= (length pattern) 1)
      (pattern-list-variable? (first pattern)))
   (list (list (first pattern) expression)))
   ((and (list? pattern) (not (null? pattern)))
      (if (and (list? expression) (not (null? expression)))
         (append (match (first pattern) (first expression))
            (match (rest pattern) (rest expression)))
         (list #f)))
((equal? pattern expression) '())
(else (list #f))))


(define (instantiate pattern bindings)
   (cond ((pattern-variable? pattern)
      (lookup-pattern-variable pattern bindings))
   ((pattern-list-variable? pattern)
      (panic "Pattern list variable not at end of list"))
   ((and (list? pattern)
   (= (length pattern) 1)
   (pattern-list-variable? (first pattern)))
   (lookup-pattern-variable (first pattern) bindings))
   ((and (list? pattern) (not (null? pattern)))
   (cons (instantiate (first pattern) bindings)
      (instantiate (rest pattern) bindings)))
(else pattern)))

;;; (match 'x 'x) ==> ()
;;; (match '+ '*) ==> #f

;;; (match '(e1 e2) '(x y)) ==> ((e1 x) (e2 y))
;;;   (match 'e1 'x) ==> ((e1 x))
;;;   (match 'e2 'y) ==> ((e2 y))

;;; (match '(e1 e1) '(x y)) ==> #f
;;;   (match 'e1 'x) ==> ((e1 x))
;;;   (match 'e1 'y) ==> ((e2 y))

;;; (match '(e1) '(x y)) ==> #f

;;; (match '(e1 e2) '(x)) ==> #f

;;; (match '(+ e2) '(* x)) ==> #f
;;;   (match '+ '*) ==> #f
;;;   (match 'e2 'x) ==> ((e2 x))

;;; (match '(e1 e2) 'x) ==> #f

;;; (and e1 e1) -~-> (and e1)
;;; (and p p)
;;; (match '(and e1 e1) '(and p p)) ==> ((e1 p))

(define (lookup-pattern-variable p bindings)
 (cond ((null? bindings) (panic "unbound pattern variable"))
       ((eq? (first (first bindings)) p) (second (first bindings)))
       (else (lookup-pattern-variable p (rest bindings)))))

;(define (instantiate p bindings)
 ;(define (instantiate-with-bindings p) (instantiate p bindings))
 ;(cond ((pattern-variable? p) (lookup-pattern-variable p bindings))
  ;     ((list? p) (map instantiate-with-bindings p))
   ;    (else p)))

;(define (first-matching-rule e rules)
 ;(cond ((null? rules) #f)
  ;     ((not (eq? (match (first (first rules)) e) #f)) (first rules))
   ;    (else (first-matching-rule e (rest rules)))))

;(define (rewrite e rules)
 ;(define (rewrite-with-rules e) (rewrite e rules))
 ;(let ((e (if (list? e) (map rewrite-with-rules e) e)))
  ;(let ((rule (first-matching-rule e rules)))
   ;(if (eq? rule #f)
    ;   e
     ;  (rewrite (instantiate (third rule) (match (first rule) e)) rules)))))

(define (applicable? rule expression)
   (not (memq #f (match (first rule) expression))))

(define (first-applicable-rule rules expression)
   (cond ((null? rules) #f)
   ((applicable? (first rules) expression) (first rules))
   (else (first-applicable-rule (rest rules) expression))))

(define (apply-rule rule expression)
(instantiate (third rule) (match (first rule) expression)))

(define (apply-rules rules expression)
   (let ((rule (first-applicable-rule rules expression)))
      (if rule
      (rewrite rules (apply-rule rule expression))
   expression)))

(define (rewrite rules expression)
   (if (list? expression)
      (apply-rules
         rules
      (map (lambda (expression) (rewrite rules expression))
         expression))
   expression))


;;; (and e1 e1) -~-> (and e1)

;;; (f (and p p)) -~-> (f (and p))

;;; (rewrite '(f (and p p)) *rules*) ==> (f (and p))

(define *rules*
 '(((not #f) -~> #t)
   ((not #t) -~> #f)
   ((not (not phi)) -~> (phi))
   ((and) -~> #t)
   ((and phi) -~> phi)
   ((or phi) -~> phi)
   ((and phi #f) -~> #f)
   ((and phi #t) -~> (and phi))
   ((and #t phi...) -~> (and phi...))
   ((and #f phi...) -~> #f)
   ((and phi1 phi2 phi3 phi...) -~> (and phi1 (and phi2 (and phi3 phi...))))))

