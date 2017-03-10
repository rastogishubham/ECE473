
;;From p2.sc solutions
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

;;From p2.sc solutions
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

;;From p2.sc solutions
(define (lookup-proposition proposition truth-assignment)
 (cond ((null? truth-assignment) (panic "Proposition not in truth assignment"))
       ((eq? proposition (first (first truth-assignment)))
	(second (first truth-assignment)))
       (else (lookup-proposition proposition (rest truth-assignment)))))

;;From p2.sc solutions
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

;;From p2.sc solutions
(define (truth-table formula)
 (map (lambda (truth-assignment)
       (list truth-assignment (boolean-evaluate formula truth-assignment)))
      (all-truth-assignments (propositions-in formula))))

;;From slides lecture 6
(define (boolean-simplify e) (rewrite *rules* e))

;;From rewrite.sc
(define (pattern-variable? p) (member p '(phi phi1 phi2 phi3)))

;;From slides lecture 6
(define (pattern-list-variable? p) (member p '(phi... phi1... phi2... phi3...)))

;;From rewrite.sc
(define (inconsistent-binding? b1 b2)
 (and (eq? (first b1) (first b2)) (not (equal? (second b1) (second b2)))))

;;From rewrite.sc
(define (inconsistent-bindings? r1 r2)
 (some (lambda (b1) (some (lambda (b2) (inconsistent-binding? b1 b2)) r2)) r1))

;;From rewrite.sc
(define (merge-results-of-match r1 r2)
 (if (or (eq? r1 #f) (eq? r2 #f) (inconsistent-bindings? r1 r2))
     #f
     (append r1 r2)))

;;From slides lecture 6
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

;;From slides lecture 6
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

;;From slides lecture 6
(define (lookup-pattern-variable p bindings)
 (cond ((null? bindings) (panic "unbound pattern variable"))
       ((eq? (first (first bindings)) p) (second (first bindings)))
       (else (lookup-pattern-variable p (rest bindings)))))

;;From slides lecture 6
(define (applicable? rule expression)
   (not (memq #f (match (first rule) expression))))

;;From slides lecture 6
(define (first-applicable-rule rules expression)
   (cond ((null? rules) #f)
   ((applicable? (first rules) expression) (first rules))
   (else (first-applicable-rule (rest rules) expression))))

;;From slides lecture 6
(define (apply-rule rule expression)
(instantiate (third rule) (match (first rule) expression)))

;;From slides lecture 6
(define (apply-rules rules expression)
   (let ((rule (first-applicable-rule rules expression)))
      (if rule
      (rewrite rules (apply-rule rule expression))
   expression)))

;;From slides lecture 6
(define (rewrite rules expression)
   (if (list? expression)
      (apply-rules
         rules
      (map (lambda (expression) (rewrite rules expression))
         expression))
   expression))

(define *rules*
 '(((not #f) -~> #t)
   ((not #t) -~> #f)
   ((not (not phi)) -~> (phi))
   ((and) -~> #t)
   ((and phi) -~> phi)
   ((and phi #f) -~> #f)
   ((and phi #t) -~> (and phi))
   ((and #t phi...) -~> (and phi...))
   ((and #f phi...) -~> #f)
   ((and phi1 phi2 phi3 phi...) -~> (and phi1 (and phi2 (and phi3 phi...))))
   ((and phi (not phi) phi...) -~> #f)
   ((and (not phi) phi phi...) -~> #f)
   ((or) -~> #f)
   ((or phi) -~> phi)
   ((or phi #f) -~> (or phi))
   ((or phi #t) -~> #t)
   ((or #t phi...) -~> #t)
   ((or #f phi...) -~> (or phi...))
   ((or phi1 phi2 phi3 phi...) -~> (or phi1 (or phi2 (or phi3 phi...)))
   ((or phi (not phi) phi...) -~> #t)
   ((or (not phi) phi phi...) -~> #t))))



