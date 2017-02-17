;Main function of program
(define (truth-table phi) 
	(get-all-lists (list) (get-props phi (list)) (list-len (get-props phi (list)) 0) (get-2-exp (list-len (get-props phi (list)) 0))))

;Checks whether input is a proposition or not
(define (check-prop sym)
	(case sym
		((and) 0)
		((or) 0)
		((not) 0)
		((#t) 0)
		((#f) 0)
		(else 
			(if (symbol? sym) 
				1
				2))))

;gets a list of propositions from input
(define (get-props phi list1)
	(if(null? phi)
		(remove-dups list1)
		(if (= (check-prop (first phi)) 1)
			(get-props (rest phi) (append list1 (list (first phi))))
			(if (= (check-prop (first phi)) 0)
				(get-props (rest phi) list1) 
				(get-props (rest phi) (get-props (first phi) list1))))))

;Gets the length of a list
(define (list-len list1 len)
	(if (null? list1)
		len
		(list-len (rest list1) (+ len 1))))

;Gets 2^vars
(define (get-2-exp vars)
	(expt 2 vars))

;appends false to a list of bools
(define (append-false num list-bin)
	(if (= num 0)
		list-bin
		(append-false (- num 1) (cons #f list-bin))))

;Converts a number into a binary representation of #t -> 1 and #f -> 0
(define (convert-to-binary num list-bin)
	(if (= num 0)
		list-bin
		(if (= (remainder num 2) 1)
			(convert-to-binary (floor (/ num 2)) (cons #t list-bin))
			(convert-to-binary (floor (/ num 2)) (cons #f list-bin)))))

;creates a list of a number in binary form with #t -> 1 and #f -> 0
(define (create-bin-list num dig)
	(if (< (list-len (convert-to-binary num (list)) 0) dig)
		(append-false (- dig (list-len (convert-to-binary num (list)) 0)) (convert-to-binary num (list)))
		(convert-to-binary num (list))))

;removes duplicates in a list
(define (remove-dups list1)
	(if(null? list1)
		list1
		(if(= (check-dup (rest list1) (first list1)) 1)
			(remove-dups (rest list1))
			(cons (first list1) (remove-dups (rest list1))))))

;checks for duplicates
(define (check-dup list x)
	(if(null? list)
		0
		(if(eq? (first list) x)
			1
			(check-dup (rest list) x))))

;Replaces everything in a list with its truth value
(define (replace phi list-merge)
	(if (null? list-merge)
		phi
		(replace (replace-element phi (first (first list-merge)) (second (first list-merge))) (rest list-merge))))

;Replaces a specifc proposition with its truth value
(define (replace-element phi old new)
	(if (null? phi)
		phi
		(if (eq? (first phi) old)
			(cons new (replace-element (rest phi) old new))
			(if (list? (first phi))
				(cons (replace-element (first phi) old new) (replace-element (rest phi) old new))
				(cons (first phi) (replace-element (rest phi) old new))))))

;Merges two lists together usually to merge a prop list and a bin list
(define (merge-lists list-props list-bin merged-list)
	(if (null? list-props)
		merged-list
		(merge-lists (rest list-props) (rest list-bin) (append merged-list (list (list (first list-props) (first list-bin)))))))

;Creates a list of all bound values for a list of propositions
(define (get-all-lists list-all list-props list-len exp-val)
	(if (= 0 exp-val)
		list-all
		(get-all-lists (cons (merge-lists list-props (create-bin-list (- exp-val 1) list-len) (list)) list-all) list-props list-len (- exp-val 1))))

;And's two things together
(define (new-and x y)
  (and x y))

;or's two things together
(define (new-or x y)
  (or x y))

;Used to reduce a list of values to and
(define (my-and list-vals)
	(reduce new-and list-vals #t))

;Used to reduce a list of values to or
(define (my-or list-vals)
	(reduce new-or list-vals #f))

;Does the not for an argument
(define (my-not arg)
	(not (first arg)))

;main calculation function
(define (calculate phi)
	(if (boolean? phi)
		phi
		(case (first phi)
			((and) (my-and (map calculate (rest phi))))
			((or) (my-or (map calculate (rest phi))))
			((not) (my-not (map calculate (rest phi)))))))