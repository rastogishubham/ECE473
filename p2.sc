;Main function of program
(define (truth-table phi) 
	(get-2-exp (list-len (get-props phi (list)) 0)))

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
	(display (expt 2 vars)))

;returns a list of false for the given length
;(define (get-false-list maxi cnt list-false)
;	(if (= maxi cnt)
;		list-false
;		(get-false-list maxi (+ cnt 1) (cons #f list-false))))

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
;Merges two lists together usually to merge a prop list and a bin list
(define (merge-lists list-props list-bin merged-list)
	(if (null? list-props)
		merged-list
		(merge-lists (rest list-props) (rest list-bin) (append merged-list (list (list (first list-props) (first list-bin)))))))