(define (check-dup list x)
	(if(null? list)
		0
		(if(= (first list) x)
			1
			(check-dup (rest list) x))))

(define (remove-one x list)
	(cond ((null? list) (list))
		((= (first list) x) (rest list))
		(else (cons (first list) (remove-one x (rest list))))))

(define (remove-dups list1)
	(if(null? list1)
		list1
		(if(= (check-dup (rest list1) (first list1)) 1)
			(remove-dups (rest list1))
			(cons (first list1) (remove-dups (rest list1))))))

(define (set-union list1 list2)
	(if(null? list1)
		list2
		(if(null? list2)
			list1
			(if(= (check-dup list2 (first list1)) 0)
				(set-union (rest list1) (cons (first list1) list2))
				(set-union (rest list1) list2)))))

(define (set-intersection list1 list2)
	(if(null? list1)
			(list)
			(if(= (check-dup list2 (first list1)) 0)
				(set-intersection (rest list1) list2)
				(cons (first list1) (set-intersection (rest list1) list2)))))

(define (set-minus list1 list2)
  (if(null? list2)
  	list1
  	(if(= (check-dup list1 (first list2)) 1)
  		(set-minus (remove-one (first list2) list1) (rest list2))
		(set-minus list1 (rest list2)))))