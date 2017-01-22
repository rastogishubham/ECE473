(define (check-dup list x)
	(if(null? list)
		0
		(if(= (first list) x)
			1
			(check-dup (rest list) x))))

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