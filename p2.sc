(define (truth-table phi) 
	1)

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

(define (get-props phi list1)
	(if(null? phi)
		list1
		(if (= (check-prop (first phi)) 1)
			(get-props (rest phi) (cons (first phi) list1))
			(if (= (check-prop (first phi)) 0)
				(get-props (rest phi) list1) 
				(get-props (rest phi) (get-props (first phi) list1))))))

(define (get-len vars)
	(expt 2 vars))
