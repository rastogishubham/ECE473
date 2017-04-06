;Generate a list of rows
(define (generate-rows n row)
	(if (= n 0)
		row
		(generate-rows (- n 1) (cons 0 row))))

;Generates a list of rows with size n and row size m
(define (generate-board n m list-rows)
 	(if (= n 0)
 	list-rows
 	(generate-board (- n 1) m (cons (generate-rows m (list)) list-rows))))