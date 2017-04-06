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

;Generates initial n x n empty board
(define (initial-board n)
	(generate-board n n (list)))

;Returns the list of legal moves for a player in board b
(define (moves b)
  0)

;Gets length of row
(define (len-row len row)
	(if (null? row)
		len
		(len-row (+ len 1) (rest row))))

;Get's legal moves of each row
(define (get-row-moves x y row list-moves len-row)
	(if (= x len-row)
		list-moves
		(if (= (first row) 0)
			(get-row-moves (+ x 1) y (rest row) (append list-moves (list (list x y))) len-row)
			(get-row-moves (+ x 1) y (rest row) list-moves len-row))))