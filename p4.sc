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
			(get-row-moves (+ x 1) y (rest row) (append list-moves (list (list (+ x 1) (+ y 1)))) len-row)
			(get-row-moves (+ x 1) y (rest row) list-moves len-row))))

;Returns list of all legal moves for a board
(define (get-moves b len y list-moves)
	(if (null? b)
		list-moves
		(get-moves (rest b) len (+ y 1) (get-row-moves 0 y (first b) list-moves len))))

;Returns the list of legal moves for a player in board b
(define (moves b)
	(get-moves b (len-row 0 (first b)) 0 (list)))

;Checks if move is legal or not and returns 1 if it is legal otherwise returns a 0
(define (check-legal-move move list-moves)
	(if (null? list-moves)
		0
		(if (equal? move (first list-moves))
			1
			(check-legal-move move (rest list-moves)))))

;Makes a move m on board b and returns the new board after checking if move is legal oherwise just returns original board
(define (make-move m b)
	b)