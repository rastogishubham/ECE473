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

;Change a row to reflect move of player
(define (change-row x row curr-col player new-row row-len)
	(if (= curr-col row-len)
		new-row
		(if (= x curr-col)
			(change-row x (rest row) (+ curr-col 1) player (append new-row (list player)) row-len)
			(change-row x (rest row) (+ curr-col 1) player (append new-row (list (first row))) row-len))))

;Go to row specified by move and change its correct column to make move
(define (go-to-row x y b curr-row player b-len b-prime)
	(if (= b-len curr-row)
		b-prime
		(if (= y curr-row)
			(go-to-row x y (rest b) (+ curr-row 1) player b-len (append b-prime (list (change-row x (first b) 0 player (list) b-len))))
			(go-to-row x y (rest b) (+ curr-row 1) player b-len (append b-prime (list (first b)))))))

;Makes a move m on board b and returns the new board after checking if move is legal oherwise just returns original board
(define (make-move m b)
	(if (= (check-legal-move (first (rest m)) (moves b)) 0)
		b
		(go-to-row (- (first (first (rest m))) 1) (- (last (first (rest m))) 1) b 0 (first m) (len-row 0 (first b)) (list))))

;Checks a row to see if a player won or not
(define (check-row row player row-len curr-col iter)
	(if (not (= player curr-col))
		0
		(if (= iter row-len)
			1
			(check-row (rest row) player row-len (first row) (+ iter 1)))))

;Checks if a player won by getting the rows
(define (win-row player b iter side-len)
	(if (= iter side-len)
		0
		(if (= 1 (check-row (first b) player side-len (first (first b)) 0))
			1
			(win-row player (rest b) (+ iter 1) side-len))))

;Returns which player won through rows only
(define (win-player-row player-list b side-len)
	(if (null? player-list)
		0
		(if (= (win-row (first player-list) b 0 side-len) 1)
			(first player-list)
			(win-player-row (rest player-list) b side-len))))

;Checks Column to see if a player won or not
;(define (check-col ))

;Returns if a player won
(define (win b)
	1)