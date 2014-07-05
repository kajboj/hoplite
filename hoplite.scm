(load "boards.scm")
(load "utils.scm")
(load "display.scm")

(define (get-x coords) (car coords))
(define (get-y coords) (cadr coords))

(define (get-symbol piece) (car piece))
(define (get-coords piece) (cadr piece))

(define (piece symbol coords)
  (list symbol coords))

(define (on-board? coords)
  (let ((x (get-x coords)) (y (get-y coords)))
    (and
      (>= x 0)
      (<= y 4)
      (<= (+ x y) 10)
      (<= x 10)
      (>= y -4)
      (>= (+ x y) 0))))

(define (coords-add coords1 coords2)
  (map + coords1 coords2))

(define (neighbours coords)
  (map
    (lambda (x) (coords-add coords x))
    (list '(-1 0) '(-1 1) '(0 1) '(1 0) '(1 -1) '(0 -1))))

(define player (piece " P " '(5 0)))
(define enemy (piece " E " '(10 0)))

(displayn 
  (render-piece
    player
    (render-piece enemy board-with-coords board-with-coords)
    board-with-coords))

(displayn (neighbours (get-coords player)))
