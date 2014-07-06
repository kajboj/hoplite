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

(define (neighbours radius coords)
  (filter on-board?
    (map
      (lambda (x) (coords-add coords x))
      (coord-shifts radius))))

(define (coord-shifts radius)
  (filter
    (lambda (c)
      (and
       (>= radius (abs (+ (get-x c) (get-y c))))
       (not (and (= 0 (get-x c)) (= 0 (get-y c))))))
    (pairs (- radius) radius (- radius) radius)))

(define player (piece " P " '(10 0)))
(define enemy (piece " E " '(5 0)))

(displayn 
  (render-symbols " . "
    (neighbours 2 (get-coords enemy))
      (render-piece
        player
        (render-piece enemy empty-board))))