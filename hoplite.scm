(load "boards.scm")
(load "utils.scm")
(load "display.scm")
(load "parse.scm")
(load "screen.scm")

(define (get-x coords) (car coords))
(define (get-y coords) (cadr coords))

(define (piece symbol coords)
  (list symbol coords))

(define (get-symbol piece) (car piece))
(define (get-coords piece) (cadr piece))

(define (world hoplite enemies)
  (list hoplite enemies))

(define (get-hoplite world) (car world))
(define (get-enemies world) (cadr world))

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

(define hoplite (piece " H " '(10 0)))
(define footman (piece " F " '(5 0)))
(define enemies (list footman))
(define game-world (world hoplite enemies))

(displayn 
  (render-symbols " . "
    (neighbours 2 (get-coords footman))
      (render-world game-world empty-board)))

(displayn (hex-coords-and-color screen))

; (displayn
;   (hoplite-move game-world))