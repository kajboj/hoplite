(load "boards.scm")
(load "utils.scm")
(load "display.scm")
(load "parse.scm")
(load "screen.scm")

(define hoplite-def (list
  (list 156 157 156)
  (lambda (coords) (piece "!H!" coords))))

(define lava-def (list
  (list 69 29 29)
  (lambda (coords) (piece "~~~" coords))))

(define footman-def (list
  (list 152 116 80)
  (lambda (coords) (piece " F " coords))))

(define demolitionist-def (list
  (list 159 82 82)
  (lambda (coords) (piece " D " coords))))

(define hole-def (list
  (list 98 97 98)
  (lambda (coords) (piece " # " coords))))

(define archer-def (list
  (list 116 153 80)
  (lambda (coords) (piece " A " coords))))

(define altar-def (list
  (list 193 194 193)
  (lambda (coords) (piece "alt" coords))))

(define wizard-def (list
  (list 147 81 114)
  (lambda (coords) (piece " W " coords))))

(define (get-color piece-def) (car piece-def))
(define (get-creator piece-def) (cadr piece-def))

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
(define (get-pieces world)
  (cons (get-hoplite world) (get-enemies world)))

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

; (displayn board-with-coords)
; (displayn (reject-empty-tiles (hex-coords-and-color screen)))

(displayn 
  (render-pieces 
    (parse-world screen
      (list
        hoplite-def
        lava-def
        footman-def
        demolitionist-def
        hole-def
        archer-def
        altar-def
        wizard-def))
    empty-board))
