(load "boards.scm")
(load "utils.scm")
(load "display.scm")

(define (get-x coords) (car coords))
(define (get-y coords) (cadr coords))

(define (get-symbol piece) (car piece))
(define (get-coords piece) (cadr piece))

(define (piece symbol coords)
  (list symbol coords))

(define player (piece "XXX" '(5 -2)))

(displayn 
  (render-piece player empty-board board-with-coords))