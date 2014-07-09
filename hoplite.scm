(load "boards.scm")
(load "pieces.scm")
(load "utils.scm")
(load "display.scm")
(load "parse.scm")
(load "screen.scm")

(define (get-color piece-def) (car piece-def))
(define (get-creator piece-def) (cadr piece-def))

(define (get-x coords) (car coords))
(define (get-y coords) (cadr coords))

(define (piece symbol coords)
  (list symbol coords))

(define (get-symbol piece) (car piece))
(define (get-coords piece) (cadr piece))

(define (hoplite? piece) (string=? "!H!" (get-symbol piece)))

(define (game-world hoplite enemies)
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

(define (coverage-checker-positive coords-list)
  (lambda (coords)
    (list-search-positive coords-list 
      (lambda (other) (coords=? other coords)))))

(define (coverage-checker-negative coords-list)
  (let ((positive (coverage-checker-positive coords-list)))
    (lambda (coords) (not (positive coords)))))

(define (legal-moves hoplite world)
  (filter
    (coverage-checker-negative (map get-coords (get-enemies world)))
    (neighbours 1 (get-coords hoplite))))

(define (coord-shifts radius)
  (filter
    (lambda (c)
      (and
       (>= radius (abs (+ (get-x c) (get-y c))))
       (not (and (= 0 (get-x c)) (= 0 (get-y c))))))
    (pairs (- radius) radius (- radius) radius)))

(let* ((world (parse-world screen piece-defs))
       (move (list-sample (legal-moves (get-hoplite world) world))))
  (begin
    (displayn 
      (render-symbols
        " . "
        (list move) 
        (render-world world board-with-coords))))

    (displayn move)
    (displayn (cadr (hex-to-ascii-coords move)))

    (displayn (ascii-coords-to-proportions (cadr (hex-to-ascii-coords move)))))
