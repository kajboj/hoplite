(load "boards.scm")
(load "pieces.scm")
(load "utils.scm")
(load "display.scm")
(load "parse.scm")
(load "screen.scm")
(load "ai.scm")

(define (get-color piece-def) (car piece-def))
(define (get-creator piece-def) (cadr piece-def))

(define (get-x coords) (car coords))
(define (get-y coords) (cadr coords))

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

(define (coords-list-add coords-list coords)
  (map
    (lambda (x) (coords-add coords x))
    coords-list))

(define (shifted-on-board coords shifts)
  (filter on-board?
    (coords-list-add shifts coords))))

(define (neighbours radius coords)
  (shifted-on-board coords (coord-shifts radius)))

(define (coverage-check coords-list coords)
  (list-search-positive coords-list 
    (lambda (other) (coords=? other coords))))

(define (coverage-checker-positive coords-list)
  (lambda (coords)
    (coverage-check coords-list coords)))

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
        (render-world world empty-board))))

    (displayn move)
    (displayn (cadr (hex-to-ascii-coords move)))

    (displayn (ascii-coords-to-proportions (cadr (hex-to-ascii-coords move)))))

; (let* ((footman1 ((get-creator archer-def) '(5 0)))
;        (footman2 ((get-creator archer-def) '(7 -4)))
;        (world (game-world 
;          ((get-creator hoplite-def) '(10 0))
;          (list footman1 footman2))))
;   (begin
;     (displayn 
;       (render-world world board-with-coords))

;     (displayn (attack-counts
;       (list '(7 -2) '(8 0) '(6 0))
;       (get-enemies world)))

;     (displayn (all-min
;       car '((1 2) (0 2) (3 1) (0 5))))

;     (displayn (function-or (list cdr cdr) '(())))))