(load "boards.scm")
(load "pieces.scm")
(load "utils.scm")
(load "display.scm")
(load "parse.scm")
(load "screen.scm")
(load "ai.scm")

(define (get-x coords) (car coords))
(define (get-y coords) (cadr coords))

(define (game-world hoplite enemies other-pieces)
  (list hoplite enemies other-pieces))

(define (get-hoplite world) (car world))
(define (get-enemies world) (cadr world))
(define (get-other-pieces world) (caddr world))

(define (get-non-hoplite-pieces world)
  (append (get-other-pieces world) (get-enemies world)))

(define (get-pieces world)
  (append
    (list (get-hoplite world))
    (get-enemies world)
    (get-other-pieces world)))

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

(define (coords-sub coords1 coords2)
  (map - coords1 coords2))

(define (coords-list-add coords-list coords)
  (map
    (lambda (x) (coords-add coords x))
    coords-list))

(define (shifted-on-board coords shifts)
  (filter on-board?
    (coords-list-add shifts coords))))

(define (neighbours radius coords)
  (shifted-on-board coords (coord-shifts radius)))

(define (is-neighbour? radius coords1 coords2)
  (coverage-check (neighbours radius coords1) coords2))

(define (coverage-check coords-list coords)
  (list-search-positive coords-list 
    (lambda (other) (coords=? other coords))))

(define (coverage-checker-positive coords-list)
  (lambda (coords)
    (coverage-check coords-list coords)))

(define (coverage-checker-negative coords-list)
  (let ((positive (coverage-checker-positive coords-list)))
    (lambda (coords) (not (positive coords)))))

(define (legal-moves hoplite non-hoplite-pieces)
  (filter
    (coverage-checker-negative (map get-coords non-hoplite-pieces))
    (neighbours 1 (get-coords hoplite))))

(define (coord-shifts radius)
  (filter
    (lambda (c)
      (and
       (>= radius (abs (+ (get-x c) (get-y c))))
       (not (and (= 0 (get-x c)) (= 0 (get-y c))))))
    (pairs (- radius) radius (- radius) radius)))

(let* ((world (parse-world screen hoplite-def enemy-defs other-pieces-defs))
       (hoplite-coords (get-coords (get-hoplite world)))
       (possible-moves
        (legal-moves (get-hoplite world) (get-non-hoplite-pieces world)))
       (move (list-sample (best-moves hoplite-coords possible-moves (get-enemies world)))))
  (begin
    (displayn 
      (render-symbols
        " . "
        (list move) 
        (render-world world empty-board))))

    (displayn move)
    (displayn (cadr (hex-to-ascii-coords move)))

    (displayn (ascii-coords-to-proportions (cadr (hex-to-ascii-coords move)))))

; (let* ((footman ((get-creator footman-def) '(5 0)))
;        (wizard ((get-creator wizard-def) '(7 0)))
;        (archer ((get-creator archer-def) '(4 2)))
;        (demol ((get-creator demolitionist-def) '(5 2)))
;        (bomb ((get-creator bomb-def) '(4 1)))
;        (world (game-world 
;          ((get-creator hoplite-def) '(10 0))
;          (list footman wizard archer demol bomb)
;          '())))
;   (begin
;     (displayn 
;       (render-world world board-with-coords))

;     (displayn
;       (select
;         (get-enemies world)
;         killable?))

;     (displayn 
;       (map 
;         (lambda (enemy) (cons (get-symbol enemy) (get-coords enemy)))
;         (kill (get-enemies world) '(6 0) '(5 1))))
;     ))