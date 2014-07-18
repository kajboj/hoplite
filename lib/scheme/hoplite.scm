(load (map
        (lambda (filename) (string-append "lib/scheme/" filename))
        '("boards.scm"
          "pieces.scm"
          "utils.scm"
          "display.scm"
          "parse.scm"
          "screen.scm"
          "ai.scm"
          "path.scm"
          "moves.scm")
        ))

(define (get-x coords) (car coords))
(define (get-y coords) (cadr coords))

(define (game-world hoplite enemies other-pieces altar)
  (list hoplite enemies other-pieces
        ((get-creator hole-def) '(1 0))
        altar))

(define (get-hoplite world) (car world))
(define (get-enemies world) (cadr world))
(define (get-other-pieces world) (caddr world))
(define (get-hole world) (cadddr world))
(define (get-altar world) (cadr (cdddr world)))

(define (get-non-visitable-pieces world)
  (append
    (get-other-pieces world)
    (get-enemies world)))

(define (get-pieces world)
  (append
    (list (get-hoplite world))
    (get-enemies world)
    (get-other-pieces world)
    (list (get-hole world))
    (list (get-altar world))))

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

(define (coords-div coords n)
  (map / coords (list n n)))

(define (coords-list-add coords-list coords)
  (map
    (lambda (x) (coords-add coords x))
    coords-list))

(define (shifted-on-board coords shifts)
  (filter on-board?
    (coords-list-add shifts coords)))))

(define (neighbours coords)
  (shifted-on-board coords coord-shifts))

(define (neighbours-2 coords)
  (shifted-on-board coords coord-shifts-2))

(define (is-neighbour? coords1 coords2)
  (coverage-check (neighbours coords1) coords2))

(define (coverage-check coords-list coords)
  (list-search-positive coords-list 
    (lambda (other) (coords=? other coords))))

(define (coverage-checker-positive coords-list)
  (lambda (coords)
    (coverage-check coords-list coords)))

(define (coverage-checker-negative coords-list)
  (let ((positive (coverage-checker-positive coords-list)))
    (lambda (coords) (not (positive coords)))))

(define (visitable-neighbours neighbours-f coords non-visitable-coords)
  (filter
    (coverage-checker-negative non-visitable-coords)
    (neighbours-f coords)))

(define coord-shifts
  '((-1  0) (1  0) (0 -1) (0  1) (1 -1) (-1  1)))

(define coord-shifts-2
  '((-2  0) (-2  1) (-2 2) (-1  2) (0 2) (1  1)
    (2  0) (2  -1) (2 -2) (1  -2) (0 -2) (-1  -1)))

(define hex-to-ascii-map
  (build-hex-to-ascii-map board-with-Xs))

(define (hex-to-ascii ascii-coords)
  (cadr (assoc ascii-coords hex-to-ascii-map)))

(define ascii-to-hex-map
  (map
    (lambda (pair) (list (cadr pair) (car pair)))
    hex-to-ascii-map))

(define (ascii-to-hex hex-coords)
  (cadr (assoc hex-coords ascii-to-hex-map)))

(let* ((world (parse-world screen hoplite-def enemy-defs other-pieces-defs))
       (hoplite-coords (get-coords (get-hoplite world)))
       (non-visitable-coords (map get-coords (get-non-visitable-pieces world)))

       (possible-moves
         (legal-moves (get-coords (get-hoplite world))
                      non-visitable-coords
                      can-leap?))

       (nearest-neighbours-generator
         (lambda (coords)
           (visitable-neighbours neighbours coords (map get-coords (get-other-pieces world)))))

       (goal (establish-goal world))

       (goal-distance-generator
         (lambda (start)
           (distance start goal nearest-neighbours-generator)))
       
       (move (list-sample
               (best-moves
                  hoplite-coords
                  possible-moves
                  (get-enemies world)
                  goal-distance-generator)))
       )
  
  (begin
    (displayn 
      (render-symbol
        " . "
        (move-coords move)
        (render-world world empty-board))
      )

    (displayn can-leap?)
    (displayn (move-coords move))
    (displayn (render-move move))
    ))

; (define m (make-move '(1 2) "hello"))
; (displayn m)
; (displayn (map move-coords (list m)))

  ; (displayn board-with-coords)
  ; (displayn 
  ;   (render-symbols
  ;     " . "
  ;     (neighbours-2 '(5 0))
  ;     empty-board))

; (let ((hex-coords-and-colors-list
;         (reject-empty-tiles (hex-coords-and-colors screen))))
;   (displayn board-with-coords)
;   (displayn hex-coords-and-colors-list)
;   )

; (displayn 
;   ((two-pixel-recognizer '((0 0 0) (100 100 100)))
;    '((-10 0 0) (100 100 100))))