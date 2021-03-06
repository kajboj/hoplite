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
          "moves.scm"
          "debug.scm")
        ))

(define (get-x coords) (car coords))
(define (get-y coords) (cadr coords))

(define-structure world hoplite enemies other-pieces hole altar used-altar)

(define (has-altar? world) (null? (world-altar world)))

(define (get-non-visitable-pieces world)
  (append
    (world-other-pieces world)
    (world-enemies world)))

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

(let* (
       (world (parse-world screen hoplite-def enemy-defs other-pieces-defs))
       (hoplite-coords (get-coords (world-hoplite world)))

       (nearest-neighbours-generator
         (lambda (coords)
           (visitable-neighbours neighbours coords (map get-coords (world-other-pieces world)))))

       (hole-coords (get-coords (world-hole world)))

       (non-visitable-coords
           (map get-coords (get-non-visitable-pieces world)))

       (hole-connection-checker
         (lambda (coords) (connected? coords hole-coords nearest-neighbours-generator)))

       (possible-moves
         (legal-moves (get-coords (world-hoplite world))
                      non-visitable-coords
                      can-leap?))

       (goal (establish-goal world))

       (goal-distance-generator
         (lambda (start)
           (distance start goal nearest-neighbours-generator)))
       
       (move (list-sample
               (best-moves
                  hoplite-coords
                  possible-moves
                  (world-enemies world)
                  goal-distance-generator
                  hole-connection-checker)))
       )
  
  (begin
    (displayn 
      (render-symbol
        " . "
        (move-coords move)
        (render-world world empty-board))
      )

    (displayn (move-coords move))
    (displayn (render-move move))))
  ))