(load "boards.scm")
(load "pieces.scm")
(load "utils.scm")
(load "display.scm")
(load "parse.scm")
(load "screen.scm")
(load "ai.scm")
(load "path.scm")

(define (get-x coords) (car coords))
(define (get-y coords) (cadr coords))

(define (game-world hoplite enemies other-pieces hole)
  (list hoplite enemies other-pieces hole))

(define (get-hoplite world) (car world))
(define (get-enemies world) (cadr world))
(define (get-other-pieces world) (caddr world))
(define (get-hole world) (cadddr world))

(define (get-non-hoplite-pieces world)
  (append (get-other-pieces world) (get-enemies world)))

(define (get-pieces world)
  (append
    (list (get-hoplite world))
    (get-enemies world)
    (get-other-pieces world)
    (list (get-hole world))))

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

(define (legal-moves coords non-visitable-coords)
  (filter
    (coverage-checker-negative non-visitable-coords)
    (neighbours 1 coords)))

(define (coord-shifts radius)
  (filter
    (lambda (c)
      (and
       (>= radius (abs (+ (get-x c) (get-y c))))
       (not (and (= 0 (get-x c)) (= 0 (get-y c))))))
    (pairs (- radius) radius (- radius) radius)))

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
       (non-visitable-coords (map get-coords (get-non-hoplite-pieces world)))
       (possible-moves
         (legal-moves (get-coords (get-hoplite world)) non-visitable-coords))

       (neighbours-generator
         (lambda (coords)
           (legal-moves coords (map get-coords (get-other-pieces world)))))

       (goal-distance-generator
         (lambda (start)
           (distance
             start
             (get-coords (get-hole world))
             neighbours-generator)))
       
       (move (list-sample
               (best-moves
                  hoplite-coords
                  possible-moves
                  (get-enemies world)
                  goal-distance-generator)))
       (move-ascii-coords (hex-to-ascii move)))
  
  (begin
    (displayn 
      (render-symbol
        " . "
        move
        (render-world 
          world
          empty-board)))
    (displayn move)
    (displayn (ascii-coords-to-proportions move-ascii-coords))
    
    ))


; (let ((hex-coords-and-colors-list
;         (reject-empty-tiles (hex-coords-and-colors screen))))
;   (displayn board-with-coords)
;   (displayn hex-coords-and-colors-list)
;   )

; (displayn 
;   ((two-pixel-recognizer '((0 0 0) (100 100 100)))
;    '((-10 0 0) (100 100 100))))