(define (attack-count current-coords coords enemies)
  (fold-left
    (lambda (acc enemy)
      (+ acc
        (if (coverage-check (get-attack enemy) coords) 1 0)))
    0
    (kill enemies current-coords coords)))

(define (attack-counts current-coords coords-list enemies)
  (map
    (lambda (coords)
      (cons (attack-count current-coords coords enemies) coords))
    coords-list))

(define (best-moves current-coords coords-list enemies)
  (map cdr
    (all-min
      (lambda (attack-count) (car attack-count))
      (attack-counts current-coords coords-list enemies))))

(define (kill enemies old-coords new-coords)
  (reject
    enemies
    (lambda (enemy) 
      (if (killable? enemy)
        (killed? (get-coords enemy) old-coords new-coords)
        #f))))

(define (killed? enemy old-coords new-coords)
  (function-or
    (list stab lunge)
    (list enemy old-coords new-coords)))

(define (stab enemy-coords old-coords new-coords)
  (and
    (is-neighbour? 1 enemy-coords old-coords)
    (is-neighbour? 1 enemy-coords new-coords)))

(define (lunge enemy-coords old-coords new-coords)
  (coords=? enemy-coords
    (coords-add new-coords (coords-sub new-coords old-coords))))
