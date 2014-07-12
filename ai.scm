(define (attack-count coords enemies)
  (fold-left
    (lambda (acc enemy)
      (+ acc
        (if (coverage-check (get-attack enemy) coords) 1 0)))
    0
    enemies))

(define (best-moves current-coords coords-list enemies)
  (map cdr
    (all-min
      (lambda (attack-count) (car attack-count))
      (score-moves current-coords coords-list enemies))))

(define (kill-count enemies enemies-after-move)
  (if (= (length enemies) (length enemies-after-move)) 0 -0.1))

(define (score-move current-coords move-coords enemies)
  (let ((enemies-after-move (kill enemies current-coords move-coords)))
    (+
      (attack-count move-coords enemies-after-move)
      (kill-count enemies enemies-after-move))))

(define (score-moves current-coords moves-list enemies)
  (map
    (lambda (move-coords)
      (cons (score-move current-coords move-coords enemies) move-coords))
    moves-list))

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
