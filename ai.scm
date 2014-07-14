(define (attack-count coords enemies)
  (fold-left
    (lambda (acc enemy)
      (+ acc
        (if (coverage-check (get-attack enemy) coords) 1 0)))
    0
    enemies))

(define (best-moves current-coords coords-list enemies goal-path-generator)
  (map cdr
    (all-min
      (lambda (scored-move) (car scored-move))
      (score-moves current-coords coords-list enemies goal-path-generator))))

(define (kill-count enemies enemies-after-move)
  (if (= (length enemies) (length enemies-after-move)) 0 -0.1))

(define (goal-drive new-coords goal-path)
  (if (coverage-check goal-path new-coords) -0.2 0))

(define (score-move current-coords move-coords enemies goal-path-generator)
  (let ((enemies-after-move (kill enemies current-coords move-coords)))
    (+
      (attack-count move-coords enemies-after-move)
      (kill-count enemies enemies-after-move)
      (goal-drive move-coords (goal-path-generator current-coords)))))

(define (score-moves current-coords moves-list enemies goal-path-generator)
  (map
    (lambda (move-coords)
      (cons
        (score-move current-coords move-coords enemies goal-path-generator)
        move-coords))
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
