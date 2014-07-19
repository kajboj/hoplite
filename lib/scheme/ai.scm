(define (establish-goal world)
  (let ((killable-enemies (select (world-enemies world) killable?)))
    (if (any? killable-enemies)
      (get-coords (car killable-enemies))
      (get-coords (car killable-enemies)))))

(define (attack-count coords enemies)
  (fold-left
    (lambda (acc enemy)
      (+ acc
        (if (coverage-check (get-attack enemy) coords) 1 0)))
    0
    enemies))

(define (best-moves current-coords moves enemies
                    goal-distance-generator hole-connection-checker)
  (let* (
         (best-moves-picker (lambda (moves)
           (all-min car
                    (score-moves current-coords
                                 moves
                                 enemies
                                 goal-distance-generator))))

         (best-by-one-moves (best-moves-picker (moves-by-one moves)))
         
         (leap-moves-connected-to-hole
           (select (moves-leap moves)
             (lambda (leap) (hole-connection-checker (move-coords leap)))))
         
         (best-leap-moves (best-moves-picker leap-moves-connected-to-hole)))
    (map cdr 
         (if (any-leaps? moves)
             (if (all-by-one-bad? best-by-one-moves)
                 (best-of-all best-leap-moves best-by-one-moves)
                 (if (not-many-enemies? enemies)
                     (best-of-all best-leap-moves best-by-one-moves)
                     best-by-one-moves))
             best-by-one-moves))))

(define (best-of-all leaps by-ones)
  (if (leaps-better-than-by-ones? leaps by-ones)
    leaps
    by-ones))

(define (leaps-better-than-by-ones? leaps by-ones)
  (< (caar leaps) (caar by-ones)))

(define (not-many-enemies? enemies) (< (length enemies) 4))
(define (all-by-one-bad? moves) (or (null? moves) (> (caar moves) 0)))
(define (any-leaps? moves) (any? (moves-leap moves)))

(define (kill-count enemies enemies-after-move)
  (* -0.2 (- (length enemies) (length enemies-after-move))))

(define (goal-drive new-coords goal-distance max-distance)
  (* -0.1 (- max-distance goal-distance)))

(define (score-move current-coords move-coords enemies goal-distance max-distance)
  (let ((enemies-after-move (kill enemies current-coords move-coords)))
    (+
      (attack-count move-coords enemies-after-move)
      (kill-count enemies enemies-after-move)
      (goal-drive move-coords goal-distance max-distance))))

(define (score-moves current-coords moves enemies goal-distance-generator)
  (let ((goal-distances (goal-distances-for-moves moves goal-distance-generator)))
    (map
      (lambda (move)
        (cons
          (score-move
            current-coords
            (move-coords move)
            enemies
            (cdr (assoc move goal-distances))
            (cdr (list-max cdr goal-distances)))
          move))
      moves)))

(define (goal-distances-for-moves moves goal-distance-generator)
  (map
    (lambda (move)
      (cons move (goal-distance-generator (move-coords move))))
    moves))

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
    (is-neighbour? enemy-coords old-coords)
    (is-neighbour? enemy-coords new-coords)))

(define (lunge enemy-coords old-coords new-coords)
  (or
    (coords=? enemy-coords
              (coords-add new-coords (coords-sub new-coords old-coords)))
    (coords=? enemy-coords
              (coords-add new-coords (coords-div (coords-sub new-coords old-coords) 2)))))
