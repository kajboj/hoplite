(define (attack-count coords enemies)
  (fold-left
    (lambda (acc enemy)
      (+ acc
        (if (coverage-check (get-attack enemy) coords) 1 0)))
    0
    enemies))

(define (attack-counts coords-list enemies)
 (map
  (lambda (coords)
   (cons (attack-count coords enemies) coords))
  coords-list))

(define (safest-moves coords-list enemies)
  (map cdr
    (all-min
      (lambda (attack-count) (car attack-count))
      (attack-counts coords-list enemies))))

; (define (kill enemies old-coords new-coords)
;  (list-transform-negative
;   (lambda (enemy) 
;    (killed? enemy old-coords new-coords)
;    enemies)))

; (define (killed? enemy old-coords new-coords)
;  (function-or
;   (list stab lunge)
;   (list enemy old-coords new-coords)))

