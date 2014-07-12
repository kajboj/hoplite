(define (piece symbol coords)
  (list symbol coords))

(define (enemy symbol coords attack)
  (list symbol coords attack))

(define (get-symbol piece) (car piece))
(define (get-coords piece) (cadr piece))
(define (get-attack enemy) (caddr enemy))

(define (empty-tile? color)
  (is-tile-type? color empty-def))

(define (is-tile-type? color piece-def)
  (color-within? 15 color (get-color piece-def)))

(define empty-def (list
  (list 66 66 66)
  (lambda (coords) (piece "   " coords))))

(define hoplite-def (list
  (list 156 157 156)
  (lambda (coords) (piece "!H!" coords))))

(define footman-def (list
  (list 152 116 80)
  (lambda (coords)
    (enemy " F "coords
      (neighbours 1 coords)))))

(define archer-def (list
  (list 116 153 80)
  (lambda (coords)
    (enemy " A "coords
      (shifted-on-board coords
          (list
            '(-5  0) '(-4  0) '(-3  0) '(-2  0)
            '( 5  0) '( 4  0) '( 3  0) '( 2  0)
            '( 0 -5) '( 0 -4) '( 0 -3) '( 0 -2)
            '( 0  5) '( 0  4) '( 0  3) '( 0  2)
            '( 5 -5) '( 4 -4) '( 3 -3) '( 2 -2)
            '(-5  5) '(-4  4) '(-3  3) '(-2  2)
            ))))))

(define demolitionist-def (list
  (list 142 76 77)
  (lambda (coords) (enemy " D " coords (list)))))

(define bomb-def (list
  (list 171 81 82)
  (lambda (coords)
    (enemy " b "coords
      (neighbours 1 coords)))))

(define wizard-def (list
  (list 147 81 114)
  (lambda (coords) (piece " W " coords))))

(define lava-def (list
  (list 69 29 29)
  (lambda (coords) (piece "~~~" coords))))

(define hole-def (list
  (list 98 97 98)
  (lambda (coords) (piece " # " coords))))

(define altar-def (list
  (list 193 194 193)
  (lambda (coords) (piece "alt" coords))))

(define enemy-defs (list
  footman-def
  archer-def
  demolitionist-def
  bomb-def
  wizard-def))

(define other-pieces-defs (list
  lava-def
  hole-def
  altar-def))

(define piece-defs (append (list hoplite-def) enemy-defs other-pieces-defs))