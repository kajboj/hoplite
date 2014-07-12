(define (piece symbol coords)
  (list symbol coords))

(define (enemy symbol coords attack)
  (list symbol coords attack))

(define (get-symbol piece) (car piece))
(define (get-coords piece) (cadr piece))
(define (get-attack enemy) (caddr enemy))

(define (get-color piece-def) (cadr piece-def))
(define (get-creator piece-def) (caddr piece-def))

(define (empty-tile? color)
  (is-tile-type? color empty-def))

(define (is-tile-type? color piece-def)
  (color-within? 15 color (cadr piece-def)))

(define (killable? enemy)
  (not (string=? (get-symbol bomb-def) (get-symbol enemy))))

(define empty-def (list
  "   "
  (list 66 66 66)
  (lambda (coords) (piece (car empty-def) coords))))

(define hoplite-def (list
  "!H!"
  (list 156 157 156)
  (lambda (coords) (piece (car hoplite-def) coords))))

(define footman-def (list
  " F "
  (list 152 116 80)
  (lambda (coords)
    (enemy (car footman-def) coords
      (neighbours 1 coords)))))

(define archer-def (list
  " A "
  (list 116 153 80)
  (lambda (coords)
    (enemy (car archer-def) coords
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
  " D "
  (list 142 76 77)
  (lambda (coords) (enemy (car demolitionist-def) coords (list)))))

(define bomb-def (list
  " b "
  (list 171 81 82)
  (lambda (coords)
    (enemy (car bomb-def) coords
      (neighbours 1 coords)))))

(define wizard-def (list
  " W "
  (list 147 81 114)
  (lambda (coords)
    (enemy (car wizard-def) coords
      (shifted-on-board coords
          (list
            '(-5  0) '(-4  0) '(-3  0) '(-2  0) '(-1  0)
            '( 5  0) '( 4  0) '( 3  0) '( 2  0) '( 1  0)
            '( 0 -5) '( 0 -4) '( 0 -3) '( 0 -2) '( 0 -1)
            '( 0  5) '( 0  4) '( 0  3) '( 0  2) '( 0  1)
            '( 5 -5) '( 4 -4) '( 3 -3) '( 2 -2) '( 1 -1)
            '(-5  5) '(-4  4) '(-3  3) '(-2  2) '(-1  1)
            ))))))

(define lava-def (list
  "~~~"
  (list 69 29 29)
  (lambda (coords) (piece (car lava-def) coords))))

(define hole-def (list
  " # "
  (list 98 97 98)
  (lambda (coords) (piece (car hole-def) coords))))

(define altar-def (list
  "alt"
  (list 193 194 193)
  (lambda (coords) (piece (car altar-def) coords))))

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