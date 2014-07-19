(define (piece symbol coords)
  (list symbol coords))

(define (enemy symbol coords attack)
  (list symbol coords attack))

(define (get-symbol piece) (car piece))
(define (get-coords piece) (cadr piece))
(define (get-attack enemy) (caddr enemy))

(define (get-color piece-def) (cadr piece-def))
(define (get-creator piece-def) (caddr piece-def))

(define (empty-tile? colors)
  (is-tile-type? colors empty-def))

(define (is-tile-type? colors piece-def)
  ((cadr piece-def) colors))

(define (single-color-recognizer piece-color)
  (lambda (screen-colors)
    (color-within? 15 (car screen-colors) piece-color)))

(define (or-recognizer recognizers)
  (lambda (screen-colors)
    (fold-left
      (lambda (acc recognizer)
        (or acc (recognizer screen-colors)))
      #f
      recognizers)))

(define (two-pixel-recognizer piece-color-list)
  (lambda (screen-colors)
    (fold-left
      (lambda (acc pair-to-compare)
        (and acc (color-within? 15 (car pair-to-compare) (cadr pair-to-compare))))
      #t
      (zip piece-color-list screen-colors))))

(define (killable? enemy)
  (not (string=? (get-symbol bomb-def) (get-symbol enemy))))

(define empty-def (list
  "   "
  (single-color-recognizer '(66 66 66))
  (lambda (coords) (piece (car empty-def) coords))))

(define hoplite-def (list
  "!H!"
  (or-recognizer (map two-pixel-recognizer
    '(((141 113 83) (120 119 81))
      ((153 154 153) (112 113 87)))))
  (lambda (coords) (piece (car hoplite-def) coords))))

(define footman-def (list
  " F "
  (two-pixel-recognizer '((147 113 78) (101 87 73)))
  (lambda (coords)
    (enemy (car footman-def) coords
      (neighbours coords)))))

(define archer-def (list
  " A "
  (single-color-recognizer '(116 153 80))
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
  (two-pixel-recognizer '((158 83 84) (136 80 80)))
  (lambda (coords) (enemy (car demolitionist-def) coords (list)))))

(define bomb-def (list
  " b "
  (two-pixel-recognizer '((172 83 83) (87 73 73)))
  (lambda (coords)
    (enemy (car bomb-def) coords
      (neighbours coords)))))

(define wizard-def (list
  " W "
  (single-color-recognizer '(147 81 114))
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
  (single-color-recognizer '(69 29 29))
  (lambda (coords) (piece (car lava-def) coords))))

(define hole-def (list
  " # "
  (single-color-recognizer '(98 97 98))
  (lambda (coords) (piece (car hole-def) coords))))

(define portal-def (list
  " @ "
  (single-color-recognizer '(93 77 109))
  (lambda (coords) (piece (car portal-def) coords))))

(define altar-def (list
  "alt"
  (single-color-recognizer '(193 194 193))
  (lambda (coords) (piece (car altar-def) coords))))

(define enemy-defs (list
  footman-def
  archer-def
  demolitionist-def
  bomb-def
  wizard-def))

(define other-pieces-defs (list
  lava-def
  portal-def
  altar-def))

(define piece-defs (append
                     (list hoplite-def)
                     enemy-defs
                     other-pieces-defs
                     (list hole-def)))

(define (get-pieces world)
  (append
    (list (get-hoplite world))
    (get-enemies world)
    (get-other-pieces world)
    (list (get-hole world))))

