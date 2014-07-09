(define hoplite-def (list
  (list 156 157 156)
  (lambda (coords) (piece "!H!" coords))))

(define lava-def (list
  (list 69 29 29)
  (lambda (coords) (piece "~~~" coords))))

(define footman-def (list
  (list 152 116 80)
  (lambda (coords) (piece " F " coords))))

(define demolitionist-def (list
  (list 142 76 77)
  (lambda (coords) (piece " D " coords))))

(define hole-def (list
  (list 98 97 98)
  (lambda (coords) (piece " # " coords))))

(define archer-def (list
  (list 116 153 80)
  (lambda (coords) (piece " A " coords))))

(define altar-def (list
  (list 193 194 193)
  (lambda (coords) (piece "alt" coords))))

(define wizard-def (list
  (list 147 81 114)
  (lambda (coords) (piece " W " coords))))

(define piece-defs (list
  hoplite-def
  lava-def
  footman-def
  demolitionist-def
  hole-def
  archer-def
  altar-def
  wizard-def))
