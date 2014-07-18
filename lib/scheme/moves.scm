(define-structure moves by-one leap)

(define-structure move coords type)
(define (make-by-one coords) (make-move coords "by-one"))
(define (make-leap coords) (make-move coords "leap"))

(define (legal-moves-maker coords non-visitable-coords move-maker)
  (map move-maker (visitable-neighbours coords non-visitable-coords)))

(define (legal-moves-by-one coords non-visitable-coords)
  (legal-moves-maker coords non-visitable-coords make-by-one))

(define (legal-moves-leap coords non-visitable-coords)
  (legal-moves-maker coords non-visitable-coords make-leap))

(define (legal-moves coords non-visitable-coords)
  (make-moves
    (legal-moves-by-one coords non-visitable-coords)
    (legal-moves-leap coords non-visitable-coords)))