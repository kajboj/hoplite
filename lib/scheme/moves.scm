(define-structure moves by-one leap)

(define-structure move coords type)
(define (make-by-one coords) (make-move coords "by-one"))
(define (make-leap coords) (make-move coords "leap"))

(define (legal-moves-by-one coords non-visitable-coords)
  (map make-by-one
       (visitable-neighbours neighbours coords non-visitable-coords)))

(define (legal-moves-leap coords non-visitable-coords)
  (map make-leap
       (visitable-neighbours neighbours-2 coords non-visitable-coords)))

(define (legal-moves coords non-visitable-coords can-leap?)
  (make-moves
    (legal-moves-by-one coords non-visitable-coords)
    (if can-leap?
        (legal-moves-leap coords non-visitable-coords)
        '())))

(define can-leap?
  (color-within? 15 '(227 160 91) leap-color))