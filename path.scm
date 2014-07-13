(define (flood-fill visited fringes max-depth depth neighbour-generator)
  (begin
    ; (displayn (render-symbols " . " visited empty-board))
    ; (displayn depth)
    ; (displayn visited)
    ; (displayn fringes)
    ; (displayn "-------")
    (if (= max-depth depth)
        fringes
        (let ((new-fringe (all-non-visited-neighbours
                            (car fringes)
                            visited
                            neighbour-generator)))
          (flood-fill
            (append new-fringe visited)
            (cons new-fringe fringes)
            max-depth
            (+ 1 depth)
            neighbour-generator)))))

(define (all-non-visited-neighbours coords-list visited neighbour-generator)
  (car (fold-left
         (lambda (acc coords)
           (let ((just-visited (non-visited-neighbours
                                 coords
                                 (cdr acc)
                                 neighbour-generator)))
             (cons
               (append just-visited (car acc))
               (append just-visited (cdr acc)))))
         (cons '() visited)
         coords-list)))

(define (non-visited-neighbours coords visited neighbour-generator)
  (reject
    (neighbour-generator coords)
    (lambda (neighbour)
      (coverage-check visited neighbour))))