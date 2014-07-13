(define (path start finish neighbour-generator)
  (let ((fringes (flood-fill
                   finish
                   (list start)
                   (list (list start))
                   100 0
                   neighbour-generator)))
    (build-path (list finish) (cdr fringes))))
    

(define (build-path path fringes)
  (if (null? fringes)
      path
      (let ((path-elem 
              (list-search-positive
                (car fringes)
                (lambda (coords) (coverage-check (neighbours 1 (car path)) coords)))
              ))
        (build-path (cons path-elem path) (cdr fringes)))))

(define (flood-fill target visited fringes max-depth depth neighbour-generator)
  (if (= max-depth depth)
      '()
      (if (coverage-check (car fringes) target)
          fringes
          (let ((new-fringe (all-non-visited-neighbours
                              (car fringes)
                              visited
                              neighbour-generator)))
            (flood-fill
              target
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