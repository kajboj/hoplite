(define (dump-colors)
  (let ((hex-coords-and-colors-list
          (reject-empty-tiles (hex-coords-and-colors screen))))
    (displayn board-with-coords)
    (displayn hex-coords-and-colors-list)
    (displayn 
      (map (lambda (x) (list (car x) (cadr x)))
      (parse-pieces hex-coords-and-colors-list piece-defs)))))