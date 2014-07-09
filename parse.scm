(define (coords=? a b)
  (and (= (car a) (car b)) (= (cadr a) (cadr b))))

(define (col coords) (car coords))
(define (row coords) (cadr coords))

(define (red rgb) (car rgb))
(define (green rgb) (cadr rgb))
(define (blue rgb) (caddr rgb))

(define (parse-coords s)
  (list
    (parse-int (substring s 0 1))
    (parse-int (string-trim (substring s 1 3)))))

(define (parse-int s)
  (if (string=? s "A") 10 (string->number s)))

(define (traverse-board board f)
  (begin
    (define (inner chars col row acc)
      (if (null? chars)
        acc 
        (if (char=? (car chars) #\newline)
          (inner (cdr chars) 0 (+ 1 row) acc)
          (inner (cdr chars) (+ 1 col) row (f chars acc (list col row))))))
    (inner (string->list board) 0 0 (list))))

(define (ascii-coords-of-Xs board)
  (traverse-board board 
    (lambda (chars acc coords)
      (if (char=? (car chars) #\X)
        (cons coords acc)
        acc))))

(define (hex-coords ascii-coords)
  (traverse-board board-with-coords
    (lambda (chars acc coords)
      (if (coords=? coords ascii-coords)
        (parse-coords (list->string (sublist chars 0 3)))
        acc))))

(define (color screen ascii-coords)
  (list-ref
    (list-ref screen (- (row ascii-coords) 1))
    (col ascii-coords)))

(define (hex-coords-and-color screen)
  (map
    (lambda (ascii-coords)
      (list
        (hex-coords (coords-add ascii-coords (list -1 -1)))
        (color screen ascii-coords)))
    (ascii-coords-of-Xs board-for-piece-recognition)))

(define (number-within? tolerance x y)
  (<= (abs (- x y)) tolerance))

(define (color-within? tolerance c1 c2)
  (and
    (number-within? tolerance (red c1) (red c2))
    (number-within? tolerance (green c1) (green c2))
    (number-within? tolerance (blue c1) (blue c2))))

(define (empty-tile? color)
  (color-within? 5 color (list 66 66 66)))

(define (reject-empty-tiles hex-coords-and-colors)
  (list-transform-negative
    hex-coords-and-colors
    (lambda (color) (empty-tile? (cadr color)))))

(define (create-pieces piece-def hex-coords-and-colors)
  (map (get-creator piece-def)
    (map car
      (list-transform-positive
        hex-coords-and-colors
        (lambda (hex-coords-and-color)
          (color-within? 15
            (cadr hex-coords-and-color)
            (get-color piece-def)))))))

(define (parse-pieces screen piece-defs)
  (let ((hex-coords-and-colors
          (reject-empty-tiles (hex-coords-and-color screen))))
    (fold-left 
      (lambda (acc piece-def)
        (append
          acc
          (create-pieces
            piece-def
            hex-coords-and-colors)))
      (list)
      piece-defs)))

(define (parse-world screen piece-defs)
  (let ((pieces (parse-pieces screen piece-defs)))
    (game-world
      (car (list-transform-positive pieces hoplite?))
      (list-transform-negative pieces hoplite?))))