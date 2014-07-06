(define (coords=? a b)
  (and (= (car a) (car b)) (= (cadr a) (cadr b))))

(define (col coords) (car coords))
(define (row coords) (cadr coords))

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