(define (displayn s) (begin (display s) (newline)))

(define (replace-first pattern new s)
  (replace-first-in-target pattern new s s))

(define (replace-first-in-target pattern new source target)
  (let* 
    ((start-index (string-search-forward pattern source))
     (end-index (+ start-index (string-length pattern))))
    (string-append
      (substring target 0 start-index)
      new
      (substring target end-index (string-length target)))))

(define (pairs imin imax jmin jmax)
  (begin
    (define (inner-pairs i j)
      (if (> i imax)
        ()
        (if (> j jmax)
          (inner-pairs (+ i 1) jmin)
          (cons (list i j) (inner-pairs i (+ j 1))))))
    (inner-pairs imin jmin)))

(define (list-sample x)
  (list-ref x (random (length x))))

(define (list-min f lst)
  (if (null? lst)
    '()
    (fold-left
      (lambda (acc e)
        (if (< (f e) (f acc)) e acc))
      (car lst)
      (cdr lst))))

(define (all-min f lst)
  (let ((min (f (list-min f lst))))
    (filter
      (lambda (x) (= (f x) min))
      lst)))

(define (function-or functions arguments)
  (fold-left
    (lambda (acc f) (or acc (apply f arguments)))
    #f
    functions))

(define reject list-transform-negative)
(define select list-transform-positive)