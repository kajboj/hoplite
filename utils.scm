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
