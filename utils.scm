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