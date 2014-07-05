(define board-template "
                 ___
             ___/0 0\\___
         ___/0 1\\___/1-1\\___
     ___/0 2\\___/1 0\\___/2-2\\___
 ___/0 3\\___/1 1\\___/2-1\\___/3-3\\___
/0 4\\___/1 2\\___/2 0\\___/3-2\\___/4-4\\
\\___/1 3\\___/2 1\\___/3-1\\___/4-3\\___/
/1 4\\___/2 2\\___/3 0\\___/4-2\\___/5-4\\
\\___/2 3\\___/3 1\\___/4-1\\___/5-3\\___/
/2 4\\___/3 2\\___/4 0\\___/5-2\\___/6-4\\
\\___/3 3\\___/4 1\\___/5-1\\___/6-3\\___/
/3 4\\___/4 2\\___/5 0\\___/6-2\\___/7-4\\
\\___/4 3\\___/5 1\\___/6-1\\___/7-3\\___/
/4 4\\___/5 2\\___/6 0\\___/7-2\\___/8-4\\
\\___/5 3\\___/6 1\\___/7-1\\___/8-3\\___/
/5 4\\___/6 2\\___/7 0\\___/8-2\\___/9-4\\
\\___/6 3\\___/7 1\\___/8-1\\___/9-3\\___/
/6 4\\___/7 2\\___/8 0\\___/9-2\\___/A-4\\
\\___/7 3\\___/8 1\\___/9-1\\___/A-3\\___/
    \\___/8 2\\___/9 0\\___/A-2\\___/
        \\___/9 1\\___/A-1\\___/
            \\___/A 0\\___/
                \\___/")

(define (displayn s) (begin (display s) (newline)))

(define (replace-first pattern new s)
  (let* 
    ((start-index (string-search-forward pattern s))
     (end-index (+ start-index (string-length pattern))))
    (string-append
      (substring s 0 start-index)
      new
      (substring s end-index (string-length s)))))

(define (render-int i)
  (if (= i 10) "A" (number->string i)))

(define (render-padded-int i)
  (if (< i 0) 
    (render-int i)
    (string-append " " (render-int i))))

(define (render-coords coords)
  (string-append
    (render-int (get-x coords))
    (render-padded-int (get-y coords))))

(define (get-x coords) (car coords))
(define (get-y coords) (cadr coords))

(define player '(5 -2))

(displayn (render-coords '(3 -3)))
(displayn (render-coords '(10 -1)))

(displayn (replace-first "3 0" "XXX" board-template))

; (displayn (render-coords '(5 -2)))
; (displayn (render-coords '(0 10)))

; (displayn board-template)