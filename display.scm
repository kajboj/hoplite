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

(define (render-piece piece board board-with-coords)
  (replace-first-in-target
    (render-coords (get-coords piece))
    (get-symbol piece)
    board-with-coords
    board))