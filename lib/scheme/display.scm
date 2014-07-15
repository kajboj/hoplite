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

(define (render-piece piece board)
  (render-symbol
    (get-symbol piece)
    (get-coords piece)
    board))

(define (render-pieces pieces board)
  (if (null? pieces)
    board
    (render-piece
      (car pieces)
      (render-pieces (cdr pieces) board))))

(define (render-symbol symbol coords board)
  (replace-first-in-target
    (render-coords coords)
    symbol
    board-with-coords
    board))

(define (render-symbols symbol coords-list board)
  (if (null? coords-list)
    board
    (render-symbol
      symbol
      (car coords-list)
      (render-symbols symbol (cdr coords-list) board))))

(define (render-world world board)
  (render-pieces (get-pieces world)
    board))

(define (get-width dimensions) (car dimensions))
(define (get-height dimensions) (cadr dimensions))

(define (board-dimensions board)
  (traverse-board board (list 0 0)
    (lambda (chars acc coords)
      (list
        (max (get-x acc) (get-x coords))
        (max (get-y acc) (get-y coords))))))

(define (ascii-coords-to-proportions ascii-coords)
  (let* ((dim (board-dimensions empty-board))
         (width (car dim))
         (height (cadr dim)))
  (list
    (/ (car ascii-coords) width)
    (/ (cadr ascii-coords) height))))