#lang racket
(require "../utils.rkt")

(define input (map string->list (file->lines (rpath "input.txt"))))

(define vertical
  (for/list ([row input])
    (list->string row)))
(define horizontal
  (for/list ([column (in-range (length (car input)))])
    (list->string (map (lambda (row) (list-ref row column)) input))))
(define diag1
  (for*/list ([row (in-range (length input))]
              [column (in-range (length input))]
              #:when (or (eq? row 0) (eq? column 0)))
    (list->string (for/list ([row_ (in-range row (length input))]
                             [column_ (in-range column (length input))])
                    (list-ref (list-ref input row_) column_)))))
(define diag2
  (for*/list ([row (in-range (length input))]
              [column (in-range (length input))]
              #:when (or (eq? row 0) (eq? column (- (length input) 1))))
    (list->string (for/list ([row_ (in-range row (length input))]
                             [column_ (in-inclusive-range column 0 -1)])
                    (list-ref (list-ref input row_) column_)))))

(define X
  (for*/list ([row (in-range (length input))]
              [column (in-range (length input))]
              #:when (and (>= (- row 1) 0)
                          (>= (- column 1) 0)
                          (< (+ row 1) (length input))
                          (< (+ column 1) (length input))))
    (let ([fn (lambda (p) (list-ref (list-ref input (+ row (car p))) (+ column (cdr p))))])
      (cons (list->string (map fn '((-1 . -1) (0 . 0) (1 . 1))))
            (list->string (map fn '((-1 . 1) (0 . 0) (1 . -1))))))))

(define (count lst)
  (apply +
         (map (lambda (str) (length (regexp-match* #px"XMAS" str)))
              (append lst (map (compose list->string reverse string->list) lst)))))

(+ (count vertical) (count horizontal) (count diag1) (count diag2))
(foldl +
       0
       (map (lambda (X)
              (match X
                [(cons (or "MAS" "SAM") (or "MAS" "SAM")) 1]
                [_ 0]))
            X))
