#lang racket
(require "../utils.rkt")

(define input (file->lines (rpath "input.txt")))
(define levels (map (lambda (line) (map string->number (string-split line " "))) input))

(define min-diff
  (lambda (level)
    (for/fold ([diff 1000]
               [p -100]
               #:result diff)
              ([l level])
      (values (min (abs (- p l)) diff) l))))
(define max-diff
  (lambda (level)
    (for/fold ([diff -1000]
               [p (car level)]
               #:result diff)
              ([l level])
      (values (max (abs (- p l)) diff) l))))

(define (ascending ls)
  (for/and ([n1 ls]
            [n2 (cdr ls)])
    (< n1 n2)))
(define (descending ls)
  (for/and ([n1 ls]
            [n2 (cdr ls)])
    (> n1 n2)))

(define (safe-1 level)
  (and (>= (min-diff level) 1) (<= (max-diff level) 3) (or (ascending level) (descending level))))

(apply + (map (lambda (level) (if (safe-1 level) 1 0)) levels))

(define (drop-any ls)
  (match ls
    ['() '()]
    [(cons h t) (append (map (curry cons h) (drop-any t)) (list t))]))

(apply + (map (lambda (level) (if (ormap safe-1 (drop-any level)) 1 0)) levels))
