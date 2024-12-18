#lang racket
(require "../utils.rkt")

(define input (file->lines (rpath "input.txt")))

(define bytepos
  (map (lambda (l) (apply cons (reverse (map string->number (string-split l ","))))) input))

(define blocked
  (for/hash ([i (in-naturals)]
             [elem bytepos])
    (values elem i)))

(define start (cons 0 0))
(define end (cons 70 70))

(define (adj-to after node)
  (for/list ([offset '((1 . 0) (-1 . 0) (0 . 1) (0 . -1))]
             #:do [(define r (+ (car offset) (car node)))
                   (define c (+ (cdr offset) (cdr node)))
                   (define newpos (cons r c))]
             #:when (and (>= r 0)
                         (>= c 0)
                         (<= r (car end))
                         (<= c (cdr end))
                         (not (<= (hash-ref blocked newpos (lambda () (+ after 1))) after))))
    (cons (cons r c) 1)))

(cdr (hash-ref (bellman-ford (curry adj-to 1024) start) end))
(displayln (for/or ([i (in-naturals 1024)])
             (define sol (bellman-ford (curry adj-to i) start))
             (if (not (hash-has-key? sol end))
                 (format "~a,~a" (cdr (list-ref bytepos i)) (car (list-ref bytepos i)))
                 #f)))
