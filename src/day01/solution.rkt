#lang racket
(require "../utils.rkt")

(define input-path (rpath "input.txt"))
(define input (file->lines input-path))

(define ls1
  (map (lambda (s) (string->number (car (string-split s " " #:trim? #t #:repeat? #t)))) input))
(define ls2
  (map (lambda (s) (string->number (cadr (string-split s " " #:trim? #t #:repeat? #t)))) input))

(define ls1_s (sort ls1 <))
(define ls2_s (sort ls2 <))

(define distances (for/sum ([n1 ls1_s] [n2 ls2_s]) (abs (- n1 n2))))

(define ls2_cnts
  (for/fold ([h (hash)]) ([n ls2])
    (hash-update h n (curry + 1) (lambda () 0))))

(println distances)
(println (apply + (map (lambda (n) (* n (hash-ref ls2_cnts n (lambda () 0)))) ls1)))