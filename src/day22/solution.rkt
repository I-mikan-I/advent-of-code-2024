#lang racket
(require "../utils.rkt")

(define k 16777216)
(define (step n)
  (let* ([M (modulo (bitwise-xor (arithmetic-shift n 6) n) k)]
         [M (modulo (bitwise-xor (arithmetic-shift M -5) M) k)]
         [M (modulo (bitwise-xor (arithmetic-shift M 11) M) k)])
    M))

(define secrets (map string->number (file->lines (rpath "input.txt"))))

(define (get-secrets initial)
  (for/fold ([n initial]
             [ls (list initial)]
             #:result ls)
            ([_ (in-range 2000)])
    (let ([next (step n)]) (values next (cons next ls)))))

(define (get-prices initial)
  (map (lambda (n) (remainder n 10)) (get-secrets initial)))

(apply + (map (lambda (initial) (car (get-secrets initial))) secrets))

(define (get-diffs initial)
  (let ([prices (reverse (get-prices initial))])
    (for/list ([p1 prices]
               [p2 (cdr prices)])
      (- p2 p1))))

(define (pattern-map initial)
  (let ([diffs (get-diffs initial)]
        [prices (reverse (get-prices initial))])
    (for/fold ([h (hash)])
              ([d1 diffs]
               [d2 (cdr diffs)]
               [d3 (cddr diffs)]
               [d4 (cdddr diffs)]
               [p (cddddr prices)])
      (let ([key (list d1 d2 d3 d4)])
        (if (hash-has-key? h key)
            h
            (hash-set h key p))))))

(define (patterns initial)
  (let ([diffs (get-diffs initial)])
    (for/list ([d1 diffs]
               [d2 (cdr diffs)]
               [d3 (cddr diffs)]
               [d4 (cdddr diffs)])
      (list d1 d2 d3 d4))))

(define secrets-pattern-maps (map pattern-map secrets))
(define all-patterns (apply set-union (map (compose list->set patterns) secrets)))

(define (pattern-benefit pttrn)
  (apply + (map (lambda (pttrn-map) (hash-ref pttrn-map pttrn (lambda () 0))) secrets-pattern-maps)))

(for/fold ([m 0]) ([pattern all-patterns])
  (let ([benefit (pattern-benefit pattern)]) (max m benefit)))
