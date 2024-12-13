#lang racket

(require racket/runtime-path)
(require (for-syntax racket/runtime-path))
(provide rpath
         gcd
         gcd-e)

; relative path
(define-syntax (rpath stx)
  (syntax-case stx ()
    [(_ str)
     (let* ([source (syntax-source #'str)]
            [base (let-values ([(base _1 _2) (split-path source)])
                    base)]
            [path (build-path base (syntax-e #'str))])
       (with-syntax ([final-path (datum->syntax #'str path)])
         #'final-path))]))

; u = q * v + r
; k * u' = q * v + r = q * (k * v') + r
; ==> gcd(v, r) = k
(define (gcd u v)
  (if (equal? v 0)
      u
      (gcd v (remainder u v))))

(define (gcd-e u v [l '(1 . 0)] [m '(0 . 1)])
  (if (equal? v 0)
      (values u (car l) (car m))
      (let-values ([(q r) (quotient/remainder u v)])
        (gcd-e v
               r
               (cons (cdr l) (- (car l) (* q (cdr l))))
               (cons (cdr m) (- (car m) (* q (cdr m))))))))
