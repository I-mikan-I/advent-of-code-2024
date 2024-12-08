#lang racket

(require racket/runtime-path)
(require (for-syntax racket/runtime-path))
(provide rpath)

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
