#lang racket

(require racket/runtime-path)
(require (for-syntax racket/runtime-path))
(provide rpath
         gcd
         vector-refs
         vector-sets!
         gcd-e
         char->symbol
         bellman-ford)

(define (bellman-ford adj-to start [collect? #f])
  (define cmp (if collect? <= <))
  (define wrap (if collect? set identity))
  (define merge
    (if collect?
        (lambda (last next)
          (cond
            [(not (cmp next (set-first last))) last]
            [(equal? (cdr (set-first last)) (cdr next)) (set-add last next)]
            [else (wrap next)]))
        (lambda (last next) (if (cmp (cdr next) (cdr last)) next last))))
  (let rec ([result (hash start (cons start 0))]
            [queue (set start)])
    (define to-update
      (for*/list ([from queue]
                  [to (adj-to from)]
                  #:when (or (not (hash-has-key? result (car to)))
                             (cmp (+ (cdr to) (cdr (hash-ref result from)))
                                  (cdr (hash-ref result (car to))))))
        (cons (car to) (cons from (+ (cdr to) (cdr (hash-ref result from)))))))
    (define queue-next (list->set (map car to-update)))
    (define result-next
      (foldl (lambda (updt result)
               (hash-update result
                            (car updt)
                            (lambda (last) (merge last (cdr updt)))
                            (lambda () (cdr updt))))
             result
             to-update))
    (if (set-empty? queue-next)
        result-next
        (rec result-next queue-next))))

(define (char->symbol chr)
  (string->symbol (string chr)))

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

(define-syntax (vector-refs stx)
  (syntax-case stx ()
    [(_ v idx) #'(vector-ref v idx)]
    [(_ v idxs ...)
     (let* ([idxs (syntax-e #'(idxs ...))])
       (foldl (lambda (idx v)
                (with-syntax ([v v]
                              [idx idx])
                  #'(vector-ref v idx)))
              #'v
              idxs))]))

(define-syntax (vector-sets! stx)
  (syntax-case stx ()
    [(_ v idx val) #'(vector-set! v idx val)]
    [(_ v idxs ... last val) #'(vector-set! (vector-refs v idxs ...) last val)]))

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
