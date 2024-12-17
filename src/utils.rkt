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

(define (bellman-ford adj-to start [merge (lambda (accum leq) leq)] [empty '()])
  (let rec ([result (hash start (cons (merge empty start) 0))]
            [queue (set start)])
    (define to-update
      (for*/list ([from queue]
                  [to (adj-to from)]
                  #:do [(define from-dist (cdr (hash-ref result from)))
                        (define edge-dist (cdr to))
                        (define to-node (car to))
                        (define to-dist
                          (and (hash-has-key? result to-node) (cdr (hash-ref result to-node))))
                        (define leq
                          (or (not (hash-has-key? result to-node))
                              (<= (+ edge-dist from-dist) to-dist)))]
                  #:when leq)
        (cons to-node (cons from (+ edge-dist from-dist)))))
    (define queue-next (list->set (map car to-update)))
    (define result-next
      (foldl (lambda (updt result)
               (match-define (cons to-node (cons from-node dist)) updt)
               (hash-update result
                            to-node
                            (lambda (last)
                              (cond
                                [(< dist (cdr last)) (cons (merge empty from-node) dist)]
                                [(<= dist (cdr last)) (cons (merge (car last) from-node) dist)]
                                [else last]))
                            (lambda () (cons (merge empty from-node) dist))))
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
