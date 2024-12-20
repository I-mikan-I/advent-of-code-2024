#lang racket

(require racket/runtime-path)
(require (for-syntax racket/runtime-path))
(provide rpath
         gcd
         vector-refs
         vector-sets!
         gcd-e
         char->symbol
         floyd-warshall
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

(define (floyd-warshall adj-to nodes [max 10000])
  (define len (length nodes))
  (define ids
    (for/hash ([node nodes]
               [i (in-naturals)])
      (values node i)))
  (define dists
    (for/vector ([node1 nodes])
      (for/vector ([node2 nodes])
        (if (eq? node1 node2) 0 max))))
  (for* ([node nodes]
         [adj (adj-to node)])
    (match-define (cons node2 w) adj)
    (vector-sets! dists
                  (hash-ref ids node)
                  (hash-ref ids node2)
                  (min w (vector-refs dists (hash-ref ids node) (hash-ref ids node2)))))
  (for* ([k (in-range len)]
         #:do [(println k)]
         [i (in-range len)]
         [j (in-range len)])

    (define new-dist (+ (vector-refs dists i k) (vector-refs dists k j)))
    (if (> (vector-refs dists i j) new-dist)
        (vector-sets! dists i j new-dist)
        (void)))
  (for/hash ([from nodes]
             [i (in-naturals)]
             #:when #t
             [to nodes]
             [k (in-naturals)])
    (values (cons from to) (vector-refs dists i k))))

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
