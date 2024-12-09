#lang racket
(require "../utils.rkt")
(define input (file->string (rpath "input.txt")))

(define disk
  (vector->immutable-vector (list->vector (map (compose string->number ~a) (string->list input)))))
(define accum
  (vector->immutable-vector (list->vector (reverse (for/fold ([sum 0]
                                                              [ls '()]
                                                              #:result ls)
                                                             ([id disk])
                                                     (values (+ sum id) (cons sum ls)))))))

(define (get-pos lptr lvar)
  (+ (vector-ref accum lptr) (- (vector-ref disk lptr) lvar)))

(define ids (quotient (vector-length disk) 2))
(define lptr 0)
(define rptr (* ids 2))

(define (next-id disk lptr rptr)
  (define lvar (vector-ref disk lptr))
  (define rvar (vector-ref disk rptr))
  (cond
    [(> lptr rptr) #f]
    [(equal? lvar 0)
     (begin
       (next-id disk (+ lptr 1) rptr))]
    [(even? lptr)
     (begin
       (vector-set! disk lptr (- lvar 1))
       (quotient lptr 2))]
    [(equal? rvar 0)
     (begin
       (next-id disk lptr (- rptr 2)))]
    [else
     (begin
       (vector-set*! disk rptr (- rvar 1) lptr (- lvar 1))
       (quotient rptr 2))]))

(define (next-id2 disk lptr rptr)
  (if (>= lptr (vector-length disk))
      #f
      (begin
        (let* ([lvar (vector-ref disk lptr)]
               [rvar (vector-ref disk rptr)]
               [pos (get-pos lptr lvar)])
          (cond
            [(equal? lvar 0)
             (begin
               (next-id2 disk (+ lptr 1) (* ids 2)))]
            [(even? lptr)
             (begin
               (vector-set! disk lptr (- lvar 1))
               (* pos (quotient lptr 2)))]
            [(<= rptr lptr)
             (begin
               (next-id2 disk (+ lptr 1) (* ids 2)))]
            [(or (> rvar lvar) (equal? rvar 0))
             (begin
               (next-id2 disk lptr (- rptr 2)))]
            [else
             (begin
               (vector-set*! disk rptr (- rvar 1) lptr (- lvar 1))
               (* pos (quotient rptr 2)))])))))

(define (disk-nums fn disk)
  (define next (fn disk lptr rptr))
  (if next
      (stream-cons next (disk-nums fn disk))
      empty-stream))

(for/sum ([id (disk-nums next-id (vector-copy disk))] [pos (in-naturals)]) (* pos id))
(for/sum ([id (disk-nums next-id2 (vector-copy disk))]) id)
