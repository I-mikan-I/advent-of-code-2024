#lang racket
(require "../utils.rkt")
(define input (file->string (rpath "input.txt")))
(define paths
  (list->vector (map (lambda (ls)
                       (list->vector (map (lambda (c) (string->number (~a c))) (string->list ls))))
                     (string-split input "\n"))))
(define (score r c paths paths?)
  (if (not (equal? (vector-ref (vector-ref paths r) c) 0))
      0
      (let-values ([(_ sum) (let rec ([visited (hash)]
                                      [r r]
                                      [c c])
                              (cond
                                [(hash-has-key? visited (cons r c))
                                 (values visited
                                         (if paths?
                                             (hash-ref visited (cons r c))
                                             0))]
                                [(equal? (vector-ref (vector-ref paths r) c) 9)
                                 (values (hash-set visited (cons r c) 1) 1)]
                                [else
                                 (for/fold ([visited (hash-set visited (cons r c) 0)]
                                            [sum 0]
                                            #:result (values (hash-set visited (cons r c) sum) sum))
                                           ([offset '((-1 . 0) (1 . 0) (0 . -1) (0 . 1))])
                                   (let ([r_ (+ r (car offset))]
                                         [c_ (+ c (cdr offset))])
                                     (if (and (>= r_ 0)
                                              (>= c_ 0)
                                              (< r_ (vector-length paths))
                                              (< c_ (vector-length paths))
                                              (equal? 1
                                                      (- (vector-ref (vector-ref paths r_) c_)
                                                         (vector-ref (vector-ref paths r) c))))
                                         (let-values ([(visited df) (rec visited r_ c_)])
                                           (values visited (+ sum df)))
                                         (values visited sum))))]))])
        sum)))

(for*/sum ([r (vector-length paths)] [c (vector-length paths)]) (score r c paths #f))
(for*/sum ([r (vector-length paths)] [c (vector-length paths)]) (score r c paths #t))
