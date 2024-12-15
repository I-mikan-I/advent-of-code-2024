#lang racket
(require "../utils.rkt")

(define input (file->string (rpath "input.txt")))

(define warehouse
  (list->vector (map (lambda (ln) (list->vector (string->list ln)))
                     (string-split (car (string-split input "\n\n")) "\n"))))
(define (start warehouse)
  (for/or ([r (in-range (vector-length warehouse))]
           #:when #t
           [c (in-range (vector-length (vector-ref warehouse r)))])
    (and (equal? #\@ (vector-refs warehouse r c)) (cons r c))))
(define movements
  (string->list (foldr string-append "" (string-split (cadr (string-split input "\n\n")) "\n"))))

(define (do-move warehouse r c ro co [dry-run? #f])
  (let* ([nr (+ r ro)]
         [nc (+ c co)]
         [chr (vector-refs warehouse nr nc)]
         [update! (if dry-run?
                      (lambda (w r c val) (void))
                      (lambda (w r c val) (vector-sets! w r c val)))])
    (match chr
      [#\# #f]
      [#\.
       (update! warehouse nr nc (vector-refs warehouse r c))
       (update! warehouse r c #\.)
       #t]
      [#\O
       (and (do-move warehouse nr nc ro co dry-run?)
            (begin
              (update! warehouse nr nc (vector-refs warehouse r c))
              (update! warehouse r c #\.)
              #t))]
      [(or #\[ #\])
       (let* ([lnc (if (equal? chr #\[)
                       nc
                       (- nc 1))]
              [rnc (if (equal? chr #\[)
                       (+ nc 1)
                       nc)])
         (and (or (equal? co 1) (do-move warehouse nr lnc ro co #t))
              (or (equal? co -1) (do-move warehouse nr rnc ro co #t))
              (or dry-run?
                  (begin
                    (if (equal? co 1)
                        (begin
                          (do-move warehouse nr rnc ro co dry-run?)
                          (do-move warehouse nr lnc ro co dry-run?))
                        (begin
                          (do-move warehouse nr lnc ro co dry-run?)
                          (do-move warehouse nr rnc ro co dry-run?)))
                    (update! warehouse nr nc (vector-refs warehouse r c))
                    (update! warehouse r c #\.)
                    #t))))])))

(define (apply-moves warehouse moves)
  (let ([warehouse (vector-map vector-copy warehouse)])
    (for/fold ([r (car (start warehouse))]
               [c (cdr (start warehouse))])
              ([move moves])
      (let-values ([(ro co) (match move
                              [#\v (values 1 0)]
                              [#\^ (values -1 0)]
                              [#\> (values 0 1)]
                              [#\< (values 0 -1)])])
        (if (do-move warehouse r c ro co)
            (values (+ r ro) (+ c co))
            (values r c))))
    warehouse))

(define (widen warehouse)
  (for/vector ([r warehouse])
    (for*/vector ([chr r]
                  [i (in-range 2)])
      (match chr
        [#\# #\#]
        [#\. #\.]
        [#\@ (if (equal? i 0) #\@ #\.)]
        [#\O (if (equal? i 0) #\[ #\])]))))

(for/sum ([row (apply-moves warehouse movements)] [r (in-naturals)]
                                                  #:when #t
                                                  [sym row]
                                                  [c (in-naturals)]
                                                  #:when (or (equal? sym #\[) (equal? sym #\O)))
         (+ (* 100 r) c))
(for/sum ([row (apply-moves (widen warehouse) movements)] [r (in-naturals)]
                                                          #:when #t
                                                          [sym row]
                                                          [c (in-naturals)]
                                                          #:when (or (equal? sym #\[)
                                                                     (equal? sym #\O)))
         (+ (* 100 r) c))
