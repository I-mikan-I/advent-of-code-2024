#lang racket
(require "../utils.rkt")
(define input (file->string (rpath "input.txt")))

(define plots (list->vector (map (compose list->vector string->list) (string-split input "\n"))))

(define (update plots regions area perim r c)
  (let* ([sym (vector-ref (vector-ref plots r) c)]
         [region (hash-ref regions (cons r c) (lambda () #f))]

         [neighbors (map (lambda (coords)
                           (let ([r (+ r (car coords))]
                                 [c (+ c (cdr coords))])
                             (and (>= r 0)
                                  (>= c 0)
                                  (< r (vector-length plots))
                                  (< c (vector-length plots))
                                  (equal? sym (vector-ref (vector-ref plots r) c))
                                  (cons r c))))
                         '((0 . 1) (0 . -1) (1 . 0) (-1 . 0)))]
         [bfs (filter (lambda (coords) (and coords (not (hash-has-key? regions coords)))) neighbors)]
         [current-area (if region
                           (hash-ref area region)
                           0)]
         [current-perim (if region
                            (hash-ref perim region)
                            (set))]
         [next-area (+ 1 current-area)]
         [next-perim (set-union current-perim
                                (for/set ([n neighbors]
                                          [dir '(r l d u)]
                                          #:when (not n))
                                  (list* dir r c)))]
         [region (if region
                     region
                     (cons r c))]
         [regions (foldl (lambda (coords regions)
                           (if coords
                               (hash-set regions coords region)
                               regions))
                         regions
                         neighbors)]
         [regions (hash-set regions (cons r c) region)]
         [area (hash-set area region next-area)]
         [perim (hash-set perim region next-perim)])
    (values regions area perim bfs)))

(define (sum1 perim)
  (for/hash ([(region perim) perim])
    (values region (set-count perim))))

(define (sum2 perim)
  (for/hash ([(region perim) perim])
    (values region
            (for/fold ([res 0]
                       [ignored (set)]
                       #:result res)
                      ([p perim]
                       #:when (not (set-member? ignored p)))
              (match p
                [(list* (and dir (or 'u 'd)) r c)
                 (values (+ res 1)
                         (set-union (for/set ([i (in-naturals)])
                                      #:break (not (set-member? perim (list* dir r (+ c i))))
                                      (list* dir r (+ c i)))
                                    (for/set ([i (sequence-map - (in-naturals))])
                                      #:break (not (set-member? perim (list* dir r (+ c i))))
                                      (list* dir r (+ c i)))
                                    ignored))]

                [(list* (and dir (or 'l 'r)) r c)
                 (values (+ res 1)
                         (set-union (for/set ([i (in-naturals)])
                                      #:break (not (set-member? perim (list* dir (+ r i) c)))
                                      (list* dir (+ r i) c))
                                    (for/set ([i (sequence-map - (in-naturals))])
                                      #:break (not (set-member? perim (list* dir (+ r i) c)))
                                      (list* dir (+ r i) c))
                                    ignored))])))))

(define (solve fn)
  (let rec ([regions (hash)]
            [area (hash)]
            [perim (hash)]
            [visited (set)]
            [bfs (map (curry apply cons)
                      (cartesian-product (range (vector-length plots))
                                         (range (vector-length plots))))])
    (if (null? bfs)
        (let ([cleaned (fn perim)])
          (for/sum ([region (list->set (hash-values regions))])
                   (* (hash-ref area region) (hash-ref cleaned region))))
        (let-values ([(h t) (values (car bfs) (cdr bfs))])
          (if (set-member? visited h)
              (rec regions area perim visited t)
              (let*-values ([(regions area perim bfs2)
                             (update plots regions area perim (car h) (cdr h))])
                (rec regions area perim (set-add visited h) (append bfs2 t))))))))

(solve sum1)
(solve sum2)
