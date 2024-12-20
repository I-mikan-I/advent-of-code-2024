#lang racket
(require "../utils.rkt")
(define input (file->lines (rpath "input.txt")))

(define track (list->vector (map (compose list->vector string->list) input)))

(define start
  (for/or ([r (in-naturals)]
           [row track]
           #:when #t
           [c (in-naturals)]
           [sm row])
    (and (equal? sm #\S) (cons r c))))
(define end
  (for/or ([r (in-naturals)]
           [row track]
           #:when #t
           [c (in-naturals)]
           [sm row])
    (and (equal? sm #\E) (cons r c))))

(define (adj-to node)
  (define r (car node))
  (define c (cdr node))
  (define sym (vector-refs track r c))
  (if (eq? sym #\#)
      '()
      (for/list ([offset '((1 . 0) (-1 . 0) (0 . 1) (0 . -1))]
                 #:do [(define ro (car offset))
                       (define co (cdr offset))
                       (define nr (+ r ro))
                       (define nc (+ c co))
                       (define cond
                         (and (>= nr 0)
                              (>= nc 0)
                              (< nr (vector-length track))
                              (< nc (vector-length track))))
                       (define sym (and cond (vector-refs track nr nc)))]
                 #:when (and cond
                             (case sym
                               [(#\.) #t]
                               [(#\S) #t]
                               [(#\E) #t]
                               [else #f])))
        (cons nr nc))))

(define (bfs adj-to start)
  (let rec ([i 1]
            [dists (hash start 0)]
            [queue (list->set (adj-to start))])
    (if (set-empty? queue)
        dists
        (let* ([dists2 (foldl (lambda (adj dists) (hash-set dists adj i)) dists (set->list queue))]
               [queue2 (for/set ([from queue]
                                 #:when #t
                                 [to (adj-to from)]
                                 #:when (not (hash-has-key? dists2 to)))
                         to)])

          (rec (+ i 1) dists2 queue2)))))
(define nodes
  (for*/list ([r (in-range (vector-length track))]
              [c (in-range (vector-length track))])
    (cons r c)))
(define apsp
  (for/hash ([node nodes])
    (values node (bfs adj-to node))))
(define default-time (hash-ref (bfs adj-to start) end))
(define (count-cheats improv psecs)
  (for*/sum
   ([cheat-start nodes] [cheat-end nodes])
   (define cheat-dist
     (+ (abs (- (car cheat-start) (car cheat-end))) (abs (- (cdr cheat-start) (cdr cheat-end)))))
   (define legal? (<= cheat-dist psecs))
   (if legal?
       (let* ([cheating-time (+ (hash-ref (hash-ref apsp start) cheat-start (lambda () 10000))
                                (hash-ref (hash-ref apsp cheat-end) end (lambda () 10000))
                                cheat-dist)])
         (if (<= cheating-time (- default-time improv)) 1 0))
       0)))

(count-cheats 100 2)
(count-cheats 100 20)
