#lang racket
(require "../utils.rkt")
(require racket/hash)
(define input (file->string (rpath "input.txt")))

(define layout
  (list->vector (map (compose list->vector (curry map char->symbol) string->list)
                     (string-split input "\n"))))

(define adjs
  (hash-union (for*/hash ([r (in-range (vector-length layout))]
                          [c (in-range (vector-length (vector-ref layout r)))]
                          [dirs '((D . R) (D . L) (R . D) (R . U) (L . U) (L . D) (U . R) (U . L))])
                (values (cons (list r c (car dirs)) (list r c (cdr dirs))) 1000))
              (for/fold ([result (hash)])
                        ([r (in-range (vector-length layout))]
                         #:when #t
                         [c (in-range (vector-length (vector-ref layout r)))]
                         #:when #t
                         [offset '((1 . 0) (-1 . 0) (0 . 1) (0 . -1))]
                         [dir '(D U R L)])
                (let ([r_ (+ r (car offset))]
                      [c_ (+ c (cdr offset))]
                      [sym (vector-refs layout r c)])
                  (cond
                    [(or (< r_ 0)
                         (< c_ 0)
                         (>= r_ (vector-length layout))
                         (>= c_ (vector-length (vector-ref layout r))))
                     result]
                    [(not (or (equal? sym '|#|) (equal? (vector-refs layout r_ c_) '|#|)))
                     (hash-set result (cons (list r c dir) (list r_ c_ dir)) 1)]
                    [else result])))))

(define (adj-to adjs)
  (lambda (from)
    (define r (car from))
    (define c (cadr from))
    (for*/list ([offset '((1 . 0) (-1 . 0) (0 . 1) (0 . -1) (0 . 0))]
                [dir '(D U R L)]
                #:when
                (hash-has-key? adjs (cons from (list (+ r (car offset)) (+ c (cdr offset)) dir))))
      (define to (list (+ r (car offset)) (+ c (cdr offset)) dir))
      (cons to (hash-ref adjs (cons from to))))))

(let ([start (for*/or ([r (in-range (vector-length layout))]
                       [c (in-range (vector-length (vector-ref layout r)))])
               (and (equal? (vector-refs layout r c) 'S) (list r c 'R)))]
      [end (for*/or ([r (in-range (vector-length layout))]
                     [c (in-range (vector-length (vector-ref layout r)))])
             (and (equal? (vector-refs layout r c) 'E) (list r c)))])
  (define solved (bellman-ford (adj-to adjs) start))
  (println (bellman-ford (adj-to adjs) start #t))
  (foldl min
         99999999
         (map (compose cdr (curry hash-ref solved))
              (map (curry apply append) (cartesian-product (list end) '((R) (D) (L) (U)))))))
