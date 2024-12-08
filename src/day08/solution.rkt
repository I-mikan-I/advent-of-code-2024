#lang racket
(require "../utils.rkt")
(define input (map string->list (file->lines (rpath "input.txt"))))

(define antennas
  (for/fold ([h (hash)])
            ([row input]
             [r (length input)]
             #:when #t
             [char row]
             [c (length row)]
             #:when (not (equal? char #\.)))
    (hash-update h char (lambda (s) (set-add s (cons r c))) set)))

(define (antenna->antinodes1 id locs)
  (map (match-lambda
         [(list a1 a2)
          #:when (equal? a1 a2)
          (cons -1 -1)]
         [(list (cons x y) (cons x2 y2)) (cons (+ x (- x x2)) (+ y (- y y2)))])
       (cartesian-product (set->list locs) (set->list locs))))
(define (antenna->antinodes2 id locs)
  (foldl append
         '()
         (map (match-lambda
                [(list a1 a2)
                 #:when (equal? a1 a2)
                 '()]
                [(list (cons x y) (cons x2 y2))
                 (let* ([dx (- x x2)]
                        [dy (- y y2)]
                        [k (gcd dx dy)]
                        [dx (quotient dx k)]
                        [dy (quotient dy k)])
                   (for/list ([k (in-naturals)])
                     (define x_ (+ x (* k dx)))
                     (define y_ (+ y (* k dy)))
                     #:break
                     (or (< x_ 0) (< y_ 0) (>= x_ (length input)) (>= y_ (length (car input))))
                     (cons x_ y_)))])
              (cartesian-product (set->list locs) (set->list locs)))))

(define (filter-antinodes antenna->antinodes)
  (list->set
   (filter (match-lambda
             [(cons r c) (and (>= r 0) (>= c 0) (< r (length input)) (< c (length (car input))))])
           (foldl append '() (hash-map antennas antenna->antinodes)))))

(set-count (filter-antinodes antenna->antinodes1))
(set-count (filter-antinodes antenna->antinodes2))
