#lang racket
(require "../utils.rkt")
(define input (file->string (rpath "input.txt")))
(struct robot (r c vr vc) #:transparent)
(define robots
  (map (match-lambda
         [(pregexp #px"p=(\\d+),(\\d+) v=(-?\\d+),(-?\\d+)" (list _ c r vc vr))
          (apply robot (map string->number (list r c vr vc)))])
       (string-split input "\n")))
(define cols 101)
(define rows 103)

(define (move seconds)
  (map (match-lambda
         [(robot r c vr vc)
          (let* ([r (+ r (* seconds vr))]
                 [c (+ c (* seconds vc))]
                 [r (modulo r rows)]
                 [c (modulo c cols)])
            (robot r c vr vc))])
       robots))

(define (quadrants moved)
  (for/fold ([ul 0]
             [ur 0]
             [ll 0]
             [lr 0]
             #:result (list ul ur ll lr))
            ([r_ moved])
    (match-define (robot r c _ _) r_)
    (cond
      [(and (< r (quotient rows 2)) (< c (quotient cols 2))) (values (+ ul 1) ur ll lr)]
      [(and (< r (quotient rows 2)) (> c (quotient cols 2))) (values ul (+ ur 1) ll lr)]
      [(and (> r (quotient rows 2)) (< c (quotient cols 2))) (values ul ur (+ ll 1) lr)]
      [(and (> r (quotient rows 2)) (> c (quotient cols 2))) (values ul ur ll (+ lr 1))]
      [else (values ul ur ll lr)])))

(apply * (quadrants (move 100)))
(let*-values ([(a b l1 l2)
               (for/fold ([cycle1 #f]
                          [cycle2 #f]
                          [last1 0]
                          [last2 0])
                         ([seconds (in-range 0 200)])
                 (let* ([moved (move seconds)]
                        [quads (quadrants moved)])
                   (match-define (list ul ur ll lr) quads)
                   (cond
                     [(> (+ ul ur) (+ ll lr 70)) (values (- seconds last1) cycle2 seconds last2)]
                     [(> (+ lr ur) (+ ll ul 70)) (values cycle1 (- seconds last2) last1 seconds)]
                     [else (values cycle1 cycle2 last1 last2)])))]
              [(b) (- b)]
              [(l1) (modulo l1 a)]
              [(l2) (modulo l2 b)]
              [(r x y) (gcd-e a b)]
              [(q) (quotient (- l2 l1) r)]
              [(u) (quotient a r)]
              [(v) (quotient b r)]
              [(x) (* x q)]
              [(y) (* y q)]
              [(k) (let ([k1 (ceiling (/ (- x) v))]
                         [k2 (ceiling (/ y u))])
                     (if (> k1 k2) k1 k2))]
              [(x) (+ x (* k v))])
  (+ l1 (* a x)))
