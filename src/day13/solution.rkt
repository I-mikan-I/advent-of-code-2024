#lang racket
(require "../utils.rkt")

(define input (string-trim (file->string (rpath "input.txt"))))
(struct machine-t (xa ya xb yb goal-x goal-y) #:transparent)
(define configs (string-split input "\n\n"))
(define (parse machine)
  (match machine
    [(pregexp
      #px"Button A: X\\+(\\d+), Y\\+(\\d+).*Button B: X\\+(\\d+), Y\\+(\\d+).*Prize: X=(\\d+), Y=(\\d+)"
      (list _ xa ya xb yb xg yg))
     (apply machine-t (map string->number (list xa ya xb yb xg yg)))]))

(define machines (map parse configs))

; (xa * k1 + xb * t1 = X)
; (ya * k2 + yb * t2 = Y)
; (xa * (k1 + x1v1) + xb (t1 - x1u1)) = X
; (ya * (k2 + x2v2) + yb (t2 - x2u2)) = Y
; k1 + x1v1 = k2 + x2v2 ; x1v1 = k2 + x2v2 - k1 ; x1 = (k2 + x2v2 - k1) / v1 (= eq1)
; t1 - x1u1 = t2 - x2u2 ; u1(k2 + x2v2 - k1)/v1 - x2u2 = t1 - t2 ; x2(v2u1 - u2v1)/v1 = t1 - t2 - u1k2/v1 + u1k1/v1
; x2 = (t1v1 - t2v1 - u1k2 + u1k1)/(v2u1 - u2v1)
; min 3(k1 + x1v1) + t1 - x1u1
(define (solutions machines)
  (filter-map (lambda (m)
                (let*-values ([(r1 k1 t1) (gcd-e (machine-t-xa m) (machine-t-xb m))]
                              [(r2 k2 t2) (gcd-e (machine-t-ya m) (machine-t-yb m))]
                              [(u1 v1 u2 v2) (values (quotient (machine-t-xa m) r1)
                                                     (quotient (machine-t-xb m) r1)
                                                     (quotient (machine-t-ya m) r2)
                                                     (quotient (machine-t-yb m) r2))]
                              [(times1) (/ (machine-t-goal-x m) r1)]
                              [(times2) (/ (machine-t-goal-y m) r2)]
                              [(k1) (values (* times1 k1))]
                              [(t1) (values (* times1 t1))]
                              [(k2) (values (* times2 k2))]
                              [(t2) (values (* times2 t2))]
                              [(x2) (/ (- (* t1 v1) (* t2 v1) (* u1 k2) (- (* u1 k1)))
                                       (- (* v2 u1) (* u2 v1)))]
                              [(x1) (/ (+ k2 (* x2 v2) (- k1)) v1)])
                  (if (and (integer? x1) (integer? x2) (integer? times1) (integer? times2))
                      (cons (+ k1 (* x1 v1)) (- t1 (* x1 u1)))
                      #f)))
              machines))
(for/sum ([s (solutions machines)]) (+ (* 3 (car s)) (cdr s)))
(for/sum ([s
           (solutions (map (lambda (m)
                             (struct-copy machine-t
                                          m
                                          [goal-x (+ 10000000000000 (machine-t-goal-x m))]
                                          [goal-y (+ 10000000000000 (machine-t-goal-y m))]))
                           machines))])
         (+ (* 3 (car s)) (cdr s)))
