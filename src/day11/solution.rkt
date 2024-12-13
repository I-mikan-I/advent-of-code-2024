#lang racket
(require "../utils.rkt")
(define input (file->string (rpath "input.txt")))
(define initstones (map string->number (string-split input " ")))

(define stones
  (for/fold ([h (hash)]) ([stone initstones])
    (hash-update h stone (curry + 1) 0)))

(define (blink stones)
  (for/fold ([h (hash)]) ([(stone num) stones])
    (let* ([logt (and (> stone 0) (+ 1 (truncate (log stone 10))))]
           [next (match stone
                   [0 (list 1)]
                   [_
                    #:when (and (> logt 0) (even? logt))
                    (list (quotient stone (expt 10 (quotient logt 2)))
                          (remainder stone (expt 10 (quotient logt 2))))]
                   [_ (list (* stone 2024))])])
      (foldl (lambda (stone h) (hash-update h stone (curry + num) 0)) h (map inexact->exact next)))))

(define final
  (for/fold ([stones stones]) ([_ (in-range 25)])
    (blink stones)))
(define final2
  (for/fold ([stones stones]) ([_ (in-range 75)])
    (blink stones)))

(for/sum ([(_ num) final]) num)
(for/sum ([(_ num) final2]) num)
