#lang racket
(require "../utils.rkt")
(define input (file->lines (rpath "input.txt")))

(define eqs
  (map (match-lambda
         [(list lhs operands)
          (cons (string->number lhs) (map string->number (string-split operands " ")))])
       (map (lambda (line) (string-split line ":")) input)))

(define (results-set operands max operators)
  (let rec ([operands (reverse operands)])
    (match operands
      [(cons h t)
       (let* ([intermediates (rec t)]
              [step (for*/set ([v intermediates]
                               [op operators])
                      (let ([res (op v h)]) (if (>= max res) res 0)))])
         step)]
      ['() (set 0)])))

(for/sum ([equation eqs] #:when (set-member? (results-set (cdr equation) (car equation) (list + *))
                                             (car equation)))
         (car equation))

(define (concat n1 n2)
  (string->number (string-append (number->string n1) (number->string n2))))

(for/sum ([equation eqs] #:when
                         (set-member? (results-set (cdr equation) (car equation) (list + * concat))
                                      (car equation)))
         (car equation))
