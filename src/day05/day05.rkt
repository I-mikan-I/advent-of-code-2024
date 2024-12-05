#lang racket
(require "../utils.rkt")
(define input (file->string (rpath "input.txt")))

(match-define (list rules updates_) (string-split input "\n\n"))

rules
(define updates
  (map (lambda (l) (map string->number (string-split l ","))) (string-split updates_ "\n")))
(define orderings
  (map (lambda (rule)
         (match rule
           [(pregexp #px"(\\d+)\\|(\\d+)" (list _ n1 n2))
            (cons (string->number n1) (string->number n2))]))
       (string-split rules "\n")))

(define (activate ls orderings)
  (define table (list->set ls))
  (println table)
  (for/fold ([h (hash)])
            ([ordering orderings]
             #:when (let ([from (car ordering)]
                          [to (cdr ordering)])
                      (and (set-member? table from) (set-member? table to))))
    (hash-update h
                 (car ordering)
                 (lambda (v) (cons (cdr ordering) v))
                 (lambda () (list (cdr ordering))))))

(define (hull relation)
  (let rec ([prev (hash)]
            [next relation])
    (if (eq? prev next)
        next
        (let ([next_ (map (match-lambda
                            [(cons k v) (apply append v (map (lambda (to) (hash-ref next to (lambda () '()))) v))])
                          (hash->list next))])
          (rec next next_)))))

orderings
updates
(hull (activate (car updates) orderings))