#lang racket
(require "../utils.rkt")
(define input (file->string (rpath "input.txt")))

(match-define (list rules updates_) (string-split input "\n\n"))

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
  (for/fold ([h (hash)])
            ([ordering orderings]
             #:when (let ([from (car ordering)]
                          [to (cdr ordering)])
                      (and (set-member? table from) (set-member? table to))))
    (hash-update h
                 (car ordering)
                 (lambda (v) (set-add v (cdr ordering)))
                 (lambda () (set (cdr ordering))))))

(define (hull relation)
  (let rec ([prev (hash)]
            [next relation])
    (if (equal? prev next)
        next
        (let ([next_
               (make-immutable-hash
                (map (match-lambda
                       [(cons k v)
                        (cons k
                              (apply set-union v (set-map v (lambda (to) (hash-ref next to set)))))])
                     (hash->list next)))])
          (rec next next_)))))

(define (check relation update)
  (let rec ([head (car update)]
            [tail (cdr update)])
    (cond
      [(empty? tail) #t]
      [(andmap (lambda (to) (not (set-member? (hash-ref relation to set) head))) tail)
       (rec (car tail) (cdr tail))]
      [else #f])))

(for/sum ([update updates])
         (if (check (hull (activate update orderings)) update)
             (list-ref update (quotient (length update) 2))
             0))

(define (cmp-update relation lhs rhs)
  (set-member? (hash-ref relation lhs set) rhs))

(for/sum ([update updates])
         (let ([relation (hull (activate update orderings))])
           (if (check relation update)
               0
               (list-ref (sort update (curry cmp-update relation)) (quotient (length update) 2)))))
