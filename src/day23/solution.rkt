#lang racket

(require "../utils.rkt")
(define input (file->lines (rpath "input.txt")))

(define-values (adjs computers)
  (for/fold ([h (hash)]
             [s (set)])
            ([connection input])
    (match-define (list lhs rhs) (string-split connection "-"))
    (define (updater new)
      (lambda (s) (set-add s new)))
    (values
     (hash-update (hash-update h lhs (updater rhs) (curry set lhs)) rhs (updater lhs) (curry set rhs))
     (set-add (set-add s lhs) rhs))))

(define (maximal-cliques computers)
  (match computers
    [(cons h '()) (set (list h))]
    [(cons h t)
     (define adj (hash-ref adjs h))
     (let* ([Gdh (maximal-cliques t)]
            [expanded (list->set
                       (set-map Gdh
                                (lambda (st)
                                  (cons h (filter (lambda (elem) (set-member? adj elem)) st)))))])
       (set-union Gdh expanded))]))

(define max-cliques (maximal-cliques (set->list computers)))

(length (filter (lambda (ls)
                  (and (equal? (length ls) 3) (ormap (lambda (str) (string-prefix? str "t")) ls)))
                (set->list max-cliques)))

(string-join (sort (foldl (lambda (cln clo) (if (> (length cln) (length clo)) cln clo))
                          '()
                          (set->list max-cliques))
                   string<?)
             ",")
