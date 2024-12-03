#lang racket
(require "../utils.rkt")

(define input (file->string (rpath "input.txt")))

(define (pairs str)
  (map (curry map string->number)
       (regexp-match* #px"mul\\((\\d{1,3}),(\\d{1,3})\\)" str #:match-select (lambda (ls) (cdr ls)))))

(define enabled
  (string-append*
   ""
   (regexp-match* #px"(?:^|do\\(\\))(.*?)(?:$|don't\\(\\))" input #:match-select cadr)))

(for/sum ([pair (pairs input)]) (apply * pair))
(for/sum ([pair (pairs enabled)]) (apply * pair))
