#lang racket
(require "../utils.rkt")
(define input (file->string (rpath "input.txt")))

(define patterns (string-split (car (string-split input "\n\n")) ", " #:trim? #t))
(define designs (string-split (cadr (string-split input "\n\n")) "\n"))

(define cache (make-hash))
(define (valid design)
  (hash-ref! cache
             design
             (lambda ()
               (if (non-empty-string? design)
                   (let ([trimmed (for/list ([pattern patterns]
                                             #:when (string-prefix? design pattern))
                                    (string-trim design pattern #:right? #f))])
                     (for/sum ([trimmed trimmed]) (valid trimmed)))
                   1))))

(for/sum ([design designs] [i (in-naturals)]) (println i) (valid design))
