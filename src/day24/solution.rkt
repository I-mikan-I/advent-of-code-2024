#lang racket

(require "../utils.rkt")

(define input (file->string (rpath "input.txt")))

(match-define (list defs gates) (string-split input "\n\n"))

(define vals
  (make-hash (map (lambda (ln)
                    (let ([split (string-split ln ": ")])
                      (cons (car split) (string->number (cadr split)))))
                  (string-split defs "\n"))))
(define deps
  (make-hash (map (lambda (ln)
                    (match ln
                      [(pregexp #px"(\\w+) AND (\\w+) -> (\\w+)" (list _ lhs rhs target))
                       (cons target (list 'AND lhs rhs))]
                      [(pregexp #px"(\\w+) OR (\\w+) -> (\\w+)" (list _ lhs rhs target))
                       (cons target (list 'OR lhs rhs))]
                      [(pregexp #px"(\\w+) XOR (\\w+) -> (\\w+)" (list _ lhs rhs target))
                       (cons target (list 'XOR lhs rhs))]))
                  (string-split gates "\n"))))

(define (get-val line vals)
  (hash-ref! vals
             line
             (lambda ()
               (match-define (list OP lhs rhs) (hash-ref deps line))
               (define lhsval (get-val lhs vals))
               (define rhsval (get-val rhs vals))
               (case OP
                 ['AND (bitwise-and lhsval rhsval)]
                 ['OR (bitwise-ior lhsval rhsval)]
                 ['XOR (bitwise-xor lhsval rhsval)]))))

(get-val "z00" vals)

(for/sum ([n (in-naturals)])
         (define name (format "z~a" (~a n #:width 2 #:align 'right #:pad-string "0")))
         #:break (not (hash-has-key? deps name))
         (* (get-val name vals) (expt 2 n)))

; z_n = p_n XOR g_n-1
; p_n = x_n XOR y_n
; g_n = gl_n OR gr_n
; gl_n = x_n AND y_n
; gr_n = p_n AND g_n-1
; g_0 = x_0 AND y_0

(define (label lines)
  (let rec ([labels (make-immutable-hash (filter-map (lambda (line)
                                                       (match line
                                                         [(pregexp #px"x(\\d+)" (list _ val))
                                                          (cons line (list 'x (string->number val)))]
                                                         [(pregexp #px"y(\\d+)" (list _ val))
                                                          (cons line (list 'y (string->number val)))]
                                                         [_ #f]))
                                                     lines))])
    (let ([new-labels
           (for/fold ([labels labels])
                     ([line lines]
                      #:when (hash-has-key? deps line))
             (match-define (list OP lhs rhs) (hash-ref deps line))
             (define lhslabel (hash-ref labels lhs (lambda () #f)))
             (define rhslabel (hash-ref labels rhs (lambda () #f)))
             (define newlabel
               (and lhslabel
                    rhslabel
                    (match (cons OP
                                 (if (symbol<? (car lhslabel) (car rhslabel))
                                     (list lhslabel rhslabel)
                                     (list rhslabel lhslabel)))
                      [(list 'AND (list 'x 0) (list 'y 0)) (list 'g 0)]
                      [(list 'AND (list 'x v1) (list 'y v2))
                       #:when (eq? v1 v2)
                       (list 'gl v1)]
                      [(list 'XOR (list 'x v1) (list 'y v2))
                       #:when (eq? v1 v2)
                       (list 'p v1)]
                      [(list 'XOR (list 'g v2) (list 'p v1))
                       #:when (eq? v1 (+ 1 v2))
                       (list 'z v1)]
                      [(list 'AND (list 'g v2) (list 'p v1))
                       #:when (eq? v1 (+ 1 v2))
                       (list 'gr (+ v1 0))]
                      [(list 'OR (list 'gl v1) (list 'gr v2))
                       #:when (eq? v1 v2)
                       (list 'g v1)]
                      [_
                       (printf "could not find fitting gate: ~v ~v ~v\n" OP lhslabel rhslabel)
                       #f])))
             (if newlabel
                 (hash-set labels line newlabel)
                 labels))])
      (if (equal? new-labels labels)
          new-labels
          (rec new-labels)))))

(define lines (append (hash-keys vals) (hash-keys deps)))
(label lines)
; swap vcv and z13
; swap z19 and vwp
; swap mps and z25
; swap cqm and vjv

(sort (list "vcv" "z13" "z19" "vwp" "mps" "z25" "cqm" "vjv") string<?)