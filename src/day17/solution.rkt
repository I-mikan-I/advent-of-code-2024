#lang racket
(require "../utils.rkt")
; (define input (file->string (rpath "input.txt")))

(define A (box 51342988))
(define B (box 0))
(define C (box 0))
(define IP (box 0))
(define program
  (list->vector (map string->number (string-split "2,4,1,3,7,5,4,0,1,3,0,3,5,5,3,0" ","))))
; bst 4 (A) B = A%8
; bxl 3 (3) B = B xor 3
; cdv 5 (B) C = A / 2^B
; bxc 0 (0) B = B xor C
; bxl 3 (3) B = B xor 3
; adv 3 (3) A = A / 8
; out 5 (B)
; jnz 0 (A != 0)

(define/match (combo op)
  [((or 0 1 2 3)) op]
  [(4) (unbox A)]
  [(5) (unbox B)]
  [(6) (unbox C)]
  [(_) (error "bad combo operand" op)])

(define (xdv reg op)
  (set-box! reg (quotient (unbox A) (expt 2 (combo op)))))
(define (bst op)
  (set-box! B (modulo (combo op) 8)))
(define (bxl op)
  (set-box! B (bitwise-xor (unbox B) op)))
(define (jnz op)
  (if (equal? (unbox A) 0)
      (void)
      (set-box! IP (- op 2))))
(define (bxc op)
  (set-box! B (bitwise-xor (unbox B) (unbox C))))
(define (out output op)
  (cons (modulo (combo op) 8) output))

(define (eval output)
  (if (>= (unbox IP) (vector-length program))
      output
      (let ([opc (vector-ref program (unbox IP))]
            [operand (vector-ref program (+ 1 (unbox IP)))])
        (begin
          ; (printf "opc: ~a operand: ~a\nA: ~a B: ~a C: ~a\n" opc operand (unbox A) B C)
          (match opc
            [0 (xdv A operand)]
            [1 (bxl operand)]
            [2 (bst operand)]
            [3 (jnz operand)]
            [4 (bxc operand)]
            [5 (set! output (out output operand))]
            [6 (xdv B operand)]
            [7 (xdv C operand)])
          (set-box! IP (+ (unbox IP) 2))
          (eval output)))))

(eval '())

(define (reset a)
  (set-box! A a)
  (set-box! B 0)
  (set-box! C 0)
  (set-box! IP 0))

(define (find-A want)

  (match want
    ['() '(0)]
    [(cons _ t)
     (let ([sols (map (curry * 8) (find-A t))])
       (for*/list ([hi sols]
                   [lo (in-range 8)]
                   #:when (equal? want
                                  (begin
                                    (reset (+ hi lo))
                                    (reverse (eval '())))))
         (+ hi lo)))]))

(find-A (vector->list program))

(reset 108107574778365)
(eval '())
