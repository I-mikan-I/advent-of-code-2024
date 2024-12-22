#lang racket
(require "../utils.rkt")
(define input (file->lines (rpath "input.txt")))

; (define targets '(#\0 #\2 #\9 #\A))
(define targets (map string->list input))

(define (transform chr)
  (case chr
    [(#\A) 0]
    [(#\0) -1]
    [else (string->number (string chr))]))

(define kp
  (hash 0
        '((L . -1) (U . 3))
        -1
        '((R . 0) (U . 2))
        1
        '((R . 2) (U . 4))
        4
        '((R . 5) (U . 7) (D . 1))
        7
        '((R . 8) (D . 4))
        8
        '((L . 7) (R . 9) (D . 5))
        5
        '((L . 4) (U . 8) (R . 6) (D . 2))
        2
        '((L . 1) (U . 5) (R . 3) (D . -1))
        9
        '((L . 8) (D . 6))
        6
        '((L . 5) (U . 9) (D . 3))
        3
        '((L . 2) (D . 0) (U . 6))))

(define remote
  (hash 'A
        '((D . R) (L . U))
        'R
        '((U . A) (L . D))
        'U
        '((R . A) (D . D))
        'D
        '((R . R) (U . U) (L . L))
        'L
        '((R . D))))

(define (shortest adjs l right)
  (define sols
    (let rec ([visited (set l)]
              [start l])
      (define next (filter (lambda (pr) (not (set-member? visited (cdr pr)))) (hash-ref adjs start)))
      (if (eq? right start)
          '(())
          (let ([visited (set-union visited (list->set (map cdr next)))])
            (apply append
                   (for/list ([next next])
                     (map (curry cons (car next)) (rec visited (cdr next)))))))))
  (filter (lambda (ls) (<= (length ls) (apply min (map length sols)))) sols))

(define cache (make-hash))

(define (opt-min moves times)
  (hash-ref! cache
             (cons moves times)
             (lambda ()
               (cond
                 [(eq? times 0) (+ 1 (length moves))]
                 [else
                  (define pairs
                    (let ([moves (cons 'A (append moves '(A)))])
                      (for/list ([move moves]
                                 [move2 (cdr moves)])
                        (cons move move2))))
                  (define opt-pairs
                    (map (lambda (pair)
                           (let ([moves_ (map (lambda (ls) ls)
                                              (shortest remote (car pair) (cdr pair)))])
                             (let ([res (foldl (lambda (move opt)
                                                 (let ([opt-new (opt-min move (- times 1))])
                                                   (if (< opt-new opt) opt-new opt)))
                                               (opt-min (car moves_) (- times 1))
                                               moves_)])
                               res)))
                         pairs))

                  (apply + opt-pairs)]))))

(define (solution targets num)
  (define begin-moves (cons 0 (map transform targets)))
  (for/sum ([m1 begin-moves] [m2 (cdr begin-moves)])
           (define remote1 (map (lambda (ls) ls) (shortest kp m1 m2)))
           (foldl (lambda (move opt)
                    (let ([opt-new (opt-min move num)]) (if (< opt-new opt) opt-new opt)))
                  (opt-min (car remote1) num)
                  remote1)))

(for/sum ([target targets])
         (* (solution target 2) (string->number (apply string (take target (- (length target) 1))))))
(for/sum ([target targets])
         (* (solution target 25) (string->number (apply string (take target (- (length target) 1))))))
