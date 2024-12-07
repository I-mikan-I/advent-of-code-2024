#lang racket

(require "../utils.rkt")
(define input (map string->list (string-split (file->string (rpath "input.txt")) "\n")))

(define obstacles
  (let* ([cols (map (lambda (l) (indexes-of l #\#)) input)]
         [rows (map list (range (length input)))]
         [combs (sequence->list (in-values-sequence (in-parallel rows cols)))])
    (begin
      (for*/set ([ls (map (lambda (pr)
                            (map (curry apply cons) (cartesian-product (car pr) (cadr pr))))
                          combs)]
                 [e ls])
        e))))
(define startpos
  (let* ([row (index-where input (curry member #\^))]
         [column (index-of (list-ref input row) #\^)])
    (cons row column)))

(define (visited obstacles)
  (let rec ([fields (set startpos)]
            [cache (hash)]
            [current startpos]
            [direction 'UP])
    (match-let ([(list x_ y_ maybe-dir) (match/values (values current direction)
                                                      [((cons x y) 'UP) (list (- x 1) y 'RIGHT)]
                                                      [((cons x y) 'DOWN) (list (+ x 1) y 'LEFT)]
                                                      [((cons x y) 'RIGHT) (list x (+ y 1) 'DOWN)]
                                                      [((cons x y) 'LEFT) (list x (- y 1) 'UP)])])
      (cond
        [(set-member? (hash-ref cache current set) direction) #f]
        [(or (>= x_ (length input)) (>= y_ (length (car input)))) fields]
        [(or (< x_ 0) (< y_ 0)) fields]
        [(set-member? obstacles (cons x_ y_))
         (rec fields
              (hash-update cache current (lambda (s) (set-add s direction)) set)
              current
              maybe-dir)]
        [else
         (begin
           (rec (set-add fields (cons x_ y_))
                (hash-update cache current (lambda (s) (set-add s direction)) set)
                (cons x_ y_)
                direction))]))))
(set-count (visited obstacles))
(for/sum ([option (visited obstacles)] #:when (not (equal? option startpos)))
         (if (not (visited (set-add obstacles option))) 1 0))
