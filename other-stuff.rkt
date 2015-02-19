#lang racket/base

(require racket/match
         racket/list
         plot
         "base-n-fib.rkt"
         )
(module+ test
  (require rackunit))

(define (find-repeating-pattern n #:base b)
  (define f0 (make-list n 0))
  (define f1 (cons 1 (make-list (- n 1) 0)))
  (reverse (find-repeating-pattern-loop f0 f1 (list f1 f0) n #:base b)))

(define (find-repeating-pattern-loop f0 f1 rev-accum n #:base b)
  (match rev-accum
    [(list-rest (== f1) (== f0) (and rst (cons _ _)))
     rst]
    [(list-rest fn fn-1 _)
     (define next (take (b+ fn-1 fn #:base b) n))
     (find-repeating-pattern-loop f0 f1 (cons next rev-accum) n #:base b)]))

(define (display-repeated-pattern n #:base b)
  (define write-b (b-writer/ln #:base b))
  (printf "base: ~v, digits: " b)
  (write-b (reverse (range b)))
  (for-each write-b (find-repeating-pattern n #:base b)))

(define (find-rep-pat-lengths n)
  (for/list ([i (in-range 2 (add1 n))])
    (list i (length (find-repeating-pattern 1 #:base i)))))

(define (plot-rep-pat-length-scatter-plot n #:rep-pat-lengths [rep-pat-lengths (find-rep-pat-lengths n)])
  (plot (list (axes) (tick-grid) (points rep-pat-lengths #:sym 'point #:color "red"))
        #:x-min -5 #:x-max n #:y-min -5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (check-equal? (find-repeating-pattern 1 #:base 2) '([0]
                                                      [1]
                                                      [1]))
  (check-equal? (find-repeating-pattern 2 #:base 2) '([0 0]
                                                      [1 0]
                                                      [1 0]
                                                      [0 1]
                                                      [1 1]
                                                      [1 0]))
  (check-equal? (find-repeating-pattern 3 #:base 2) '([0 0 0]
                                                      [1 0 0]
                                                      [1 0 0]
                                                      [0 1 0]
                                                      [1 1 0]
                                                      [1 0 1]
                                                      [0 0 0]
                                                      [1 0 1]
                                                      [1 0 1]
                                                      [0 1 0]
                                                      [1 1 1]
                                                      [1 0 0]))
  (display-repeated-pattern 4 #:base 2)
  (define rep-pat-lengths
    (find-rep-pat-lengths 10000))
  (for ([p (in-list rep-pat-lengths)] [_ (in-range 21)])
    (match-define (list i len) p)
    (printf "(length (find-repeating-pattern 1 #:base ~v)) = ~v\n" i len))
  (argmax second rep-pat-lengths)
  (plot-rep-pat-length-scatter-plot 10000 #:rep-pat-lengths rep-pat-lengths)
  )
