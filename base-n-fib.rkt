#lang racket/base

(provide (all-defined-out))

(require racket/match
         racket/list
         )
(module+ test
  (require rackunit))

(define digitss '(#"0123456789"
                  #"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(define (find-digits digitss #:base base)
  (match digitss
    [(list) (error 'idontknowwhattodo)]
    [(cons digits rst)
     (cond [(<= base (bytes-length digits)) digits]
           [else (find-digits rst #:base base)])]))

(define (digit-writer #:base base)
  (define digits (find-digits digitss #:base base))
  (define (write-digit d)
    (write-byte (bytes-ref digits d)))
  write-digit)

(define (write-spaces n)
  (for ([i (in-range n)]) (write-char #\space)))

(define (b-writer/w #:width w #:base base)
  (define write-b (b-writer #:base base))
  (define (write-b/w b)
    (write-spaces (- w (length b)))
    (write-b b))
  write-b/w)

(define (b-writer #:base base)
  (define write-digit (digit-writer #:base base))
  (define (write-b b)
    (for ([d (in-list (reverse b))]) (write-digit d)))
  write-b)

(define ((writer/ln writer) x)
  (writer x) (newline))

(define (display-base-n-fibs f0 f1 #:up-to n #:base base)
  (define write-b (writer/ln (b-writer #:base base)))
  (printf "base: ~v, digits: " base)
  (write-b (reverse (range base)))
  (define rr-fibs (reversed-base-n-fibs f0 f1 #:up-to n #:base base))
  (define write-b/w (writer/ln (b-writer/w #:width (length (first rr-fibs)) #:base base)))
  (define n.len (string-length (number->string n)))
  (for ([r-fib (in-list (reverse rr-fibs))] [i (in-naturals)])
    (define i.str (number->string i))
    (write-spaces (- n.len (string-length i.str)))
    (printf "~a | " i.str)
    (write-b/w r-fib)))

(define (reversed-base-n-fibs f0 f1 #:up-to up-to-n #:base base)
  (base-n-fibs-loop (list f1 f0) 2 #:up-to up-to-n #:base base))

(define (base-n-fibs f0 f1 #:up-to up-to-n #:base base)
  (reverse (reversed-base-n-fibs f0 f1 #:up-to up-to-n #:base base)))

(define (fib n #:base base)
  (match n [0 '(0)] [1 '(1)]
    [n (first (reversed-base-n-fibs '(0) '(1) #:up-to n #:base base))]))

(define (base-n-fibs-loop accum n #:up-to up-to-n #:base base)
  (cond [(< up-to-n n) accum]
        [else
         (define f-1 (first accum))
         (define f-2 (second accum))
         (define f (b+ f-2 f-1 #:base base))
         (base-n-fibs-loop (cons f accum) (add1 n) #:up-to up-to-n #:base base)]))

(define (first+rest/0+null lst)
  (cond [(empty? lst) (values 0 '())]
        [else (values (first lst) (rest lst))]))

(define (b+ b1 b2 #:base base)
  (define (loop b1 b2 rb carry)
    (cond [(and (empty? b1) (empty? b2))
           (match carry
             [0 (reverse rb)]
             [n (reverse (cons n rb))])]
          [else
           (define-values [b1.fst b1.rst] (first+rest/0+null b1))
           (define-values [b2.fst b2.rst] (first+rest/0+null b2))
           (define-values [new-carry d]
             (q/r (+ b1.fst b2.fst carry) #:base base))
           (loop b1.rst b2.rst (cons d rb) new-carry)]))
  (loop b1 b2 '() 0))

(define (q/r n #:base base)
  (define (loop q n)
    (cond [(< n base) (values q n)]
          [else (loop (add1 q) (- n base))]))
  (loop 0 n))

(module+ test
  (test-case "b+"
    (define-check (check-b+ base b1 b2 b)
      (check-equal? (b+ b1 b2 #:base base) b)
      (check-equal? (b+ b2 b1 #:base base) b))
    (check-b+ 2 '() '() '())
    (check-b+ 2 '(1) '() '(1))
    (check-b+ 2 '(1) '(1) '(0 1))
    (check-b+ 2 '(0 1) '() '(0 1))
    (check-b+ 2 '(0 1) '(1) '(1 1))
    )
  (test-case "base-n-fibs"
    (check-equal?
     (base-n-fibs '(0) '(1) #:up-to 12 #:base 10)
     (map reverse '((0) (1) (1) (2) (3) (5) (8) (1 3) (2 1) (3 4) (5 5) (8 9) (1 4 4)))))
  (display-base-n-fibs '(0) '(1) #:up-to (* 32 3) #:base 2)
  (define write-b (writer/ln (b-writer #:base 10)))
  (void (write-string "(fib (* 1 60) #:base 10) = ")
        (write-b       (fib (* 1 60) #:base 10)))
  (void (write-string "(fib (* 10 60) #:base 10) = ")
        (write-b       (fib (* 10 60) #:base 10)))
  (void (write-string "(fib (* 100 60) #:base 10) = ")
        (write-b       (fib (* 100 60) #:base 10)))
  )
