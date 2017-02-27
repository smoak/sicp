#lang racket/base
(require rackunit)

; exercise 1.1
(check-equal? 10 10)
(check-equal? 12 (+ 5 3 4))
(check-equal? 8 (- 9 1))
(check-equal? 3 (/ 6 2))
(check-equal? 6 (+ (* 2 4) (- 4 6)))

(define a 3)
(check-equal? 3 a)

(define b (+ a 1))
(check-equal? 4 b)

(check-equal? 19 (+ a b (* a b)))
(check-equal? #f (= a b))
(check-equal? 4 (if (and (> b a) (< b (* a b)))
    b
    a))
(check-equal? 16 (cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25)))
(check-equal? 6 (+ 2 (if (> b a) b a)))
(check-equal? 16 (* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1)))

; exercise 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))
