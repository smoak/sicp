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

; exercise 1.3

(define (sum-of-squares a b)
  (+ (* a a) (* b b)))

(define (sum-of-two-largest-squares a b c)
  (cond ((and (> a c) (> b c)) (sum-of-squares a b))
        ((and (> b a) (> c a)) (sum-of-squares b c))
        ((and (> a b) (> c b)) (sum-of-squares a c))
        ))
(check-equal? (+ 36 64) (sum-of-two-largest-squares 4 6 8))
(check-equal? (+ 36 64) (sum-of-two-largest-squares 6 4 8))
(check-equal? (+ 36 64) (sum-of-two-largest-squares 6 8 4))

; exercise 1.4
; if b is positive it will a and b together
; if b is negative it will subtract b from a
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; exercise 1.5

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

; (test 0 (p))
; applicative-order
; evaluate 0 as 0
; evaluate p as p
; continue infinitely
; normal-order
; expand test and then eval the if. 0 is 0 so eval 0 and never eval p

; exercise 1.6

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

;(define (sqrt-iter guess x)
;  (new-if (good-enough? guess x)
;          guess
;          (sqrt-iter (improve guess x)
;                     x)))

; This won't work because it has to evaluate both branches and so it will infinitely recurse

; exercise 1.7
(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

; Not entirely sure about this exercise

; exercise 1.8
(define (cube-good-enough? guess x)
  (< (abs (- (* guess guess guess) x)) 0.001))

(define (improve-cube guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (cube-root-iter guess x)
  (if (cube-good-enough? guess x)
      guess
      (cube-root-iter (improve-cube guess x)
                      x)))

(define (cube-root x)
  (cube-root-iter 1.0 x))

(cube-root 512)