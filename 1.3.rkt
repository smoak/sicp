#lang racket
(require rackunit)

(define (cube x)
  (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))

(check-equal? (+ (cube 3) (cube 4)) (sum-cubes 3 4))
(check-equal? 3025 (sum-cubes 1 10))

(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))

(check-equal? 55 (sum-integers 1 10))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(check-equal? 3.139592655589783 (* 8 (pi-sum 1 1000))) ; might fail due to floating point precision?

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

; exercise 1.29

;(define (simpsons-integral f a b n)
;  (define h (/ (- b a) n))
;  (* (/ h 3) (sum f (+ a (* k h))))
;)
; not sure how to compute k?

; exercise 1.30

(define (sum term a next b)
  (define (iter a result)
    (if 