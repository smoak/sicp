#lang racket
(require rackunit)

(define (cube x)
  (* x x x))

(define (sum func a next-func b)
  (if (> a b)
      0
      (+ (func a)
         (sum func (next-func a) next-func b))))

(define (inc n) (+ n 1))

(define (sum-cubes start end)
  (sum cube start inc end))   ;  (start..end).map { |x| cube(x) }.inject(:+)

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

;(define (sum-iter func a next-func b)
;  (define (iter a result)
;    (if (> a result)
;        0

; exercise 1.31
; bleh too much math

; exercise 1.32

(define (sum-accumulate func a next-func b)
  (define (combine a b)
    (+ a b)
  )
  (accumulate combine 0 func a next-func b)
)

(define (accumulate combiner null-value func current next-func end)
  (define (next-value) (next-func current))
  (define (calculated-value) (func current))
  (define (next-values) (accumulate combiner null-value func (next-value) next-func end))
  (if (> current end)
      null-value
      (combiner (calculated-value) (next-values))
  )
)

(check-equal? 3 (sum-accumulate identity 1 inc 2))
(check-equal? 15 (sum-accumulate identity 1 inc 5)) ; 1 + 2 + 3 + 4 + 5 = 15