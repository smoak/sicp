#lang racket
(require rackunit)

(define (average a b)
  (/ (+ a b) 2))

(define (square x)
  (* x x))

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

; exercise 1.33
; skipping


(define (f g)
  (g 2))

(check-equal? 4 (f square))

(check-equal? 6 (f (lambda (z) (* z (+ z 1)))))

; exercise 1.34

; infinite loop?

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average-damp f)
  (lambda (x) (average x (f x))))


(check-equal? 55 ((average-damp square) 10))

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

; exercise 1.40


; exercise 1.41

(define (double func)
  (lambda (x) (func (func x))))

((double inc) 1)
(((double (double double)) inc) 5)

; exercise 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

(check-equal? 49 ((compose square inc) 6))

; exercise 1.43
(define (repeated func times)
  (if (= times 1)
      func
      (compose func (repeated func (- times 1)))))

(check-equal? 625 ((repeated square 2) 5))

; exercise 1.44
(define (smooth func dx)
  (lambda (x) (/ (+ (func (- x dx)) (func x) (func (+ x dx))) 3)))

((smooth sin 0.7) (/ pi 2))

(define (n-fold-smooth func dx n)
  (repeated (smooth func dx) n))

; exercise 1.45

; exercise 1.46
; can this be done with a lambda?
(define (iterative-improve good-enough-func improve-guess-func)
  (define (try-improve guess)
    (if (good-enough-func guess)
        guess
        (try-improve (improve-guess-func guess))))
  try-improve)
