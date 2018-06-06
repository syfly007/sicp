(load "ch1.scm")

(define (sqrt x)
  (sqrt-iter 1.0 x))
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))
(define (good-enough? guess x)
  (< 
    (abs (- guess (improve guess x))) 
    (* guess 0.00001)))
(define (improve guess x)
  (average (/ x guess) guess))
(define (average x y)
  (/ (+ x y) 2.0))