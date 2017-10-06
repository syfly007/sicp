(load "ch1.scm")

(define (root-cube x)
  (root-cube-iter 1.0 x))
(define (root-cube-iter guess x)
  (if (good-enough? guess x)
      guess
      (root-cube-iter (improve guess x) x)))
(define (good-enough? guess x)
  (< (abs (- guess (improve guess x))) (* guess 0.00001)))
(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))