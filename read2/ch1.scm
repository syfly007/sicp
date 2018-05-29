(* (+ 2 (* 4 6))
   (+ 3 5 7))
(define a 5)
(display "hello world")

(define (square x)
  (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(sum-of-squares 3 4)

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))
(display (f 5))

(sum-of-squares (+ 5 1) (* 5 2))

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))

(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define (>= x y)
  (or (> x y)(= x y)))

(define (>= x y)
  (not (< x y)))

;1.1.7
(define (sqrt x)
  (sqrt-iter 1.0 x))
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))
(define (good-enough? guess x)
  (< (abs (- x (square guess))) 0.001))
(define (improve guess x)
  (average (/ x guess) guess))
(define (average x y)
  (/ (+ x y) 2.0))

(sqrt 9)
3.00009155413138

(sqrt (+ 100 37))
11.704699917758145

(sqrt (+ (sqrt 2) (sqrt 3)))
1.7739279023207892

(square (sqrt 1000))
1000.000369924366

;1.1.8
(define (sqrt x)
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))

;1.2.1
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(define (factorial n)
  (fact-iter 1 1 n))
(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter 
        (* product counter) 
        (+ counter 1) 
        max-count)))

;1.2.2
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))
(define (fib n)
  (fib-iter 1 0 n))
(define (fib-iter a b count)
  (cond ((= 0 count) b)
        (else (fib-iter (+ a b) a (- count 1)))))

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0)
             (= kinds-of-coins 0)) 0)
        (else (+ (cc amount (- kinds-of-coins 1))
                 (cc (- amount (first-denomination kinds-of-coins)) 
                     kinds-of-coins)))))
(define (first-denomination denomination)
  (cond ((= denomination 1) 1)
        ((= denomination 2) 5)
        ((= denomination 3) 10)
        ((= denomination 4) 25)
        ((= denomination 5) 50)))
