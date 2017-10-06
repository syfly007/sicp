(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

; def fibTest(n):
;   a=(1+5.0**(0.5))/2
;   b=(1-5.0**(0.5))/2
;   print (a**n-b**n)/(5.0**(0.5))

#| def  fib(n):
  return fib_iter(1,0,n)

def fib_iter(a,b,count):
  if count == 0:
    return b
  else:
    return fib_iter((a+b),a,(count-1)) |#


; exp1.13
; 见解题集

; exp1.14
; 见思维导图
; Space(n,k) = O(n)
; Step(n,k) = o(n^k)
; 详细解释见 http://www.billthelizard.com/2009/12/sicp-exercise-114-counting-change.html，需要翻墙
; http://www.cppblog.com/cuigang/archive/2008/03/26/45470.html解释更精确


; exp1.15
(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))
(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle) 
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

; space&step O(log a) 以3为底数

; my expt
(define (expt b n) 
  (if (= n 0) 
    1
    (* b (expt b (- n 1)))))

(define (expt b n) 
  (expt-iter 1 b n))
(define (expt-iter t b n) 
  (if (= n 0)
    t
    (expt-iter (* t b) b (- n 1))))  

; book expt
(define (expt b n)
   (expt-iter b n 1))
 
(define (expt-iter b counter product)
   (if (= counter 0)
       product
       (expt-iter b
                 (- counter 1)
                 (* b product)))) 

(define (fast-expt b n) 
  (cond ((= n 0) 1) 
        ((even? n) (square (fast-expt b (/ n 2)))) 
        (else (* b (fast-expt b (- n 1))))))

(define (square x) (* x x))
; remainder函数lisp内置的
(define (even? n) (=  (remainder n 2) 0))
(define (half n) ( if (=  (remainder n 2) 0) 
                      (/ n 2) 
                      (/ (- n 1) 2)))

(define (fast-expt b n) 
  (fast-expt-iter b n 1))
(define (fast-expt-iter b n a) 
  (cond ((= n 0) a) 
        ((even? (half n)) (fast-expt-iter (* b b) (half n) a)) 
        (else (fast-expt-iter b (half n) (* a b)))))

; exp1.19
(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   ??FILL-THIS-IN?? ; compute p'
                   ??FILL-THIS-IN?? ; compute q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

(define (gcd a b)
  (if (= b 0) a
    (gcd b (remainder a b))))

; exp1.20
; 说明应用序一般情况下效率高于正则序

(define (smallest-divisor n) 
  (find-divisor n 2))
(define (find-divisor n test-divisor) 
  ( cond  ((> (square test-divisor) n) n) 
          ((divisors? n test-divisor) test-divisor) 
          (else (find-divisor n (+ test-divisor 1)))))
(define (divisors? a b) 
  (= 0 (remainder a b)))
(define (square x)(* x x))

(define (prime? n) (= n (smallest-divisor n)))


(define (expmod base exp m) 
  (cond ((= exp 0) 1) 
        ((even? exp) 
          (remainder (square (expmod base (/ exp 2) m)) 
            m)) 
        (else 
          (remainder (* (expmod base (- exp 1) m) base) 
            m))))

(define (fermat-test n) 
  (define (try-it a) 
    (= (expmod a n n ) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times) 
  (cond ((= times 0) true) 
        ((fermat-test n) (fast-prime? n (- times 1))) 
        (else false)))

; 实际执行用的
(define (fast-prime? n times) 
  (cond ((= times 0) #t) 
        ((fermat-test n) (fast-prime? n (- times 1))) 
        (else #f)))

; exp1.21
(smallest-divisor 199) 
;199
(smallest-divisor 1999)
;1999
(smallest-divisor 19999)
;7

; exp1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))


; my solution
(define (square x) (* x x))
(define (next-odd n) 
  (if (odd? n) 
    (+ n 2)
    (+ n 1)))
(define (odd? n) (= 1 (remainder n 2)))

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (runtime) (current-inexact-milliseconds))
(define (search-prime startNum count) 
  (display "Search from :") 
  (display startNum) 
  (newline) 
  (start-my-prime-test startNum startNum count (runtime))) 
(define (start-my-prime-test startNum currNum count startTime) 
  (cond ((= count 0) (report-my-search startNum currNum startTime)) 
        ((prime? currNum) 
            (display "find prime:") 
            (display currNum) 
            (newline) 
            (start-my-prime-test startNum (next-odd currNum) (- count 1) startTime)) 
        (else (start-my-prime-test startNum (next-odd currNum) count startTime))))
(define (report-my-search startNum currNum startTime) 
  (display "cost total time:") 
  (display (- (runtime) startTime)) 
  (display " every test cost time:") 
  (display (/ (- (runtime) startTime) (- currNum startNum))))

; > (search-prime 100000000 3)
; Search from :100000000
; find prime:100000007
; find prime:100000037
; find prime:100000039
; cost total time:22.0 every test cost time:0.5365853658536586
; > (search-prime 1000000000 3)
; Search from :1000000000
; find prime:1000000007
; find prime:1000000009
; find prime:1000000021
; cost total time:51.0 every test cost time:2.260869565217391
; > (search-prime 10000000000 3)
; Search from :10000000000
; find prime:10000000019
; find prime:10000000033
; find prime:10000000061
; cost total time:341.0 every test cost time:5.412698412698413
; > (search-prime 100000000000 3)
; Search from :100000000000
; find prime:100000000003
; find prime:100000000019
; find prime:100000000057
; cost total time:981.0 every test cost time:16.627118644067796
; > (search-prime 1000000000000 3)
; Search from :1000000000000
; find prime:1000000000039
; find prime:1000000000061
; find prime:1000000000063
; cost total time:3070.0 every test cost time:47.23076923076923
; > (search-prime 10000000000000 3)
; Search from :10000000000000
; find prime:10000000000037
; find prime:10000000000051
; find prime:10000000000099
; cost total time:12071.0 every test cost time:119.51485148514851
; > (search-prime 100000000000000 3)
; Search from :100000000000000
; find prime:100000000000031
; find prime:100000000000067
; find prime:100000000000097
; cost total time:41210.0 every test cost time:416.26262626262627

; exp1.23
(define (next-test-divisor n) 
  (if (= n 2)
    3
    (+ n 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next-test-divisor test-divisor)))))

; > (search-prime 100000000 3)
; Search from :100000000
; find prime:100000007
; find prime:100000037
; find prime:100000039
; cost total time:19.0 every test cost time:0.4634146341463415
; > (search-prime 1000000000 3)
; Search from :1000000000
; find prime:1000000007
; find prime:1000000009
; find prime:1000000021
; cost total time:37.0 every test cost time:1.608695652173913
; > (search-prime 10000000000 3)
; Search from :10000000000
; find prime:10000000019
; find prime:10000000033
; find prime:10000000061
; cost total time:186.0 every test cost time:2.9523809523809526
; > (search-prime 100000000000 3)
; Search from :100000000000
; find prime:100000000003
; find prime:100000000019
; find prime:100000000057
; cost total time:580.0 every test cost time:9.830508474576272
; > (search-prime 1000000000000 3)
; Search from :1000000000000
; find prime:1000000000039
; find prime:1000000000061
; find prime:1000000000063
; cost total time:1710.0 every test cost time:26.307692307692307
; > (search-prime 10000000000000 3)
; Search from :10000000000000
; find prime:10000000000037
; find prime:10000000000051
; find prime:10000000000099
; cost total time:6844.0 every test cost time:67.76237623762377
; > (search-prime 100000000000000 3)
; Search from :100000000000000
; find prime:100000000000031
; find prime:100000000000067
; find prime:100000000000097
; cost total time:23083.0 every test cost time:233.16161616161617

; 是要快一些，但比值不是2

; exp1.24
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))    

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (my-random (- n 1)))))

(define MaxNum 4294967087)
(define (my-random n) 
  (if (<= n MaxNum)
    (random n)
    (random MaxNum)))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

(define (start-my-prime-test startNum currNum count startTime) 
  (cond ((= count 0) (report-my-search startNum currNum startTime)) 
        ((fast-prime? currNum 10) 
            (display "find prime:") 
            (display currNum) 
            (newline) 
            (start-my-prime-test startNum (next-odd currNum) (- count 1) startTime)) 
        (else (start-my-prime-test startNum (next-odd currNum) count startTime))))

; > (search-prime 100000000 3)
; Search from :100000000
; find prime:100000007
; find prime:100000037
; find prime:100000039
; cost total time:5.0 every test cost time:0.14634146341463414
; > (search-prime 1000000000 3)
; Search from :1000000000
; find prime:1000000007
; find prime:1000000009
; find prime:1000000021
; cost total time:7.0 every test cost time:0.30434782608695654
; > (search-prime 10000000000 3)
; Search from :10000000000
; find prime:10000000019
; find prime:10000000033
; find prime:10000000061
; cost total time:60.0 every test cost time:0.9523809523809523
; > (search-prime 100000000000 3)
; Search from :100000000000
; find prime:100000000003
; find prime:100000000019
; find prime:100000000057
; cost total time:56.0 every test cost time:0.9661016949152542
; > (search-prime 1000000000000 3)
; Search from :1000000000000
; find prime:1000000000039
; find prime:1000000000061
; find prime:1000000000063
; cost total time:63.0 every test cost time:0.9692307692307692
; > (search-prime 10000000000000 3)
; Search from :10000000000000
; find prime:10000000000037
; find prime:10000000000051
; find prime:10000000000099
; cost total time:97.0 every test cost time:0.9603960396039604
; > (search-prime 100000000000000 3)
; Search from :100000000000000
; find prime:100000000000031
; find prime:100000000000067
; find prime:100000000000097
; cost total time:97.0 every test cost time:0.9797979797979798
; > (search-prime 1000000000000000 3)
; Search from :1000000000000000
; find prime:1000000000000037
; find prime:1000000000000091
; find prime:1000000000000159
; cost total time:140.0 every test cost time:0.8757763975155279
; > (search-prime 10000000000000000 3)
; Search from :10000000000000000
; find prime:10000000000000061
; find prime:10000000000000069
; find prime:10000000000000079
; cost total time:97.0 every test cost time:1.1975308641975309
; > (search-prime 100000000000000000 3)
; Search from :100000000000000000
; find prime:100000000000000003
; find prime:100000000000000013
; find prime:100000000000000019
; cost total time:57.0 every test cost time:2.7142857142857144
; > (search-prime 1000000000000000000 3)
; Search from :1000000000000000000
; find prime:1000000000000000003
; find prime:1000000000000000009
; find prime:1000000000000000031

; 计算速度太快，看不出效率变化

; exp1.25
; (define (expmod base exp m)
;   (cond ((= exp 0) 1)
;         ((even? exp)
;          (remainder (square (expmod base (/ exp 2) m))
;                     m))
;         (else
;          (remainder (* base (expmod base (- exp 1) m))
;                     m))))    

(define (fast-expt b n) 
  (cond ((= n 0) 1) 
        ((even? n) (square (fast-expt b (/ n 2)))) 
        (else (* b (fast-expt b (- n 1))))))

(define (expmod base exp m)
   (remainder (fast-expt base exp) m))

; exp1.26
(define (expmod base exp m)
   (cond ((= exp 0) 1)
         ((even? exp)
          (remainder (* (expmod base (/ exp 2) m)
                        (expmod base (/ exp 2) m))
                     m))
         (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))

; > (search-prime 1000 3)
; Search from :1000
; find prime:1009
; find prime:1013
; find prime:1019
; cost total time:31.0 every test cost time:1.4761904761904763
; > (search-prime 10000 3)
; Search from :10000
; find prime:10007
; find prime:10009
; find prime:10037
; cost total time:609.0 every test cost time:15.615384615384615
; > (search-prime 100000 3)
; Search from :100000
; find prime:100003
; find prime:100019
; find prime:100043
; cost total time:7685.0 every test cost time:170.77777777777777

; expmod相当于计算了2次，运算时间明显提升

; exp1.27
(define (my-fermat-test a n)
    (cond 
      ((= a n) #t)
      ((= (expmod a n n) a) (my-fermat-test (+ a 1) n))
      (else #f))
    )

(define (pass-fermat-test n)
  (my-fermat-test 1 n)
)

; > (pass-fermat-test 561)
; #t
; > (pass-fermat-test 1105)
; #t
; > (pass-fermat-test 1729)
; #t
; > (pass-fermat-test 2465)
; #t
; > (pass-fermat-test 2821)
; #t
; > (pass-fermat-test 6601)
; #t

; exp1.28
(define (square x) (* x x))
(define (expmod base exp m)
    (cond ((= exp 0)
            1)
          ((nontrivial-square-root? base m)                 ; 新增
            0)                                              ;
          ((even? exp)
            (remainder (square (expmod base (/ exp 2) m))
                       m))
          (else
            (remainder (* base (expmod base (- exp 1) m))
                       m))))
(define (nontrivial-square-root? a n)
    (and (not (= a 1))
         (not (= a (- n 1)))
         (= 1 (remainder (square a) n))))
(define (non-zero-random n)
    (let ((r (random n)))
        (if (not (= r 0))
            r
            (non-zero-random n))))
(define (Miller-Rabin-test n)
    (let ((times (ceiling (/ n 2))))
        (test-iter n times)))

(define (test-iter n times)
    (cond ((= times 0)
            #t)
          ((= (expmod (non-zero-random n) (- n 1) n)
              1)
            (test-iter n (- times 1)))
          (else
            #f)))
; book-1.3.1
(define (cube x) (* x x x))

(define (sum-integers a b) 
  (if (> a b)
    0
    (+ a (sum-integers (+ a 1) b))))
(define (sum-cubes a b) 
  (if (> a b)
    0
    (+ (cube a) (sum-cubes (+ a 1) b))))
(define (pi-sum a b) 
  (if (> a b)
    0
    (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a) (sum term (next a) next b))))
(define (inc n) (+ n 1))
(define (sum-cube a b) 
  (sum cube a inc b))

(define (identity x) x)
(define (sum-integers a b) 
  (sum identity a inc b))

(define (pi-sum a b) 
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b)
  )

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx)
  )

