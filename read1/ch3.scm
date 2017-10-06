;;;;SECTION 3.1

;;;SECTION 3.1.1

;: (withdraw 25)
;: (withdraw 25)
;: (withdraw 60)
;: (withdraw 15)

(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))


(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))


(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))


;: (define W1 (make-withdraw 100))
;: (define W2 (make-withdraw 100))
;: (W1 50)
;: (W2 70)
;: (W2 40)
;: (W1 40)


(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

;: (define acc (make-account 100))

;: ((acc 'withdraw) 50)
;: ((acc 'withdraw) 60)
;: ((acc 'deposit) 40)
;: ((acc 'withdraw) 60)

;: (define acc2 (make-account 100))

; exp3.1
(define (make-accumulator n)
  (lambda (x) (begin (set! n (+ n x)) n)))

(define A (make-accumulator 5))
(A 10)
(A 10)

; exp3.2
(define (make-monitored fun)
  (define n 0)
  (define (dispatch m) 
    (cond ((eq? m 'how-many-calls?) n) 
          ((eq? m 'reset-count) (begin (set! n 0) 0))
          (else (begin (set! n (+ n 1)) (fun m))))) 
  dispatch)

(define s (make-monitored sqrt))
(s 100)
(s 'how-many-calls?)

; exp3.3
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch input-password m)
    (if (not (eq? input-password password))
      (error "Incorrect password")
      (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m)))))
  dispatch)

(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40)
((acc 'some-other-password 'deposit) 50)

; exp3.4
(define (make-account balance password)
  (define MAX-INPUT-NUM 7)
  (define error-input-num 0)

  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  ; 没有n，会有找不到参数的报错
  (define (call-the-cops n) (display "call-the-cops!"))
  (define (error-input n) (display "Incorrect password"))

  (define (verify input-password m) 
    (if (not (eq? input-password password))
      (begin 
        (set! error-input-num (+ error-input-num 1)) 
        (if (>= error-input-num MAX-INPUT-NUM)
          call-the-cops
          error-input))
      (begin (set! error-input-num 0) (dispatch m))))

  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  verify)

(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40)
((acc 'some-other-password 'deposit) 50)

; book3.1.2
;; *following uses rand-update -- see ch3support.scm
;; *also must set random-init to some value
(define random-init 7)      ;**not in book**
(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))


(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
   (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

;; second version (no assignment)
(define (estimate-pi trials)
  (sqrt (/ 6 (random-gcd-test trials random-init))))

(define (random-gcd-test trials initial-x)
  (define (iter trials-remaining trials-passed x)
    (let ((x1 (rand-update x)))
      (let ((x2 (rand-update x1)))
        (cond ((= trials-remaining 0)   
               (/ trials-passed trials))
              ((= (gcd x1 x2) 1)
               (iter (- trials-remaining 1)
                     (+ trials-passed 1)
                     x2))
              (else
               (iter (- trials-remaining 1)
                     trials-passed
                     x2))))))
  (iter trials 0 initial-x))

; exp3.5
(define (P x y)
 (>= (square 3) (+ (square (- x 5)) (square (- y 7)))))
(define test-fun '())
(define x1-value 0)
(define x2-value 0)
(define y1-value 0)
(define y2-value 0)
(define RANGE 10000)


(define (estimate-integral fun x1 x2 y1 y2 trials)
  (begin 
    (set! x1-value x1)
    (set! x2-value x2)
    (set! y1-value y1)
    (set! y2-value y2)
    (set! test-fun fun)
    (/ (* (* (abs (- x1 x2)) (abs (- y1 y2))) 
          (monte-carlo trials p-test)) 
    (square 3.0))))

(define (p-test) 
  (test-fun (real-random-range x1-value x2-value) (real-random-range y1-value y2-value)))

(define (real-random-range low high)
  (/ (random-in-range (* low RANGE) (* high RANGE)) (* 1.0 RANGE)))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(estimate-integral P 2 8 4 10 10000000)

; other's solution
 (define (P x y) 
   (< (+ (expt (- x 5) 2) 
         (expt (- y 7) 2)) 
      (expt 3 2))) 
 (define (estimate-integral P x1 x2 y1 y2 trials) 
   (define (experiment) 
     (P (random-in-range x1 x2) 
        (random-in-range y1 y2))) 
   (monte-carlo trials experiment)) 
 (define pi-approx 
   (/ (* (estimate-integral P 2.0 8.0 4.0 10.0 10000) 36) 
      9.0)) 
 pi-approx 

; exp3.6
(define random-init 0)      ;**not in book**
(define (rand-update x) (+ x 1)) ; A not-very-evolved PNRG 

; (define rand
;   (let ((x random-init))
;     ((define (generate) 
;       (begin 
;         (set! x (rand-update x))
;         x)) 
;     (define (reset value) 
;       (set! x value))
;     (define (dispatch m) 
;       (cond 
;         ((eq? m 'generate) (generate))
;         ((eq? m 'reset) reset)
;         (else (display "Error Input!")))))
;     dispatch))

; other's solution
(define rand 
   (let ((x random-init)) 
     (define (dispatch message) 
       (cond ((eq? message 'generate) 
               (begin (set! x (rand-update x)) 
                      x)) 
             ((eq? message 'reset) 
               (lambda (new-value) (set! x new-value))))) 
     dispatch)) 

(rand 'generate)