(define nil '())
(define null '())
(define true #t)
(define false #f)

(define (false? s) (if (s)
						true
						false))

(define (square x) (* x x))

(define (get-index arr data) 
	(define (iter arr data n) 
		(cond ((null? arr) -1) 
			  ((eq? (car arr) data) n) 
			  (else (iter (cdr arr) data (+ n 1)))))
	(iter arr data 0)
)