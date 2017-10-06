(define (sum-of-squares x y) 
	(+ (square x) (square y)))

(define (f a)
	(sum-of-squares (+ a 1) (* a 2)))

; 1.3
(define (max2 a b c)
	(
		cond ((< a b)
			(cond 	((< c a)
					(+ a b)) 
					(else (+ b c))
			))
			(else (cond ( (< c b)
					(+ a b) )
					(else (+ a c) )
			))
		
	)
)

; 1.4
(define (a-plus-abs-b a b)
	( (if (< b 0)
	    -
	    +)
	  a
	  b
		)
 	)

;1.5
(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))

(define (sqrt x)
	( the y (
		and (y > 0)
		(= (square y) x))))

