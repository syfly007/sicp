(define (f1 n)
  (cond ((< n 3) n)
        (else (+ (f1 (- n 1))
                 (* 2 (f1 (- n 2)))
                 (* 3 (f1 (- n 3)))))))

(define (f2 n)
  (f-iter 2 1 0 n))
(define (f-iter a b c count)
  (cond ((= count 0) c)
        (else (f-iter
                (+ (* 3 c) (* 2 b) a)
                a
                b
                (- count 1)
               ))))