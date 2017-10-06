(define (p) (p))

(define (test x y) 
  (if (= x 0) 
      0 
      y))
;(test 0 (p))

;What behavior will Ben observe with an interpreter that uses applicative-order evaluation? 
;program can not be stoped

 ;What behavior will he observe with an interpreter that uses normal-order evaluation? 
;return 0