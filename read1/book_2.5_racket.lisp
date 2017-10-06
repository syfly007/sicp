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
)(define nil '())
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

;;; CODE FROM OTHER CHAPTERS OF STRUCTURE AND INTERPRETATION OF
;;;  COMPUTER PROGRAMS NEEDED BY CHAPTER 2

;;;from chapter 1
(define (square x) (* x x))

;;;from section 1.2.5, for Section 2.1.1
(define (gcd1 a b)
  (if (= b 0)
      a
      (gcd1 b (remainder a b))))

;;;from section 1.2.2, for Section 2.2.3
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

;;; ***not in book, but needed for code before quote is introduced*** 
(define nil '())

;;;-----------
;;;from section 3.3.3 for section 2.4.3
;;; to support operation/type table for data-directed dispatch

(define (assoc1 key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc1 key (cdr records)))))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc1 key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc1 key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc1 key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc1 key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(define coercion-table (make-table))
(define get-coercion (coercion-table 'lookup-proc))
(define put-coercion (coercion-table 'insert-proc!))

;;;-----------

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part1 z) (car z))
  (define (imag-part1 z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude1 z)
    (sqrt (+ (square (real-part1 z))
             (square (imag-part1 z)))))
  (define (angle1 z)
    (atan (imag-part1 z) (real-part1 z)))
  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a))))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part1 '(rectangular) real-part1)
  (put 'imag-part1 '(rectangular) imag-part1)
  (put 'magnitude1 '(rectangular) magnitude1)
  (put 'angle1 '(rectangular) angle1)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

  (define (install-polar-package)
  ;; internal procedures
  (define (magnitude1 z) (car z))
  (define (angle1 z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part1 z)
    (* (magnitude1 z) (cos (angle1 z))))
  (define (imag-part1 z)
    (* (magnitude1 z) (sin (angle1 z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part1 '(polar) real-part1)
  (put 'imag-part1 '(polar) imag-part1)
  (put 'magnitude1 '(polar) magnitude1)
  (put 'angle1 '(polar) angle1)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

  (define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))

(define (real-part1 z) (apply-generic 'real-part1 z))
(define (imag-part1 z) (apply-generic 'imag-part1 z))
(define (magnitude1 z) (apply-generic 'magnitude1 z))
(define (angle1 z) (apply-generic 'angle1 z))


;; Constructors for complex numbers

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))


(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
  (define (equ? x y) (eq? x y))
  (define (=zero? x) (eq? x 0))
  (define (tag x)
    (attach-tag 'scheme-number x))
  (define (raise x) 
    (make-rational (contents x) 1))
  (put 'equ? '(scheme-number scheme-number) equ?)
  (put '=zero? '(scheme-number) =zero?)
  (put 'project 'scheme-number 
       (lambda (x) x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'exp '(scheme-number scheme-number)
     (lambda (x y) (tag (expt x y))))
  (put 'raise 'scheme-number raise)

  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd1 n d)))
      (cons (/ n g) (/ d g))))
  (define (raise x) 
    (make-real-number (/ (numer (contents x)) (denom (contents x)))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (equ? x y)
    (and (eq? (numer x) (numer y)) 
         (eq? (denom x) (denom y))))
  (define (=zero? x) (eq? 0 (numer x)))

  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'equ? '(rational rational) equ?)
  (put '=zero? '(rational) =zero?)
  (put 'project 'rational 
        (lambda (x) (make-scheme-number (round (/ (numer (contents x)) (denom (contents x)))))))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'raise 'rational raise)
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-real-number-package)
  (define (equ? x y) (eq? x y))
  (define (=zero? x) (eq? x 0))
  (define (tag x)
    (attach-tag 'real-number x))
  (define (raise x) 
    (make-complex-from-real-imag 
        (contents x)
        0))
  (put 'equ? '(real-number real-number) equ?)
  (put '=zero? '(real-number) =zero?)
  (put 'project 'real-number 
       (lambda (x) (make-rational (round x) 1)))
  (put 'add '(real-number real-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real-number real-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real-number real-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real-number real-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'real-number
       (lambda (x) (* 1.0 (tag x))))
  (put 'exp '(real-number real-number)
     (lambda (x y) (tag (expt x y))))
  (put 'raise 'real-number raise)

  'done)

(define (make-real-number n)
  ((get 'make 'real-number) n))

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part1 z1) (real-part1 z2))
                         (+ (imag-part1 z1) (imag-part1 z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part1 z1) (real-part1 z2))
                         (- (imag-part1 z1) (imag-part1 z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude1 z1) (magnitude1 z2))
                       (+ (angle1 z1) (angle1 z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude1 z1) (magnitude1 z2))
                       (- (angle1 z1) (angle1 z2))))
  (define (equ? x y) 
    (and (eq? (real-part1 x) (real-part1 y)) 
         (eq? (imag-part1 x) (imag-part1 y))))
  (define (=zero? x) 
    (and (eq? (real-part1 x) 0) 
         (eq? (imag-part1 x) 0)))

  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'equ? '(complex complex) equ?)
  (put '=zero? '(complex) =zero?)
  (put 'project 'complex 
       (lambda (x) (make-real-number (real-part1 (contents x)))))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))

  (put 'real-part1 '(complex) real-part1)
  (put 'imag-part1 '(complex) imag-part1)
  (put 'magnitude1 '(complex) magnitude1)
  (put 'angle1 '(complex) angle1)

  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))       

; exp2.77
; 执行(magnitude1 z)时，会先取出通用型的magnitude1，然后根据 rectangular 或 polar 类型取出对应的magnitude1操作。
; (apply-generic)被调了2次，第一次是分派(put 'magnitude1 '(complex) magnitude1)，第二次是分派(put 'magnitude1 '(rectangular) magnitude1)。

; exp2.78
(define (attach-tag type-tag contents)
  (cond ((number? contents) contents) 
        (else (cons type-tag contents))))

(define (type-tag datum)
  (cond ((number? datum) 
          (if (eq? datum (round datum))
            'scheme-number
            'real-number))
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum -- TYPE-TAG" datum)))
  )

(define (contents datum)
  (cond ((number? datum) datum) 
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum -- CONTENTS" datum))))

; 实际上不需要修改scheme包

; (define (install-scheme-number-package)
;   (put 'add '(scheme-number scheme-number)
;        (lambda (x y) (+ x y)))
;   (put 'sub '(scheme-number scheme-number)
;        (lambda (x y) (- x y)))
;   (put 'mul '(scheme-number scheme-number)
;        (lambda (x y) (* x y)))
;   (put 'div '(scheme-number scheme-number)
;        (lambda (x y) (/ x y)))
;   (put 'make 'scheme-number
;        (lambda (x) (x)))
;   'done)

; (define (make-scheme-number n)
;   ((get 'make 'scheme-number) n))

; exp2.79 2.80
(define (equ? x y) (apply-generic 'equ? x y) )
(define (=zero? x) (apply-generic '=zero? x))

; book2.5.2
;; to be included in the complex package
;: (define (add-complex-to-schemenum z x)
;:   (make-from-real-imag (+ (real-part1 z) x)
;:                        (imag-part1 z)))
;: 
;: (put 'add '(complex scheme-number)
;:      (lambda (z x) (tag (add-complex-to-schemenum z x))))


;; Coercion

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(put-coercion 'scheme-number 'complex scheme-number->complex)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else
                         (error "No method for these types"
                                (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))

; exp2.81
(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)
(put-coercion 'scheme-number 'scheme-number
               scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)

(define (exp1 x y) (apply-generic 'exp x y))

; a)执行一次强制操作，然后再进行一次apply-generic操作；
;   对2个复数调exp方法会一直找不到对应的操作，并无限递归下去。
; b)没有纠正，反而会造成无限递归；
; c)
; (define (apply-generic op . args)
;   (let ((type-tags (map type-tag args)))
;     (let ((proc (get op type-tags)))
;       (if proc
;           (apply proc (map contents args))
;           (if (and (= (length args) 2) 
;               (not eq? (car args) (cadr args)))       ;增加判断
;               (let ((type1 (car type-tags))
;                     (type2 (cadr type-tags))
;                     (a1 (car args))
;                     (a2 (cadr args)))
;                 (let ((t1->t2 (get-coercion type1 type2))
;                       (t2->t1 (get-coercion type2 type1)))
;                   (cond (t1->t2
;                          (apply-generic op (t1->t2 a1) a2))
;                         (t2->t1
;                          (apply-generic op a1 (t2->t1 a2)))
;                         (else
;                          (error "No method for these types"
;                                 (list op type-tags))))))
;               (error "No method for these types"
;                      (list op type-tags)))))))

; 这么改更清楚
(define (apply-generic op . args)
    (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
            (if proc
                (apply proc (map contents args))
                (if (= (length args) 2)
                    (let ((type1 (car type-tags))
                          (type2 (cadr type-tags))
                          (a1 (car args))
                          (a2 (cadr args)))
                        (if (equal? type1 type2)                                    ; 新增
                            (error "No method for these types" (list op type-tags)) ; 
                            (let ((t1->t2 (get-coercion type1 type2))
                                  (t2->t1 (get-coercion type2 type1)))
                                (cond (t1->t2
                                        (apply-generic op (t1->t2 a1) a2))
                                      (t2->t1
                                        (apply-generic op a1 (t2->t1 a2)))
                                      (else
                                        (error "No method for these types"
                                                (list op type-tags)))))))
                    (error "No method for these types"
                            (list op type-tags)))))))


; exp2.82
; 题目中强制策略，只能够获取所有参数类型都一样的方法，而对参数类型并不完全相同的方法无能为力。

; exp2.83
(define (raise x) 
  ((get 'raise (type-tag x)) x))

; exp2.84
(define (get-index arr data) 
  (define (iter arr data n) 
    (cond ((null? arr) -1) 
        ((eq? (car arr) data) n) 
        (else (iter (cdr arr) data (+ n 1)))))
  (iter arr data 0)
)

(define typeTower '(scheme-number rational real-number complex))
(define (higherType type1 type2) 
  (> (get-index typeTower type1) (get-index typeTower type2)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
            (if proc
                (apply proc (map contents args))
                (if (= (length args) 2)
                    (let ((type1 (car type-tags))
                          (type2 (cadr type-tags))
                          (a1 (car args))
                          (a2 (cadr args)))
                        (cond ((equal? type1 type2) 
                                (error "No method for these types" (list op type-tags))) ; 
                              ((higherType type1 type2) 
                                (apply-generic op a1 (raise a2)))
                              (else 
                                (apply-generic op (raise a1) a2))
                          ))
                    (error "No method for these types"
                            (list op type-tags)))))))

; exp2.85
(define (project x) 
  ((get 'project (type-tag x)) x))
(define (drop x) 
  (if (not (number? x)) ;注意equ?等函数也会调用apply-generic,从而调用drop，所以必须排除数值类型以外的值调用drop
    x
    (if (eq? (get-index typeTower (type-tag x)) 0)
      x
      (let ((dropNum (project x)))
        (cond ((equ? x (raise dropNum)) 
                (drop dropNum)) 
              (else x))))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
            (if proc
                (drop (apply proc (map contents args)))
                (if (= (length args) 2)
                    (let ((type1 (car type-tags))
                          (type2 (cadr type-tags))
                          (a1 (car args))
                          (a2 (cadr args)))
                        (cond ((equal? type1 type2) 
                                (error "No method for these types" (list op type-tags))) ; 
                              ((higherType type1 type2) 
                                (apply-generic op a1 (raise a2)))
                              (else 
                                (apply-generic op (raise a1) a2))
                          ))
                    (error "No method for these types"
                            (list op type-tags)))))))

; exp2.86
; 在复数内部做处理时，可以通过raise操作转为实数

; book.2.5.3
;; *incomplete* skeleton of package
(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  ;;[procedures same-variable? and variable? from section 2.3.2]
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

  ;; representation of terms and term lists
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))

  ;;[procedures adjoin-term ... coeff from text below]
  ;; Representing term lists

  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))

  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))

  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))
  ; exp2.88
  (define (sub-poly p1 p2)
    (add-poly p1 (negative-terms p2)))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))
    ; exp2.87
  (define (=zero?-termlist t)
    (if (empty-termlist? t)
        #t
        (and (=zero? (coeff (first-term t)))
             (=zero?-termlist (rest-terms t)))))
  (put '=zero? '(polynomial)
       (lambda (p) (=zero?-termlist (term-list p))))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'variable 'polynomial
       (lambda (p) (variable p)))
  (put 'add '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial) 
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  ; exp2.88
  (put 'sub '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly p1 (contents (negative (tag p2)))))))

  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)

;; Constructor
(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(install-scheme-number-package)
(install-rational-package)
(install-real-number-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)

(define z1 (make-complex-from-real-imag 1 1))

(raise (raise 3))
(add z1 (make-scheme-number 3))


(install-polynomial-package)

(define a (make-polynomial 'x '((5 1) (4 2) (2 3) (1 -2) (0 -5))))

  ; exp2.87
; add =zero? in install-polynomial-package
(=zero? (make-polynomial 'x '()))
; #t
(=zero? (make-polynomial 'x (list (list 0 0))))
; #t
(=zero? (make-polynomial 'x (list (list 1 0) (list 2 0))))
; #t
(=zero? (make-polynomial 'x (list (list 1 1))))
; #f

; exp2.88
(define (install-negative-package)
  (put 'negative '(scheme-number) (lambda (x) (mul -1 x))) 
  (put 'negative '(rational) 
    (lambda (x) (mul (attach-tag 'rational x) (make-rational -1 1)))) 
  (put 'negative '(real-number) (lambda (x) (mul -1.0 x))) 
  (put 'negative '(complex) 
    (lambda (x) (make-complex-from-real-imag (* -1 (real-part1 x)) (* -1 (imag-part1 x))))) 
  (put 'negative '(polynomial) 
    (lambda (p) (mul (attach-tag 'polynomial p) (make-polynomial (variable p) '((0 -1))))))
    'done)

(define (variable p)
  ((get 'variable 'polynomial) p))

(install-negative-package)

(define (negative x) (apply-generic 'negative x))

(negative (make-scheme-number 10))
(negative (make-rational 3 4))
(negative (make-real-number -7.5))
(negative (make-complex-from-real-imag 3 -5))
(negative (make-polynomial 'x '((5 1) (4 2) (2 3) (1 -2) (0 -5))))

(sub (make-polynomial 'x '((5 1) (4 2) (2 3) (1 -2) (0 -5))) (make-polynomial 'x '((5 1) (4 2) (2 3) (1 -2) (0 -5))))
(sub (make-polynomial 'x '((5 1) (4 2) (2 3) (1 -2) (0 -5))) (make-polynomial 'x '((100 2) (5 2) (2 3) (1 -3) (0 -5))))

; exp2.89
