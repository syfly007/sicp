; exp2.36
(define (accumulate-n op init seqs)
   (if (null? (car seqs))
       nil
       (cons (accumulate op init (map car seqs))
             (accumulate-n op init (map cdr seqs)))))

(accumulate-n + 0 (list(list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

; exp2.37
(define (dot-product v w)
   (accumulate + 0 (map * v w)))
(dot-product (list 1 2 3) (list 4 5 6))


(define (matrix-*-vector m v)
   (map (lambda (x) (dot-product x v)) m))
(matrix-*-vector 	(list 	(list 1 2 3) 
							(list 1 2 3) 

							(list 1 2 3)) 

					(list 4 5 6))

(define (transpose mat)
   (accumulate-n cons nil mat))
(transpose (list (list 1 2 3) 
				 (list 1 2 3)))


(define (matrix-*-matrix m n)
   (let ((cols (transpose n)))
     (map (lambda (mi) (matrix-*-vector cols mi)) m)))
(matrix-*-matrix (list 	(list 1 2 3) 
						(list 1 2 3)) 
				 (list 	(list 4 5 6) 
				 		(list 4 5 6) 
				 		(list 4 5 6)))

; exp2.38
(define (fold-left op initial sequence)
   (define (iter result rest)
     (if (null? rest)
         result
         (iter (op result (car rest))
               (cdr rest))))
   (iter initial sequence))

(define (fold-right op initial sequence)
   (if (null? sequence)
       initial
       (op (car sequence)
           (fold-right op initial (cdr sequence)))))　

(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))
(fold-right list nil (list 1 2 3))
(fold-left list nil (list 1 2 3))

; 3/2
; 1/6
; (1 (2 (3 ())))
; (((() 1) 2) 3)

; 满足交换律
; 错，应该是满足结合律

; exp2.39
(define (reverse sequence)
   (fold-right (lambda (x y) (append y (list x))) nil sequence))


(define (reverse sequence)
   (fold-left (lambda (x y) (append (list y) x )) nil sequence))
(reverse (list 1 2 3))

; 更简
(define (reverse sequence)
   (fold-left (lambda (x y) (cons y x )) nil sequence))

; book2.3.3
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;; decoding
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

;; sets

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

; (make-leaf-set (list '(A 4) '(B 2) '(C 1) '(D 1)))

; exp2.67
(define sample-tree
 (make-code-tree (make-leaf 'A 4)
                 (make-code-tree
                  (make-leaf 'B 2)
                  (make-code-tree (make-leaf 'D 1)
                                  (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

; (decode sample-message sample-tree)
; (A D A B B C A)

; exp2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

;有问题，运行2.70报错
(define (encode-symbol message tree) 
    (cond ((and (leaf? (left-branch tree)) 
                (eq? (symbol-leaf
                        (left-branch tree)) 
                    message) )
            (list 0)) 
          ((and (leaf? (right-branch tree))
                (eq? (symbol-leaf 
                        (right-branch tree)) 
                      message)) 
            (list 1))
          ((and (not (leaf? (left-branch tree))) 
                (not (null? (encode-symbol message (left-branch tree)))))
            (append (list 0) (encode-symbol message (left-branch tree)))) 
          ((and (not (leaf? (right-branch tree))) 
                (not (null? (encode-symbol message (right-branch tree)))))
            (append (list 1) (encode-symbol message (right-branch tree))))
          (else (error "Can't find this symbol in tree:" message))

          ))




; (encode '(A D A B B C A) sample-tree)
; (0 1 1 0 0 1 0 1 0 1 1 1 0)

; exp2.69

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

; (define (successive-merge tree) 
;   (cond ((and (eq? 1 (length tree) (leaf? tree)) 
;           (make-code-tree (cadr tree) (car tree) )) 
;         ((eq? 1 (length tree))
;           (car tree)) 
;         (else (successive-merge (cons (make-code-tree (cadr tree) (car tree) ) (cddr tree))))))



; 参考答案，正确，我的答案没有考虑每次归并后重新用adjoin-set对序列按权重排序
(define (successive-merge ordered-set)
    (cond ((= 0 (length ordered-set))
            '())
          ((= 1 (length ordered-set))
            (car ordered-set))
          (else
            (let ((new-sub-tree (make-code-tree (car ordered-set)
                                                (cadr ordered-set)))
                  (remained-ordered-set (cddr ordered-set)))
                (successive-merge (adjoin-set new-sub-tree remained-ordered-set))))))

(generate-huffman-tree (list '(A 4) '(B 2) '(C 1) '(D 1)))

; exp2.70
(define huffTree (generate-huffman-tree (list 
                                          '(A 2)
                                          '(NA 16)
                                          '(BOOM 1)
                                          '(SHA 3)
                                          '(GET 2)
                                          '(YIP 9)
                                          '(JOB 2)
                                          '(WAH 1)
                                          )))


(encode '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP) huffTree)


; book2.4.1
(make-from-real-imag (real-part z) (imag-part z))
(make-from-mag-ang (magnitude z) (angle z))

(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))

;; Ben (rectangular)
(define (real-part z) (car z))                     
(define (imag-part z) (cdr z))
(define (magnitude z) (sqrt (+ (square (real-part z)) (square (imag-part z)))))
(define (angle z) (atan (imag-part z) (real-part z)))
(define (make-from-real-imag x y) (cons x y))
(define (make-from-mag-ang r a) (cons (* r (cos a)) (* r (sin a))))

;; Alyssa(angle)
(define (real-part z) (* magnitude z) (cos (angle z)))
(define (imag-part z) (* magnitude z) (sin (angle z)))
(define (magnitude z) (car z))
(define (angle z) (cdr z))
(define (make-from-real-imag x y) 
  (cons (sqrt (+ (square x) (square y))) 
        (atan y x)))
(define (make-from-mag-ang r a) (cons r a))

; book2.4.2
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

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z)
  (eq? (type-tag z) 'polar))

;; Ben (rectangular)

(define (real-part-rectangular z) (car z))

(define (imag-part-rectangular z) (cdr z))

(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))

(define (angle-rectangular z)
  (atan (imag-part-rectangular z)
        (real-part-rectangular z)))

(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))

(define (make-from-mag-ang-rectangular r a) 
  (attach-tag 'rectangular
              (cons (* r (cos a)) (* r (sin a)))))

;; Alyssa (polar)

(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))

(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))

(define (magnitude-polar z) (car z))

(define (angle-polar z) (cdr z))

(define (make-from-real-imag-polar x y) 
  (attach-tag 'polar
               (cons (sqrt (+ (square x) (square y)))
                     (atan y x))))

(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))

;; Generic selectors

(define (real-part z)
  (cond ((rectangular? z) 
         (real-part-rectangular (contents z)))
        ((polar? z)
         (real-part-polar (contents z)))
        (else (error "Unknown type -- REAL-PART" z))))

(define (imag-part z)
  (cond ((rectangular? z)
         (imag-part-rectangular (contents z)))
        ((polar? z)
         (imag-part-polar (contents z)))
        (else (error "Unknown type -- IMAG-PART" z))))

(define (magnitude z)
  (cond ((rectangular? z)
         (magnitude-rectangular (contents z)))
        ((polar? z)
         (magnitude-polar (contents z)))
        (else (error "Unknown type -- MAGNITUDE" z))))

(define (angle z)
  (cond ((rectangular? z)
         (angle-rectangular (contents z)))
        ((polar? z)
         (angle-polar (contents z)))
        (else (error "Unknown type -- ANGLE" z))))

;; same as before
(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

;; Constructors for complex numbers

(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))

(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))

; book2.4.3
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a))))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

  (define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
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
