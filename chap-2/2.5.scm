(define (add x y)
  (apply-generic 'add x y)
)
(define (sub x y)
  (apply-generic 'sub x y)
)
(define (mul x y)
  (apply-generic 'mul x y)
)
(define (div x y)
  (apply-generic 'div x y)
)
(define (attach-tag type-tag contents)
  (cons type-tag contents)
)
;; 使用包处理参数 基本操作存表
;; scheme-number常规数包
(define (install-scheme-number-package)
  ;;声明 tag x
  (define (tag x)
    ;; 打标
    (attach-tag 'scheme-number x))
  (define scheme-numer-tag '(scheme-numer scheme-number))
  (put 'add scheme-numer-tag (lambda (x y) (tag (+ x y))))
  (put 'sub scheme-numer-tag (lambda (x y) (tag (- x y))))
  (put 'mul scheme-numer-tag (lambda (x y) (tag (* x y))))
  (put 'div scheme-numer-tag(lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number (lambda (x) (tag x)))
  'done
)
;; 取出scheme-number
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

;; 无理数包
(define (install-rational-package)
  (define (number x) (car x))

  (define (denom x ) (cdr x))

  (define (make-rat n d)
    (let
      ((g (gcd n d)))
      (cons (/ n g) (/ d g))))

  (define (add-rat x y)
    (make-rat
      (+
        (* (number x) (denom y))
        (* (number y) (denom x)))
      (* (denom x) (denom y))))

  (define (sub-rat x y)
    (make-rat
      (-
        (* (number x) (denom y))
        (* (number y) (denom x)))
      (* (denom x) (denom y))))

  (define (mul-rat x y)
    (make-rat
      (* (number x) (number y))
      (* (denom x) (denom y))))

  (define (div-rat x y)
    (make-rat
      (* (number x) (denom y))
      (* (denom x) (number y))))
;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (define rational-tag '(rational rational))
  (put 'add rational-tag (lambda (x y) (tag (add-rat x y))))
  (put 'sub rational-tag (lambda (x y) (tag (sub-rat x y))))
  (put 'mul rational-tag (lambda (x y) (tag (mul-rat x y))))
  (put 'div rational-tag (lambda (x y) (tag (div-rat x y))))
  'done
)
;; 取无理数
(define (make-rational n d)
  ((get 'make 'rational) n d))

;; 复数处理包 complex

(define (install-complex-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  (define (add-complex z1 z3)
    (make-from-real-imag
      (+ (real-part z1) (real-part z2))
      (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag
      (- (real-part z1) (real-part z2))
      (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang
      (* (magnitude z1) (magnitude z2))
      (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang
      (/ (magnitude z1) (magnitude z2))
      (- (angle z1) (angle z2))))
  ;;interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (define tag-name '(complex complex))
  (put 'add tag-name (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub tag-name (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul tag-name (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div tag-name (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
    (lambda (z1 z2) (tag (make-from-real-imag x y)))
  )
  (put 'make-from-mag-and 'complex
    (lambda (r a) (tag (make-from-mag-ang r a)))
  )
  'done
)

;; 角度和坐标定义
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y)
)
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a)
)

;; 强制的概念，把一种数据类型强制转换为另一种数据类型，使得两包想通

;; scheme-number -> complex-number 普通数看为虚部为0的复数

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(put-coercion 'scheme-number 'complex scheme-number->complex)
;; 根据配装，我们重新设计一下`apply-generic`
(define (apply-generic op , args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (if (= (length args) 2)
          (let ((type1 (car type-tags))
            (type2 (cadr type-tags))
            (a1 (car args))
            (a2 (cadr args)))
            (let ((t1->t2 (get-coercion type1 typ2))
              (t2->t1 (get-coercion type2 typ1))))
              (cond
                (t1->t2
                  (apply-generic op (t1->t2 a1) a2))
                (t2->t1
                  (apply-generic op a1 (t2->t1 a2)))))))))))

;; 实例 符号代数
;; 多项式 polynomial作为标志
(define (install-polynomial-package)
  ;; internal procedures
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  ;; 未知数项加减乘除
  (define (add-poly p1 p2)
    ;; same-variable? 取2.3节是否为一个变量 p1 p2 如果为一个未知数
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
        (add-terms (term-list p1) (term-list p2)))
      (display 'error polys not in same var')
    )
  )
  (define (mul-poly p1 p2)
    ;; same-variable? 取2.3节是否为一个变量 p1 p2 如果为一个未知数
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
        (mul-terms (term-list p1) (term-list p2)))
      (display 'error polys not in same var')
    )
  )
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (define symbol '(polynomial polynomial))
  (put 'add symbol (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul symbol (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial (lambda (var terms) (tag (make-poly var terms))))
  'done
)

;; 假定 the-empty-termlist 空列表
;; adjoin-term 新项加入列表
;; empty-terlist? 检查一个项表是否为空
;; 求和多项式
(define (add-terms L1 L2)
  (cond
    ((empty-termlist? L1) L2)
    ((empty-termlist? L2) L1)
    (else
      (let
        ((t1 (first-term L1))
          (t2 (first-term L2)))
        (
          (cond 
            ((> (order t1) (order t2))
              (addjoin-term t1 (add-terms (rest-terms L1) L2)))
            ((< (order t1) (order t2))
              (addjoin-term t2 (add-terms (rest-terms L1) L2)))
            (else
              (adion-term
                (make-term (order t1) (add (coeff t1) (coeff t2)))
                (add-term (rest-terms L1) (rest-terms L2))
              )
            )
          )
        )))
  )
)
;; 乘起两个项表
(define (mul-terms L1 L2)
  (if (empty-termlist? L1)
    (the-empty-termlist)
    (add-terms
      (mul-term-by-all-terms (first-term L1) L2)
      (mul-terms (rest-terms L1) L2)
    )
  )
)
(define (mul-term-by-all-terms t1 L)
  (if (empty-termlist? L)
    (the-empty-termlist)
    (let 
    ((t2 (first-term L))) 
    (adjoin-term
      (make-term (+ (order t1) (order t2)) (mul (coeff t1) (coeff t2)))
      (mul-term-by-all-terms t1 (rest-terms L))
    ))
  )
)