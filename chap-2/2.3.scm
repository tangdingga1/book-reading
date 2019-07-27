;; 是否为符号
(define (variable? x) (symbol? x))
;; 符号是否相等
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2))
)
;; 构造和式和乘式
(define (make-sum a1 a2) (list '+ a1 a2))
(define (make-product m1 m2) (list '* m1 m2))
;; 是否为和式(第一个是否为+的符号)
(define (sum? x)
  (and (pair? x) (eq? (car x) 'x))
)
;; 得到被加数与加数
;; cadr 表示第二项
(define (addend s) (cadr s))
;; caddr 表示第三项
(define (augend s) (caddr s))
;; 同理乘式以及乘式的第二三个元素
(define (product? x)
  (and (pair? x) (eq? (car x) '*))
)
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))
;; 求导函数deriv
(define (deriv exp var)
  (cond
    ;; number 是否为数值
    ((number? exp) 0)
    ;; 是否是一个符号，是的话相同为1，否则为0
    ((variable? exp) 
      (if (same-variable? exp var) 1 0))
      ;; 是否导数求和
    ((sum? exp) 
      (make-sum (deriv (addend exp) var)
                (deriv (augend exp) var))
    )
    ((product? exp) 
      (make-sum 
        (make-product (multiplier exp)
                      (deriv (multiplicand exp) var))
        (make-product (deriv (multiplier exp) var)
                      ((multiplicand exp)))
    )
    (else 
      (error "unknown expression type -- DERIV" exp)
    )
  )
)