;; 线性递归 与 迭代计算
;; 线性递归 保存的信息随着n的增长而线性增长
;; 迭代计算 固定状态
(define (inc a) (+ a 1))
(define (dec a) (- a 1))
(define (demo1 a b)
  (if (= a b) 
    b
    (inc (demo1 (dec a) b))))
(define 
  (demo2 a b)
  (if (= a 0)
    b
    (demo2 (dec a) (inc b))))

(define (A x y)
  (cond 
    ((= y 0) 0)
    ((= x 0) (* 2 y))
    ((= y 1) 2)
    (else 
      (A (- x 1) (A x (- y 1))))))

;; 练习1.11 按照规则书写迭代计算过程
;; 1.11 递归法
(define (rule n)
  (if (< n 3)
    n 
    (+ 
      (rule (- n 1))
      (* 2 (rule (- n 2)))
      (* 3 (rule (- n 3))))))
;; 1.11 迭代法
(define 
  (rule2 n)
  (rule2-it 0 1 2 n))

(define 
  (rule2-it a b c n)
  (if (< n 3) (less-than a b c n)
      (rule2-it b c 
        (+ c (* 2 b) (* 3 a))
        (- n 1))))
(define 
  (less-than a b c n)
  (cond ((= n 0) a)
        ((= n 1) b)
        ((= n 2) c)))

;; 1.2.4 求幂部分
练习1.16 求幂部分
;; 递归法求幂
;; b^n = (b ^ n / 2) ^ 2 b 为偶数
;; b^n = b * b ^ n - 1 b 为基数

;; tools
;; 判断一个数是否是偶数 iseven => #t #f
(define (is-even n)
  (if (> n 1)
    (is-even (/ n 2))
    (= n 1)
  )
)
;; 求一个数的幂
(define (expt n power)
  (cond ((= power 1) n)
        ((= power 0) 1)
        ((is-even power) 
        (* 
          (expt n (/ power 2))
          (expt n (/ power 2))
        ))
        (else (* n (expt n (- power 1))))
  )
)

;; 迭代法求幂，主函数
(define 
  (expt-iter n power)
  (if (is-even power)
    (expt-iter-computed n power)
    ;; 奇数减1 肯定是偶数
    (* n (expt-iter-computed n (- power 1)))))
;; 内部函数，去迭代计算幂的值
(define 
  (expt-iter-computed n power)
  (if
    (= power 0)
    n
    (expt-iter-computed (* n n) (- power 2))
  )
)

;; 1.2.6 素数检测

;; 寻找因子的素数检测 如果n不是素数，它必然有一个小于或者等于 根号n的因子，算法复杂度为根号n
(define (smallest-divisor n)
  (find-divisor n 2)
)
;; 求一个数是否能被另一个数整除
(define (isremainder divisor n)
  (if (< divisor n)
    (= divisor 0)
    (isremainder (- divisor n) n)
  )
)
(define (find-divisor n test-divisor)
  ;; 是否大于根号n，如果大于根号n，返回n自身(n为素数)
  (cond ((> (* test-divisor test-divisor) n) n)
        ;; 是否能被整除,是的话返回test-divisor
        ((isremainder n test-divisor) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))
)