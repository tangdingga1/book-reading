;; 得到分子
(define (numer x) (car x))
;; 得到分母
(define (denom x) (cdr x))
;; 打印无理数
(define (console x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x))
)
;; 求公约数，以便约分无理数
;; r 是 a 除以 b 的余数，那么 a 和 b 的公约数 正好也是b 和r 的公约数
(define (GCD a b)
  ;; 求余数
  (define (get-remainder a b) 
    (if (< a b) a (get-remainder (- a b) b))
  )
  (if (= b 0) 
    a
    (GCD b (get-remainder a b))
  )
)

;; 定义无理数
(define (make-rat n d)
  (define GCD-num (GCD n d))
  (console (cons (/ n GCD-num) (/ d GCD-num)))
)
(define x (cons 1 2))
(display x)

2.1.4 拓展练习：区间算术
;; 区间构造符的定义是 make-interval
(define (make-interval a b) (cons a b))

;;求上界 upper-bound
(define (upper-bound interval) 
  (if (< (car interval) (cdr interval))
    (cdr interval)
    (car interval)
  )
)
;; 求下界 lower-bound
(define (lower-bound interval) 
  (if (> (car interval) (cdr interval))
    (cdr interval)
    (car interval)
  )
)
;; 这里上下界其实只需要通过一个flag进行区分，可以写成一个
;; interval 区间
;; upper-or-lower-flag #t 为 upper
(define (get-bound interval upper-or-lower-flag)
  (let 
  ((lower (lower-bound interval))
    (upper (upper-bound interval)))
  (if upper-or-lower-flag upper lower)
  )
)

;; 差应该是上界之差与下界之差 sub-interval
(define (sub-interval x y)
  (make-interval
    (- (get-bound x #f) (get-bound y #f))
    (- (get-bound x #t) (get-bound y #t))
  )
)