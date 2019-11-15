;; 素数
(define (sum-primes a b)
    (define (iter count accum)
        (cond ((> count b) accum)
              ((prime? count) (iter (+ count 1) (+ count accum)))
              (else (iter (+ count 1) accum))))
    (iter a 0)
)
;; 序列操作
(define (sum-prime a b)
    (accumulate + 0 (filter prime ? (enumerate-interval a b)))
)
;; 流的素数和
(stream-car
    (stream-cdr
        (stream-filter prime?
            (stream-enumerate-interval a b))))

(define (stream-enumerate-interval low high)
    (if (> low high)
        the-empty-stream
        (cons-stream
            low
            (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
    (cond ((strean-null? stream) the-empty-stream)
          ((pred (stream-car stream))
            (cond-stream (stream-car stream)
                (stream-filter pred (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

;; 流基础结构满足
;; (stream-car (cons-stream x y)) = x
;; (stream-cdr (const-stream x y)) = y
;; 判断是否是空流
;; (stream-null? stream-list)
;; 空流
;; the-empty-stream
;; 流的各种操作
(define (stream-ref s n)
    (if (= n 0)
        (stream-car s)
        (stream-ref (stream-cdr s) (- n 1))
    )
)
;; map
(define (stream-map proc s)
    ;; 是否为空流
    (if (stream-null? s)
        the-empty-stream
        (cons-stream (proc (stream-car s))
            (stream-map proc (stream-cdr s)))))
;; forEach
(define (stream-for-each proc s)
    (if (stream-null? s)
        'done
        (begin (proc (stream-car s))
               (stream-for-each proc (stream-cdr s))))
)
;; 无穷流
(define (integers-starting-from n)
    (cons-stream
        n (integers-starting-from (+ n 1))
    )
)
;; 无穷整数流
(define integers (integers-starting-from 1))



;; 不能被7整除的流
(define (divisible? x y) (= (remainder x y) 0))

(define no-sevens
    (stream-filter (lambda (x) (not (divisible? x 7))) integers)
)

;; 斐波那契数列
(define (fibgen a b)
    (cons-stream a (fibgen b (+ a b)))
)
(define fibs (fibgen 0 1))

;; 无穷素数集
(define (sieve stream)
    (cons-stream
        (stream-car stream)
        (sieve
            (stream-filter
                (lambda (x) (not (divisible? x (stream-car stream))))
            (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))

;; 隐式定义流
(define ones (cons-stream 1 ones))

(define (add-streams s1 s2)
    (stream-map + s1 s2)
)

(define integers (cons-stream 1 (add-streams ones integers)))

;; 斐波那契
(define fibs
    (cons-stream 0
        (cons-stream 1
            (add-stream (stream-cdr fibs) fibs))))

(define (scale-stream stream factor)
    (stream-map (lambda (x) (* x factor)) stream)
)
(define double (const-stream 1 (scal-stream double 2)))

;; 流模式的计算
;; 平方根的流
(define (sqrt-improve guess x)
    (average guess (/ x guess))
)

(define (sqrt-stream x)
    (define guesses
        (cond-stream 1.0
            (stream-map (lambda (guess) (sqrt-improve guess x))
            guesses)))
    guess)
;; 调用 (display-stream (sqrt-stream 2))

;; 求 π-> 流的收敛
;; π/4 = 1 - 1/3 + 1/5 - 1/7 ....
(define (pi-summands n)
    (cons-stream (/ 1.0 n)
        (stream-map - (pi-summands (+ n 2)))))
(define pi-stream
    (scale-stream (partial-sums (pi-summands 1)) 4))

;; 欧拉的流变化理论
(define (euler-transform s)
    (let 
        ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stram-ref s 2)))
        (const-stream
            (-
                s2
                (/ (square (- s2 s1))
                   (+ s0 (* -2 s1) s2)))
            (euler-transform (stream-cdr s)))))

;; 序对的无穷流
;; 生成整数序对(i, j),i <= j 而且 i + j 是素数
(stream-filter (lambda (pair) (prime? (+ car pair) (cdr pair))) int-pairs)

(stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
;; 序对流
(define (pairs s t)
    (cons-stream
        (list (stream-car s) (stream-car t))
        (<安某种方式组合
            (stream-map (lambda (x) (list (stream-car s) x))
                        (stream-cdr t))
            (pairs (stream-cdr s) (stream-cdr t)))))

;; 流的合并，必须要第一个流全部生成以后，再生成第二个流
;; 交替的获取两个流的值
(define (interleave s1 s2)
    (if (stream-null? s1)
        s2
        (cons-stream (stream-car s1)
                     (interleave s2 (stream-cdr s1)))))

;; 函数式编程的模块化
(define rand
    (let
        ((x random-init))
        (lambda () (set! x (rand-update x))
        x)))

;; 银行账户取值简化版
(define (make-simplified-withdraw balance)
    (lambda (amout)
        (set! balance (- balance amout))
        banlance
    )
)
;; 流改造版
(define (stream-withdraw balance amount-stream)
    (cons-stream
        balance
        (stream-withdraw
            (- balance (stream-car amount-stream))
            (stream-cdr amount-stream)
        )
    )
)
