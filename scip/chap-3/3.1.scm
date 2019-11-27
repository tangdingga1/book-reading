;; 使用set 
(define balance 100)
(define (withdraw amount)
  (if (>= balance amount)
    ;; (begin <exp1> <exp2>) 按照顺序求值
    (begin (set! balance (- amount)) balance)
    "Insufficient funds"
  )
)
;; 封存局部变量到函数内部
(define new-withdraw
  (let 
  (
    (balance 100)) 
    (lambda (amount)
      (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance)
        "Insufficient funds"
      )
    )
  )
)
;; 提款处理器,独立的对象
(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount)) balance)
      "Insufficient funds"
    )
  )
)

;; 带银行账户的封存对象
(define (make-account balance)
  ;; 账户取钱函数
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount)) balance)
      "Insufficient funds"
    )
  )
  ;;
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance
  )
  (define (dispatch m)
    (cond
      ((eq? m 'withdraw) withdraw)
      ((eq? m 'deposit deposit))
      (else (error "unkonw request -- MAKE-ACCOUNT" m))))
  dispatch
)
;; 3.1 累加器
(define (make-accumulator num)
  (lambda (add-num)
    (+ num add-num)
  )
)
(define A (make-accumulator 5))

;; 赋值变量带来的利益
;; 假设存在一个过程 rend-update 从一个给定的数开始形成一系列随机数
(define rand
  (let
    ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

;; 蒙特卡罗 利用6/π 是随机选取两个整数最大公约数为1的几率，求出π的值
(define (estimate-pi trials) (sqrt (/ 6 (monte-carlo trials cesaro-test))))
;; gcd 为前面写的求最大公约数的函数
(define (cesaro-test) (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond
      ((= trials-remaining 0) (/ trials-passed trials))
      ((experiment)
        (iter (- trials-remaining 1) (+ trials-passed 1)))
      (else
        (iter (- trials-remaining 1) (trials-passed)))))
  (iter trials 0)
)

;; 不适用局部变量 直接操作update
(define (eastimate-pi trials)
  (sqrt (/ 6 (random-god-test trials random-init))))
(define (random-gcd-test trials initial-x)
  (define (iter trials-remaining trials-passed)
    (let ((x1 (rand-update x1)))
      (let ((x2 (rand-update x1)))
        (cond 
          ((= trials-remaining 0) (/ trials-passed trials))
          ((= (gcd x1 x2) 1)
            (iter (- trials-remaining 1) (+ trials-passed 1) x2))
          (else
            (iter (- trials-remaining 1) trials-passed x2))))))
  (iter trials 0 initial-x)
)
