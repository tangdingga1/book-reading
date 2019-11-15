;; scheme 串行化
;; parallel-execute为一个并行进程运行，每一个参数为一个无参数过程，这些过程会同时并行
(parallel-execute <p1> <p2> <p3>)
(parallel-execute
    (lambda () (set! x (* x x)))
    (lambda () (set! x (+ x 1)))
)

;; 序列化过程 make-serializer 串行进程
(make-serializer <p1> <p2> <p3>)

;; 交换账户余额
(define (exchange account1 account2)
    (let ((difference
        (-
            (account1 'balance')
            (account2 'balance')
        ))))
    ((account1 'withdraw') difference)
    ((account2 'deposit') difference))

;; 串行化后的
(define (make-account-and-serializer balance)
    (define (withdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount)) balance)
            "Insufficient funds"))
    (define (deposit amount)
        (set! balance (+ balance amount))
        balance)
    (let ((balance-serializer (make-serializer)))
        (define (dispatch m)
            (cond ((eq? m 'withdraw) withdraw)
                  ((eq? m 'deposit) deposit)
                  ((eq? m 'balance) balance)
                  ((eq? m 'serializer) balance-serializer)
                  (else (error "Unknown request -- make-account" m))))
    dispatch))

;; 串行的exchange过程
(define (serialized-exchange account1 account2)
    (let ((serializer1 (account1 'serializer))
         ((serializer2 (account2 'serializer)))
    ((serializer1 (serializer2 exchange))
        account1
        account2
    )))  

;; 串行化的实现
(define (make-serializer)
    (let ((mutex (make-mutex)))
        (lambda (p) 
            (define (serialized-p . args)
                (mutex 'acquire)
                (let ((val (apply p args)))
                    (mutex 'release)
                    val))
        
        serialized-p)))

;; 互斥元构造函数
(define (make-mutex)
    (let ((cell (list false)))
        (define (the-mutex m)
            (cond ((eq? m 'acquire)
                (if (test-and-set! cell)
                    (the-mutex 'acquire)))
                ((eq? m 'release) (clear! cell))))
        the-mutex))

(define (clear! cell)
    (set-car! cell false)
)

;; 检查结构并返回
(define (test-and-set! cell)
    (if (car cell)
        true
        (begin (set-car! cell true) false)))
    