;; eval的定义
(define (eval exp env)
    (cond
        ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
            (make-procedure (lambda-paramters exp)
                            (lambda-body exp)
            env))
        ((begin? exp)
            (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
            (apply (eval (operator exp) env)
                   (list-of-values (operands exp) env)))
        (else (error "Unknown expression type -- EVAL" exp))
    )
)

;; 过程参数 list-of-value
(define (list-of-values exps env)
    (if (no-operands? exps)
        '()
        (cons (eval (first-operand exps) env)
              (list-of-values (rest-operands exps) env))))

;; 条件
(define (eval-if exp env)
    (if (true? (eval (if-predicate exp) env))
        (eval (if-consequent exp) env)
        (eval (if-alternative exp) env)))

;; 赋值和定义
(define (eval-assignment exp env)
    (set-variable-value! (assignment-variable exp)
                         (eval (assignment-value exp) env)
    env)
'ok)

;; 变量定义
(define (eval-definition exp env)
    (define-variable! (definition-variable exp)
                         (eval (definition-value exp) env)
    env)
'ok)

;; apply
;; apply-primitive-procedure 基本过程应用
(define (apply procedure arguments)
    (cond
        ((primitive-procedure? procedure)
          (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
            (procedure-body procedure)
            (extend-environment
                (procedure-paramters procedure)
                arguments
                (procedure-paramters procedure))))
        (else
            (error "unknown procedure type -- apply" procedure)
        )
    )
)

;; 序列 eval-sequence
(define (eval-sequence exps env)
    (cond
        ((list-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

;; 4.1.2 表达式的表示
(define (self-evaluating? exp)
    (cond
        ((number? exp) true)
        ((string? exp) true)
        (else false)))

;; 变量的表示
(define (variable? exp) (symbol? exp))

(define (quoted exp)
    (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
    (if (pair? exp)
        (eq? (car exp) tag)
        false))

;; 赋值的形式 (set! <var><value>)
(define (assignment? exp)
    (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

;; 定义的形式 (define <var><value>) (define (<var><params1><params2>) <body>)
(define (definition? exp)
    (tagged-list? exp 'define))

(define (definition-variable exp)
    (if (symbol? (cadr exp))
        (cadr exp)
        (caddr exp)))

(define (definition-value exp)
    (if (symbol? (cadr exp))
        (caddr exp)
        (make-lambda (cdadr exp)
                     (cddr exp))))

;; lambda 表达式是 lambda 开始的表
(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
    (cons 'lambda (cons parameters body)))

;; if 分支
(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
    (if (not (null? (cddr exp)))
        (cadddr exp)
        'false))

;; if 构造函数
(define (make-if predicate consequent alternative)
    (list 'if predicate consequent alternative)
)

;; begin
(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

;; 构造函数
(define (sequence->exp seq)
    (cond
        ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

;; 其他的复合表达式
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

;; cond 转换为 if
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clauses? clauses)
    (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
    (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
    (if (null? clauses)
        'false
        (let
            ((first (car clauses))
             (rest (cdr clauses)))
            (if (cond-else-clause? first)
                (if (null? rest)
                    (sequence->exp (cond-actions first))
                    (error "ELSE clause isn't last -- COND->IF" clauses))
                (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest))))))

;; 求值器的数据结构
(define (true? x)
    (not (eq? x false))
)
(define (false? x)
    (eq? x false)
)
;; 假设存在 (apply-primitive-procedure <proc><arg>)
;; (primitive-procedure? <proc>)
(define (make-procedure parameters body env)
    (list 'procedure parameters body env))

(define (compound-procedure? p)
    (tagged-list? p 'procedure)
)

(define (procedure-parameters p) (cadr p))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))

;; 对环境的操作
;; 环境设置为一个框架的表

(define (enclosing-envroment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())


;; 约束值的表
(define (make-frame variables values)
    (cons variables values))

(define (frame-variable frame) (car frame))

(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
    (set-car! frame (cons var (car frame)))
    (set-car! frame (cons val (cdr frame)))
)

;; 变量和值不匹配发出错误信号
(define (extend-environment vars vals base-env)
    (if (= (length vars) (length vals))
        (cons (make-frame vars vals) base-env)
        (if (< (length vars) (length vals))
            (error "too many arguments" vars vals)
            (error "too few" vars vals))))

;; 环境中查找变量
(define (lookup-variable-value var env)
    (define (env-loop env)
        (define (scan vars vals)
            (cond
                ((null? vars)
                 (env-loop (enclosing-environment env)))
                ((eq? var (car vars))
                 (car vals))
                (else (scan (cdr vars) (cdr vals))))
        (if (eq? env the-empty-environment)
            (error "unbound variable" var)
            (let ((frame (first-frame env)))
                (scan (frame-variable frame)
                      (frame-values frame))))
        (env-loop env)))

;; 环境中设置变量
(define (set-variable-value var val env)
    (define (env-loop env)
        (define (scan vars vals)
            (cond
                ((null? vars)
                 (env-loop (enclosing-environment env)))
                ((eq? var (car vars))
                 (set-car! vals val))
                (else (scan (cdr vars) (cdr vals))))
        (if (eq? env the-empty-environment)
            (error "unbound variable" var)
            (let ((frame (first-frame env)))
                (scan (frame-variable frame)
                      (frame-values frame))))
        (env-loop env)))

;; 定义变量，找约束，找不到约束，第一个框架中加入
(define (define-variable var val env)
    (let ((frame (first-frame env)))
        (define (scan vars vals)
            (cond
                ((null? vars)
                 (add-binding-to-frame! var val frame))
                ((eq? var (car vars))
                 (set-car! vals val))
                (else (scan (cdr vars) (cdr vals)))))
        (scan (frame-variables frame)
              (frame-values frame))))

;; 作为程序运行求值器
(define (setup-environment)
    (let
        ((inital-env
            (extend-environment (primitive-procedure-names)
                                (primitive-procedure-objects)
                                the-empty-environment)))
        (define-variable! 'true true inital-env)
        (define-variable! 'false inital-env)
    inital-env))


