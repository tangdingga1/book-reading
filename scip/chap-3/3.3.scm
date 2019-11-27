;; 队列 末端插入，前端删除

;; 防止每次插入队列都需要扫描到队列的最末端，非常低效，采用前端指针和后端指针
;; 前后指针
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
;; 设置队列前后值
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

;; make-queue 返回一个初始的空表
(define (make-queue) (cons '() '()))
;; (empty-queue? <queue>) 队列是否为空 -> 队列头是否为null
(define (empty-queue? queue) (null? (front-ptr queue)))
;; (front-queue <queue>) 返回队列前端的对象，不修改队列
(define (front-queue queue)
  (if (empty-queue? queue)
    (error "empty" queue)
    (car (front-ptr queue))
  )
)
;; (insert-queue! <queue>) 插入队列
(define (insert-queue! queue item)
  (let
    ((new-pair (cons item '())))
    ;; 如果是空的队列，那么前后
    (cond
      ((empty-queue? queue)
        (set-front-ptr! queue new-pair)
        (set-rear-ptr! queue new-pair)
        queue
      )
      (else
        (set-cdr! (rear-ptr queue) new-pair)
        (set-rear-ptr! queue new-pair)
        queue))))
;; (delete-queue! <queue>) 删除队列
(define (delete-queue! queue)
  (cond
    ((empty-queue? queue)
      (error "empty" queue))
    (else
      (set-fornt-ptr queue (cdr (front-ptr queue))))
    queue
  )
)


;; caar car两次
;; cdr cdr两次
;; table记录
(define (lookup key table)
  (let
    ((record (assoc key (cdr table))))
    (if record
      (cdr record)
      false)))

(define (assoc key records)
  (cond
    ((null? records) false)
    ;; 取出的 records 和 key 相同的话，返回这一项
    ((equal? key (caar records)) (car records))
  )
)
(define (insert! key value table)
  (let
    ((record (assoc key (cdr table))))
    (if record
      (set-cdr! record value)
      (set-cdr! table (cons (cons key value) (cdr table)))
    ))
  'ok
)
;; 二维表
(define (lookup key-1 key-2 table)
  (let
    ((subtable (assoc key-1 (cdr table))))
    (if subtable
      (let
        ((record (assoc key-2 (cdr subtable))))
        (if record (cdr record) false))
      false)))
;; 二维表插入数据
(define (insert! key-1 key-2 value table)
  (let
    ((subtable (assoc key-1 (cdr table))))
    (if subtable
      ;; 如果存在
      (let
        ((record (assoc key-2 (cdr subtable))))
        (if record
          (set-cdr! record value)
          (set-cdr! subtable (cons (cons key-2 value) (cdr subtable)))))
      ;; 不存在
      (set-cdr!
        table
        (cons (list key-1 (cons key-2 value)) (cdr table)))))
  'ok
)

;; 完成创建表格模块
(define (make-table)
  (let 
    ((local-table (list '*table*)))
    (
      (define (lookup key-1 key-2 table)
        (let
        ((subtable (assoc key-1 (cdr table))))
        (if subtable
          (let
            ((record (assoc key-2 (cdr subtable))))
            (if record (cdr record) false))
          false)))
      (define (insert! key-1 key-2 value table)
        (let
          ((subtable (assoc key-1 (cdr table))))
          (if subtable
            ;; 如果存在
            (let
              ((record (assoc key-2 (cdr subtable))))
              (if record
                (set-cdr! record value)
                (set-cdr! subtable (cons (cons key-2 value) (cdr subtable)))))
            ;; 不存在
            (set-cdr!
              table
              (cons (list key-1 (cons key-2 value)) (cdr table)))))
        'ok
      )
      (define (dispatch m)
        (cond
          ((eq? m 'lookup-proc) lookup)
          ((eq? m 'insert-proc) insert!)
          (else (error "unknown operation"))
        )
      )
    )
    dispatch
  )
)
;; 模拟信号电路
;; make-wire 基本的信号单位 创建线
(define a (make-wire))
;; 与或反门 or-gate and-gate inverter-gate 一个门需要多个线连接

;; half-adder 半加器，构造电路的一种
(define (half-adder a b s c)
  (let
    (
      (d (make-wire))
      (e (make-wire))
    )
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok
  )
)
;; 全加器，更加复杂的电路，一个全加器由两个半加器和一个门组成
(define (full-adder a b c-in sum c-out)
  (let
    (
      (s (make-wire))
      (c1 (make-wire))
      (c2 (make-wire))
    )
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok
  )
)

;; 基本功能块
;; (get-signal <wire>) 返回连线上的信号值
;; (set-signal! <wire><new value>) 连接线上的信号值修改为新值
;; (add-action! <wire> <procedire of no arguments) 断言，只要信号改变，就要运行
;; 反门逻辑
(define (inverter input output)
  (define (inverter-input)
    (let
      ;; 得到线上面的新值
      ((new-value (logical-not (get-signal input))))
      ;; after-delay 模拟信号线延迟获取信号
      ;; 延迟之后，给output输出线设置新值
      (after-delay inverter-delay
        (lambda () (set-signal! output new-value)))))
  (add-action! input inver-input)
  'ok
)
;; 反门的逻辑
(define (logical-not s)
  (cond
    ((= s 0) 1)
    ((= s 1) 0)
    (else (error "Invalid signal" s))
  )
)

;; 与门逻辑
(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let
      ((new-value (logical-and (get-signal a1 ) (get-signal a2))))
      (after-delay and-gate-delay
      (lambda () (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok
)

;; 或门逻辑
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let
      ((new-value (logical-or (get-signal a1 ) (get-signal a2))))
      (after-delay or-gate-delay
      (lambda () (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok
)


;; 线路的表示
;; signal-value信号值
;; 过程action-procedures，信号改变时，这些过程都需要运行
(define (make-wire)
  (let
    ((signal-value 0)
      (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value) (call-each action-procedures))
      'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedure))
      (proc))
    (define (dispatch m)
      (cond
        ((eq? m 'get-signal) signal-value)
        ((eq? m 'set-signal) set-my-signal!)
        ((eq? m 'set-signal) accept-action-procedure)
        (else (error 'unknown operation' m))))
    dispatch))

;; set-sighnal 检查新的信号是否改变了线路上的信号，如果是就会调用 call-each
(define (call-each procedures)
  (if (null? procedures)
    'done
    (begin
      ((car procedures))
      (call-each (cdr procedures)))))
    
;; 基本过程
(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure)
)

;; 待处理表 after-delay
;; (make-agenda) 返回一个新的待处理表
;; (empty-agenda? <agenda>) 在所给处理表为空时返回真
;; (first-agenda-item <agenda>) 返回待处理表的第一项
;; (remove-first-agenda-item! <agenda>) 删除其中的第一项
;; (add-to-agenda! time action agenda) 加入一项，在特定时间运行给定的动作过程
;; (current-time agenda) 返回模拟时间
(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
    action
    the-agenda))

;; 模拟过程 progagate 它对 the-agenda操作，执行每个过程
(define (propagate)
  (if (empty-agenda? the-agenda)
    'done
    (let
      ((first-item (first-agenda-item the-agenda)))
      (first-item)
      (remove-first-agenda-item! the-agenda)
      (propagate))))

;; 监视器 放于线路上，只要值变了，就打印出新的值
(define (probe name wire)
  (add-action! wire
    (lambda ()
      (newline)
      (display name)
      (display "")
      (display "new-value = ")
      (display (get-signal wire)))))

;; 处理表时间段组成
(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

;; 3.5约束的概念 9C = 5(F-32)
;; 约束的使用
(define C (make-connector))
(define F (make-connector))
(celsius-fahrenheit-converter C F)
(define (celsius-fahrenheit-converter C F)
  (let
    (
      (u (make-connector))
      (v (make-connector))
      (w (make-connector))
      (x (make-connector))
      (y (make-connector))
    )
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (contant 32 y)
    'ok))

;; 安装probe，使得每次赋予连接器一个值的时候，都会打印出来两个对应的值
(probe "celsius temp" C)
(probe "Fahrenheit temp" F)

(set-value! C 25 'user) ;; celsius temp 25  Fahrenheit temp 77

;; 约束系统的实现
;; (has-value? connector) 是否有值
;; (get-value connector) 返回连接器当前的值
;; 通知informant信息员，设置新值
;; (set-value! connector new-value informant)
;; 通知撤销源忘记其值
;; (forget-value! connector retractor)
;; 通知连接器参与一个新约束
;; (connect connector new-constraint)

(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond
      ((and (has-value? a1) (has-value? a2))
        (set-value! sum (+ (get-value a1) (get-value a2)) me))

      ((and (has-value? a1) (has-value? sum))
        (set-value! a2 (- (get-value sum) (get-value a1)) me))
      
      ((and (has-value? a2) (has-value? sum))
        (set-value! a1 (- (get-value sum) (get-value a2)) me))
    )
  )
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond
      ((eq? request 'I-have-a-value) (process-new-value))
      ((eq? request 'I-lost-my-value) (process-forget-value))
      (else (error "Unkonw request -- ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me) me
)
;; 乘法的实现
(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond
      
      ((or (and (has-value? m1) (= (get-value m1) 0))
           (and (has-value? m2) (= (get-lvaue m2) 0)))
        (set-value! product 0 me))
      
      ((and (has-value? m1) (has-value? m2))
        (set-value! product (* (get-value m1) (get-value m2)) me))
      
      ((and (has-value? product) (has-value? m1))
        (set-value! m2 (/ (get-value product) (get-value m1)) me))

      ((and (has-value? product) (has-value? m2))
        (set-value! m1 (/ (get-value product) (get-value m2)) me))
    )
  )
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond
      ((eq? request 'I-have-a-value) (process-new-value))
      ((eq? request 'I-lost-my-value) (process-forget-value))
      (else (error "Unkonw request -- ADDER" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me) me
)
;; constant 简单的设置值， 
(define (constant value connector)
  (define (me request)
    (error "Unknown request" request))
  (connect connector me)
  (set-value! connector value me) me)

;; 监视器 设置或者取消的时候打出一个值
(define (probe name connector)
  (define (print-probe value)
    (newline)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond
      ((eq? request 'I-have-a-value) (process-new-value))
      ((eq? request 'I-Lost-my-value) (process-forget-value))
      (else (error "Unknown request -- PROBE" request))))
  (connect connector me) me)
;; 连接器的表示
;; value 值 informant 设置连接器的对象 constraints 所涉及的所有约束的表
(define (make-connector)
  (let
    ((value false) (informant false) (constraints '()))
    (define (set-my-value newval setter)
      (cond (
        (not (has-value? me))
          (set! value newval)
          (set! informant setter)
          (for-each-except setter inform-about-value constraints))
        ((not (= value newval))
          (error "Contradiction" (list value newval)))
        (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
        (begin
          (set! informant false)
          (for-each-except retractor inform-about-no-value constraints))
        'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
        (set! constraints (cons new-constraint constraints)))
      (if (has-value? me) (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond
        ((eq? request 'has-value?) (if informant true false))
        ((eq? request 'value) value)
        ((eq? request 'set-value!) set-my-value)
        ((eq? request 'forget) forget-my-value)
        ((eq? request 'connect) connect)
        (else (error "Unknown operation -- CONNECTOR" request))))
    me))