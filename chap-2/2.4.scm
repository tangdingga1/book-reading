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
  (and (pair? x) (eq? (car x) '+))
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
                (deriv (augend exp) var)))
    ((product? exp) 
      (make-sum 
        (make-product (multiplier exp)
                      (deriv (multiplicand exp) var))
        (make-product (deriv (multiplier exp) var)
                      (multiplicand exp))))
    (else 
      (display "unknown expression type -- DERIV"))))
(deriv '(+ x 3) 'x)

;; 集合，每个数字只存在一项
;; 给定的x是否是set中的一项
(define (element-of-set? x set)
  (cond
    ((null? set) #f)
    ;; 使用equal，确保集合可以不是基本类型
    ((equal? x (car set)) #t)
    (else (element-of-set? x (cdr set)))
  )
)
;; 给定项已经存在于集合，那么返回集合，否则cons进集合
(define (adjoin-set x set)
  (if (element-of-set? x set) set (cons x set))
)
;; 两个集合的交集
(define (intersection-set set1 set2)
  (cond
    ((or (null? set1) (null? set2) ()))
    ;; 如果set1取出项是set2的成员，那么把他填入
    ((element-of-set? (car set1) set2)
      (cons (car set1) (intersection-set (cdr set1) set2)))
    (else (intersection-set (cdr set1) set2))
  )
)
;; 两个集合的并集
(define (union-set set1 set2)
  (cond
    ;; 只去操作set1
    ((null? set1) set2)
    ;; 如果set1的这一项是set2的成员
    ((element-of-set? (car set1) set2)
      (intersection-set (cdr set1) set2))
    ;; 否则就把set1这一项放入到set2当中
    (else (intersection-set (cdr set1) (cons (car set1) set2)))
  )
)
;; 加速集合查找，似的集合按照顺序排列
;; 在按照顺序排列的集合情况下优化的element-of-set
(define (element-of-set? x set)
  (cond
    ((null? set) #f)
    ((= x (car set)) #t)
    ((< x) (car set) #f)
    (else (element-of-set? x (cdr set)))
  )
)
;; 在按照顺序排列的集合情况下优化的intersection-set
(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
    () 
    (let
      (
        (x1 (car set1))
        (x2 (car set2)))
      (cond(
        ;; 相当的时候直接放入
        ((= x1 x2) (cons x1 (intersection-set (cdr set1) (cdr set2))))
        ;;小于直接就pass
        ((< x1 x2)
          (intersection-set (cdr set1) set2))
        ;; 大于继续
        ((< x2 x1)
          (intersection-set set1 (cdr set2)))))
    )
  )
)
;; 优化的adjoin-set
(define (adjoin-set x set)
  (cond
    ((null? set) (cons x set))
    ((< x (car set) (cons x set)))
    ((= x (car set) set))
    ((> x (car set) (adjoin-set x (cdr set))))
  )
)

;; 二叉集合树
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right) (list entry left right))

;; 二叉树改写 element-of-set, 建立在二叉平衡树的基础上
(define (element-of-set? x set)
  (cond
    ((null? set) #f)
    ((= x (entry tree)) #t)
    ((< x (entry tree)) (element-of-set? x (left-branch set)))
    ((> x (entry tree)) (element-of-set? x (right-branch set)))
  )
)

;; adjoin-set 二叉树改写
(define (adjoin-set x set)
  (cond
    ((null? set) (make-tree x '() '()))
    ((= x (entry set)) set)
    ((< x (entry set))
      (make-tree (entry set)
        (adjoin-set x (left-branch set))
        (right-branch set)))
    ((> x (entry set))
      (make-tree (entry set)
        (left-branch set)
        (adjoin-set x (right-branch set))))
  )
)
;; huffman tree
;; define huffman tree
;; 一个树包含标识 leaf 符号 权重
(define (make-leaf symbol weight)
  (list 'leaf symbol weight)
)
;; 是否是树结构
(define (leaf? object)
  (eq? (car object) 'leaf)
)
;; 获得树的符号
(define (symbol-leaf x) (cadr x))
;; 获得树的权重
(define (weight-leaf x) (caddr x))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)
  )
)
(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)
  )
)
;; make tree
(define (make-code-tree left right)
  (list left right 
    （append (symbols left) (symbols right))
     (+ (weight left) (weight right))
  )
)
;; 解码过程
(define (choose-branch bit branch)
  (cond
    ((= bit 0) (left-branch branch))
    ((= bit 1) (right-branch branch))
    (else (display 'bad bit -- 0 or 1'))
  )
)
(define (decode bit tree)
  (define (decode-1 bite current-branch)
    (if (null? bite)
      ;;空的返回一个空表
      '()
      (let 
      ((next-branch (choose-branch (car bits) current-branch)))
      ;; 如果下一个树杈是树形 
      (if (leaf? next-branch)
        (cons
          ;; 获得树的符号列表
          (symbol-leaf next-branch)
          ;;
          (decode-1 (cdr bits) next-branch)
        )
        ;; 1
        (decode-1 (cdr bits) next-branch)
      )
      )
    )
  )
  (decode-1 bits tree)
)
