;; 2.2.1 序列的表示
;; list 定义数组
(define demo-list (list 1 2 3 4))
(define nil-list (list))
;; nil 表示空的表，用于超出表边界和空表的显示 只有堆栈的性质
;; car 表示取出list的第一项
;; cdr 用来取出list除了第一项之外剩下项目
;; cons 向表最前面添加一个元素
;; 使用 null? list 可以判定对应的list是否为空

;; 利用表的特性，完成一个表操作函数list-ref，给定数字取出表的n项的数据
(define (list-ref input-list n)
  (if (= n 0)  
    (car input-list)
    (list-ref (cdr input-list) (- n 1))
  )
)
;; length函数，用于得到表的长度
(define (length input-list)
  (if (null? input-list)
    0
    (+ 1 (length (cdr input-list)))
  )
)
(length demo-list 2)

;; 练习2.17 last-pair 返回表中最后一个元素
;; 树形递归法 取出一项看是否为空，是返回取出的这一项，否则放回去
(define (list-pair input-list)
  (if (null? (cdr input-list))
    (car input-list)
    (list-pair (cdr input-list))
  )
)
;; 练习 2.18 reverse 翻转数组
(define (reverse input-list)
  ;; 从第一项开始到最后一项
  (define (add-item-to-list input-list void-list)
    (if (null? input-list)
      void-list 
      (add-item-to-list
        (cdr input-list)
        (cons (car input-list) void-list)
      )
    )
  )
  (add-item-to-list input-list (list))
)

;; 2.2.3 序列作为一个约定的界面
;; filter funciton
(define (filter predicate sequence)
  (cond ((null? sequence) ())
    ;; 判定取出的数字是否符合
    ((predicate (car sequence))
      (cons (car sequence) (filter predicate (cdr sequence)))
    )
    (else (filter predicate (cdr sequence)))
  )
)
;; (filter (lambda (x) (> x 2)) (list 1 2 3 4))
;; accumlate 类似于reduce函数
(define (accumlate op initial sequence)
  ;;
  (if (null? sequence)
      initial
      ;; 这里op传的是 + - * / 的符号
      (op
        (car sequence)
        (accumlate op initial (cdr sequence))
      )
  )
)
;; enumerate-tree pass dataStruct like (list (list 1) 1)
(define (enumerate-tree tree)
  (cond ((null? tree) ())
        ((not (pair? tree))
          (list tree)
        )
        (else (append (enumerate-tree (car tree))
          (enumerate-tree (cdr tree))
        ))
  )
)
(enumerate-tree (list 1 (list 2 3) (list 2)))