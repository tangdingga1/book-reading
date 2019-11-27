;; 过程的抽象
;; 抽象一个从a到b的过程,他可以用term方法，对a(当前项)进行计算，用next方法对a进行叠加，在大于等于b时，会停止该方法，并返回0

;; 递归法
(define (sum term a next b)
  (if (> a b)
    0
    (+ a
      (sum term (next a) next b)
    ))
)
;;迭代法
(define (sum-iter term a next b)
  ;; 外部使用不应该去关心起始值！需要做一部屏蔽
  (sume-iter-main term a next b 0)
)
(define (sume-iter-main term a next b now)
  (if (> a b)
    now
    (sume-iter-main term (next a) next b (+ now (term a)))
  )
)


;; 计算a-b的正数之和
(define (addself a) (+ a 1))
(define (term a) a)

;; 使用display进行输出
(display "1111")

;; 使用lambda
(define (sumAdd iterm x)
  (iterm 4)
)
(sumAdd 
  (lambda (x) (+ x 4)) 
  4
)
;; 使用let
;; (ket (
;;   (<var1><exp1>)
;;   (<var2><exp2>)
;;   )
;;  body)
(let ((x 3)) (+ x 3))
