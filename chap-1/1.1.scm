;; 练习1.2 请将下面表达式变换为前缀形式
(/
  (+
    (+ 5 4)
    (-
      2
      (- 3 (+ 6 (/ 4 5)))))

  (* 3 (- 6 2) (- 2 7))
)


;; 练习 1.3 请定义一个过程，它以三个数为参数，返回其中较大的两个数之和，解答函数 returnTowBiggerSum

;; returnSmallerNum函数 输入两个数，返回更小的那个数
(define (returnSmallerNum a b)
  (if (< a b) a b)
)
;; 思路为两个较大数就是三个数减去最小那个数
(define (returnTowBiggerSum a b c)
  (-
    (+ a b c)
    (returnSmallerNum (returnSmallerNum a b) c)
  )
)