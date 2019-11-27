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

;; 牛顿算术逼近法求平方根, 给一个 x 求他的算术平方根，主函数 sqrt-iter

;; 1. 给定一个猜测 guess的数，看它的平方是否与x相差0.001
;; 2. 不是的话求出 (x / guess) 与 x 的平均值 作为guess
;; 重复步骤1 ，直至求出算术平方根

;; abs 求一个数的绝对值
(define (abs x)
  (if (< x 0) (- 0 x) x))
;; good-enough 判断猜测的数字是否符合平方小于x 0.001的要求，是的话表示符合平方根
(define (good-enough guess x)
  (<
    (abs (- (* guess guess) x))
    0.001))
;; average 求两个数的平均数
(define (average x y)
  (/ (+ x y) 2))
;; improve 改进猜测 它 与被开方数 除以 上一个猜测 的平均值
(define (improve guess x)
  (average guess (/ x guess)))

;; 主函数 sqrt-iter 给定一个数x ，和猜测为x的平方根的数， 求x的平方根
(define (sqrt-iter guess x)
  (
    if (good-enough guess x)
      guess
      (sqrt-iter (improve guess x) x)))