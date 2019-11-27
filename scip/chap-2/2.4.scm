;; 复数的抽象屏障
;; 直角坐标方式
(define (real-part z) (car z))
(define (imag-part z) (cdr z))
(define (magnitude z)
  (sqrt
    (+ (square (real-part z)) (square (imag-part z)))
  )
)
(define (angle z)
  ;; atan scheme 自带的反正切函数  y x 返回 y/x 的角度 参数的符号决定角度的象限
  (atan (imag-part z) (real-part z))
)
(define (make-from-real-imag x y)
  (cons x y)
)
(define (make-from-mag-ang r a)
  (cons (* r (cos a)) (* r (sin a )))
)
;; 极坐标方式
(define (real-part z)
  (* (magnitude z) (cos (angle z)))
)
(define (imag-part z)
  (* (magnitude z) (sin (angle z)))
)
(define (magnitude z) (car z))
(define (angle z) (cdr z))

(define (make-from-real-imag x y)
  (cons (sqrt (+ (square x) (square y)) (atan y x)))
)
(define (make-from-mag-imag r a)
  (cons r a)
)
;; 创建复数复合结构
(make-from-real-imag (real-part z) (imag-part z))
(make-from-mag-ang (magnitude z) (angle z))

;; 复数的操作方法 加 减 乘 除
(define (add-complex z1 z2)
  (make-from-real-imag
    (+ (real-part z1) (real-part z2))
    (+ (imag-part z1) (imag-part z2))
  )
)
(define (sub-complex z1 z2)
  (make-from-real-imag
    (- (real-part z1) (real-part z2))
    (- (imag-part z1) (imag-part z2))
  )
)
(define (mul-complex z1 z2)
  (make-from-mag-ang
    (* (magnitude z1) (magnitude z2))
    (+ (angle z1) (angle z2))
  )
)
(define (div-complex z1 z2)
  (make-from-mag-ang
    (/ (magnitude z1) (magnitude z2))
    (- (angle z1) (angle z2))
  )
)

;; 带标志的数据
;; 利用标志来实现两种坐标方式融合
(define (attach-tag type-tag content)
  (cons (type-tag content))
)
(define (type-tag datum)
  (if (pair? datum)
    (car datum)
    (display "Bad tagged datum -- type-TAG")
  )
)
(define (contents datum)
  (if (pair? datum)
    (cdr datum)
    (display "bad tagged datum -- CONTENTS")
  )
)
;; 判断是哪种数据结构
(define (rectangular? z)
  (eq? (type-tag z) 'rectangular)
)
(define (polar? z)
  (eq? (type-tag z) 'polar)
)
;; 加入标识层之后的直角坐标和极坐标表示方式
;; 直角坐标
(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))
(define (magnitude-rectangular z)
  (sqrt (+
    (saquare (real-part-rectangular z))
    (saquare (imag-part-rectangular z))
  ))
)
(define (angle-rectangular z)
  (atan
    (imag-part-rectangular z)
    (real-part-rectangular z)
  )
)
(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y))
)
(define (make-from-mag-ang-rectangular r a)
  (attach-tag
    'rectangular
    (cons
      (* r (cos a))
      (* r (sin a))
    )
  )
)
;; 极坐标方式
(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z)))
)
(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z)))
)
(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))

(define (make-from-real-imag-polar x y)
  (attach-tag
    'polar
    (cons (sqrt (+ (square x) (square y)) (atan y x)))
  )
  
)
(define (make-from-mag-imag r a)
  (attach-tag
    'polar
    (cons r a)
  )
)