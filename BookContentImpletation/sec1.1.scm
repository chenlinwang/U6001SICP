;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Sec1.7 Square Roots by Newton's Method
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Define error to measure
(define error 0.001)

;To test whether converage
(define (good-enough? guess x)
    (< (abs (- (square guess) x)) error))

;To improve the result using the Newton's method
(define (average x y)
    (/ (+ x y) 2))
(define (improve guess x)
    (average guess (/ x guess)))

;To iterate until meet the converage
(define (sqrt-iter guess x) (if (good-enough? guess x)
                                guess
                                   (sqrt-iter (improve guess x) x)))

;Pack the thing
(define (sqrt x) (sqrt-iter 1.0 x))
