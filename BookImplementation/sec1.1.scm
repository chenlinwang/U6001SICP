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
(define (sqrt-iter guess x)
    ;; Compute the approximate square root by iterative improvement. guess is the current approximation and x is the number, whose square root we try to compute.
    ;; (number, number) -> numer
    ;; x>= 0, guess^2 = x
    (if (good-enough? guess x) ;; see guess good enough
        guess ;; yes, return guess
        (sqrt-iter (improve guess x) x))) ;; no, improve again

;Pack the thing
(define (sqrt x) (sqrt-iter 1.0 x))


;Pack the thing
(define (sqrt-pack x)
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
    (sqrt-iter 1.0 x))

;Pack the thing
(define (sqrt-densepack x)
    ;Define error to measure
    (define error 0.001)

    ;To test whether converage
    (define (good-enough? guess)
        (< (abs (- (square guess) x)) error))

    ;To improve the result using the Newton's method
    (define (average x y)
        (/ (+ x y) 2))
    (define (improve guess)
        (average guess (/ x guess)))

    ;To iterate until meet the converage
    (define (sqrt-iter guess) (if (good-enough? guess)
                                    guess
                                       (sqrt-iter (improve guess))))
    (sqrt-iter 1.0))
