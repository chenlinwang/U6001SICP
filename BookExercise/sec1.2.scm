;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Ex1.10
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(define (f n) (A 0 n))

(define (g n) (A 1 n))

(define (h n) (A 2 n))

(define (k n) (* 5 n n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Ex1.11
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (f-r n)
  (if (< n 3)
      n
      (+ (f-r (- n 1)) (f-r (- n 2)) (f-r (- n 3)))))

(define (f-i n)
  (define (f-iter f m l n)
    (if (= n 0)
        f
        (f-iter m l (+ f m l) (- n 1))))
  (f-iter 0 1 2 n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Ex 1.12
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Pasca Triangle
(define (p-r n i)
  (if (or (= i 1) (= i n))
      1
      (+ (p-r (- n 1) (- i 1)) (p-r (- n 1) i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Ex 1.15
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;approximation of sine
; recursive process
(define (sine x)
  ;cube of x
  (define (cube x) (* x x x))
  ;function
  (define (f x) (- (* 3 x) (* 4 (cube x))))
  (if (< x 0.1)
      x
      (f (sine (/ x 3.0)))))

(define (times-run x)
  (if (< x 0.1)
      0
      (+ 1 (times-run (/ x 3.0)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Ex 1.16
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; iterative version of the fast exponent
(define root 2)

(define (ex-f-i n)
  (define (square n) (* n n))
  ;define a test for even
  (define (even? n)
    (= (remainder n 2) 0))
  ;real process
  (define (ex-f-iter a b n)
    (cond ((= n 0) 0)
          ((= n 1) (* a b))
          ((even? n)
           (ex-f-iter a (square b) (/ n 2)))
          (else
           (ex-f-iter (* a b) b (- n 1)))))
  (ex-f-iter 1 root n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Ex 1.17 1.18
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;build the default operators
;double the number
(define (double n)
  (* n 2))
;halve
(define (halve n)
  (/ n 2))
;root
(define root 2)

;;So do it-again
;test for even
(define (even? n)
  (= (remainder n 2) 0))

;recursive
(define (mul-f-r n)
  (cond ((= 0 n) 0)
        ((even? n)
         (double (mul-f-r (halve n))))
        (else
         (+ root (mul-f-r (- n 1))))))

;iterative
(define (mul-f-i n)
  (define (mul-f-iter a b n)
    (cond ((= n 0) 0)
          ((= n 1) (+ a b))
          ((even? n) (mul-f-iter a (double b) (halve n)))
          (else
           (mul-f-iter (+ a b) b (- n 1)))))
  (mul-f-iter 0 root n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Ex 1.19
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;square
(define (square x) (* x x))
;Define the calculation for p and q
(define (cal-p p q) (+ (square p) (square q)))
(define (cal-q p q) (+ (* 2 p q) (square q)))
;Define how to apply tranformation T
(define (cal-tl p q l f) (+ (* l (+ p q)) (* f q)))
(define (cal-tf p q l f) (+ (* l q) (* f p)))

;iterative
(define (fib-f-i n)
  (define (fi p q l f n)
    (cond ((= 0 n) f)
          ((even? n)
           (fi (cal-p p q) (cal-q p q) l f (/ n 2)))
          (else
           (fi p q (cal-tl p q l f) (cal-tf p q l f) (- n 1)))))
  (fi 0 1 1 0 (- n 1)))
