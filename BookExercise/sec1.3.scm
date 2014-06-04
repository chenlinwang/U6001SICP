;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Exercise 1.29
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simpson's Rule is a more accurate method of numerical integration than the method illustrated above. Using Simpson's Rule, the integral of a function f between a and b is approximated as

;; h/3 * ( y_0 + 4y_1 + 2y_2 + ... + 2y_{n-2} + 4y_{n-1} +y_n)

;; where h = (b - a)/n, for some even integer n, and y_k = f(a + kh). (Increasing n increases the accuracy of the approximation.) Define a procedure that takes as arguments f, a, b, and n and returns the value of the integral, computed using Simpson's Rule. Use your procedure to integrate cube between 0 and 1 (with n = 100 and n = 1000), and compare the results to those of the integral procedure shown above.

;;change x to even if it is not
(define (changetoeven x)
    (if (= 0 (remainder x 2))
        x
        (+ x 1)))

;;test whether x is even
(define (even? x)
    (= 0 (remainder x 2)))

;;simpson formula maker for f and n
(define (simpson f n)
    ;;change n to even n
    (define newn (changetoeven n))
  ;;the integral formula
  (define (sum a b)
      ;; calculate the increment
      (define dx (/ (- b a) (* 1.0 newn)))
    ;;using iterative process to optimize
    (define (sumiter init s count)
        (if (= count newn)
            s
            (sumiter (+ init dx) (+ s (* (if (even? count) 2 4) (f init)))  (+ count 1))))
    (* dx (/ 1.0 3.0) (+ a b (sumiter (+ dx a) 0 1))))
  ;;return the formual
  sum)

(define (cube x) (* x x x))
(define Sx3-100 (simpson cube 100))
(define Sx3-1000 (simpson cube 1000))

(Sx3-100 0 1)
;; 0.25
(Sx3-1000 0 1)
;; 0.25

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Exercise 1.31
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1. The sum procedure is only the simplest of a vast number of similar abstractions that can be captured as higher-order procedures. Write an analogous procedure called =product= that returns the product of the values of a function at points over a given range. Show how to define =factorial= in terms of =product=. Also use product to compute approximations to \pi  using the formula.
;; : \pi / 4 = (2/3) * (4/3) * (4/5) * ...

;; 2. If your =product= procedure generates a recursive process, write one that generates an iterative process. If it generates an iterative process, write one that generates a recursive process.

;;make the product
(define (product term next)
    (define (p-iter a b s)
        (if (> a b)
            s
            (p-iter (next a) b (* (term a) s))))
  (lambda (a b) (p-iter a b 1)))

;;upper parts of the function
(define (upper x) (* 2 (+ 1 (ceiling (/ x 2.0)))))
;;bottom parts of the function
(define (bottom x) (- (upper (+ x 1)) 1))
;;test to see whether it is correct
(define (test-ub)
    (newline)
  (display (map upper (list 0 1 2 3 4 5 6 7)))
  (newline)
  (display (map bottom (list 0 1 2 3 4 5 6 7)))
  (newline))

(define (increment x) (+ 1 x))
(define (pi-each x) (/ (upper x) (bottom x)))
(define (pi b) (* 4 ((product pi-each increment) 0.0 b)))

(define pical (map pi (list 10 100 1000 10000)))
(define (cal-precision x r) (* 100.0 (/ (abs (- x r)) r)))
(newline)
(display pical)
(newline)
(display (map (lambda (x) (cal-precision x 3.1415926)) pical))
(newline)
