;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Ex1.1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 10
10
; 12
(+ 5 4 3)
; 8
(- 9 1)
; 3
(/ 6 2)
; 6
( + (* 2 4) (- 4 6))
; 3
(define a 3)
; 4
(define b (+ a 1))
; 19
(+ a b (* a b))
; #f
(= a b)
; 4
(if (and (> b a) (< b (* a b)))
    b
    a)
; 16
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
; 6
(+ 2 (if (> b a) b a))
; 12
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (* a 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Ex1.2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 3)))))
   (* 3 (- 6 2) (- 2 7)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Ex1.3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;What I am trying to do is to find the smallest number fisrt and then subtract its square with the sum of the squares of the numbers
;;Define square
(define (square x) (* x x))
;;Find minimum in two numbers
(define (min2 x y) (if (> x y) y x))
;;Find minimum in three numbers
(define (min3 x y z) (min2 (min2 x y) z))
;;Result
(define (ex1.3 x y z) (- (+ (square x) (square y) (square z))
                         (square (min3 x y z))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Ex1.4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (a-plus-abs-b a b)
      ((if (> b 0) + -) a b))
;;The a-plus-abs-b first test whether b is positive, if so, it will use plus, else it will use the minus

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Ex1.5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (p) (p))
(define (test x y)
    (if (= x 0)
        ￼0
        y))

;; (test 0 (p))

;; For applicative-order, it will end in infinite looping. As the order evaluates, it will first try to evaluate (p) and it results in (p) and then it will go like this forever.
;; For the normal-order, it will end in giving the correct answer 0. As the order evaluates , it will try to evaluate the test function and then the if-clause terminates the funcation and return 0.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Ex1.6
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (define (new-if predicate then-clause else-clause)
;;       (cond (predicate then-clause)
;;             (else else-clause)))

;; (define (sqrt-iter guess x)
;;     (new-if (good-enough? guess x)
;;             guess
;;             (sqrt-iter (improve guess x)
;;                        ￼x)))

;; For similar reasons as Ex1.5, the function will end in infinite looping. As the applicative-order goes, when it try to evaluate the first layer, it will first evaluate good-enough? function and then evaluate sqrt-iter, and then evaluate improve and then good-enough?, for now the new-if is simply a function that needs to be evaluated. In this case it will never be evaluated, thus infinite looping.
;;
;; sqrt-iter -> good-enough? -> sqrt-iter -> improve
;;                ^                            |
;;                |                            |
;;                ------------------------------
