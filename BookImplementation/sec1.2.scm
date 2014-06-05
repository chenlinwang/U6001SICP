;;Basics
(define (square n) (* n n))

(define (even? n)
  (= (remainder n 2) 0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Factorial
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Doing it downwards
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

;; Doing it upwards
(define (factorial-up n)
  ;; A recursion to mulitply until reach n
  (define (up result next)
    (if (= next n)
        (* result n)
        (up (* result next) (+ next 1))))
  (up 1 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Fibonacci
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (Fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else
         (+ (Fib (- n 1)) (Fib (- n 2))))))

(define (Fib-I n)
  (define (Fib-iter former latter n)
    (if (= n 0)
        former
        (Fib-iter latter (+ former latter) (- n 1))))
  (Fib-iter 0 1 n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Count the change
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (Count money kind)
  (define (money-table kind)
    (cond ((= kind 0) 0)
          ((= kind 1) 1)
          ((= kind 2) 5)
          ((= kind 3) 10)
          ((= kind 4) 20)
          ((= kind 5) 50)
          ((= kind 6) 100)
          (else 0)))
  (cond ((or (< money 0) (= kind 0)) 0)
        ((= money 0) 1)
        (else (+ (Count (- money (money-table kind)) kind) (Count money (- kind 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Exponent
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Ordinary Way of Computing
(define root 2)
; recursive process
(define (ex-r n)
  (if (= n 1)
      root
      (* root (ex-r (- n 1)))))
; iterative process
(define (ex-i n)
  (define (ex-iter sum n)
    (if (= n 0)
        sum
        (ex-iter (* root sum) (- n 1))))
  (ex-iter 1 n))

;;Fast Way of Computing
(define (ex-f n)
  (define (square n) (* n n))
  ;Test for even number
  (define (even? n)
    (= (remainder n 2) 0))
  ;Begin process
  (cond ((= n 0) 1)
        ((even? n) (square (ex-f (/ n 2))))
        (else (* root (ex-f (- n 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;GCD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Great Common divider
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;prime test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;for the normal one
(define (pt-n n)
;Test if it is a divider
  (define (test-divider k n)
    (= (remainder n k) 0))
  (define (pt-iter k n)
    (cond ((> (square k) n) n)
          ((test-divider k n) k)
          (else
           (pt-iter (+ k 1) n))))
  (pt-iter 2 n))

;;for the fermat test
;recursive process
(define (expmod-r bas n p)
  (cond ((= n 1)
         (remainder bas p))
        ((even? n)
         (remainder (expmod-r (remainder (square bas) p) (/ n 2) p) p))
        (else
         (remainder (* bas (expmod-r bas (- n 1) p)) p))))

;iterative process
(define (expmod-i bas n p)
  (define (exp-iter left bas n p)
    (cond ((= n 1)
           (remainder (* left bas) p))
          ((even? n)
           (exp-iter left (remainder (square bas) p) (/ n 2) p))
          (else
           (exp-iter (remainder (* left bas) p) bas (- n 1) p))))
  (exp-iter 1 bas n p))

(define (ft n times)
  (define testnum (random (- n 1)))
  (cond ((= times 0) #t)
        ((= (expmod-i testnum n n) testnum)
         (ft n (- times 1)))
        (else
         #f)))

(define (ft-s n times)
  (define testnum (random (- n 1)))
  (cond ((= times 0) #t)
        ((= (expmod-i testnum (- n 1) n) 1)
         (ft-s n (- times 1)))
        (else
         #f)))
