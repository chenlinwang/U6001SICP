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