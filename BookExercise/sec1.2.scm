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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Ex 1.21
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smallest divisor
(define (divides? n k) (= (remainder n k) 0))

(define (smallest-dividor n)
  (define (sd-i k)
    (cond ((> (square k) n) n)
          ((divides? n k) k)
          (else
           (sd-i (+ k 1)))))
  (sd-i 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Ex 1.22
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;find prime in normal ways
(define (test-prime n count)
  (define start (current-milliseconds))

  (define (fp k count)
    (cond ((= count 0) (rt (current-milliseconds)))
          ((= (smallest-dividor k) k) (dpg k count))
          (else
           (fp (+ k 1) count))))
  (define (rt end)
    (display (- end start))
    (newline))

  ;display and go to next level
  (define (dpg k count)
;    (display k)
;    (display "\t")
    (fp (+ k 1) (- count 1)))

  (fp n count))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Ex 1.23
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;another version of the smallest dividor
(define (smallest-dividor-two n)
  ;Next function to skip even number
  (define (next n)
    (if (= n 2)
        3
        (+ n 2)))
  (define (sm-i k)
    (cond ((> (square k) n) n)
          ((divides? n k) k)
          (else (sm-i (next k)))))
  (sm-i 2))

(define (test-find-prime num func)
  ;test for the prime number
  (define (tf n)
    (= (func n) n))

 ;denote the start time
  (define start (current-milliseconds))

 ;iteratively go up to num
  (define (time k)
    (cond ((= k num) (rt (current-milliseconds)))
          ((tf k) (dpg k))
          (else
           (time (+ k 1)))))

  ;display and go to next level
  (define (dpg k)
;    (display k)
;    (display "\t")
    (time (+ k 1)))

 ;report the answer
  (define (rt end)
    (newline)
    (display "With ")
    (display (- end start))
    (display " ms , found prime number ")
    (display " out of ")
    (display num)
    (newline))

  (time 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Ex 1.24
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;exponent module
(define (expmod bas n p)
  (define (eiter left bas n p)
    (cond ((= n 1)
           (remainder (* left bas) p))
          ((even? n)
           (eiter left (remainder (square bas) p) (/ n 2) p))
          (else
           (eiter (remainder (* left bas) p) bas (- n 1) p))))
  (eiter 1 bas n p))

;test for prime number
(define (fermat? n k)
  (= (expmod k n n) k))

;Prime test
(define (prime? n times)
  (cond ((= times 0)
         #t)
        ((fermat? n (random (- n 1)))
         (prime? n (- times 1)))
        (else
         #f)))

(define (time-find-prime n)
  (newline)
  (start-prime-test n (current-milliseconds) 0))

;n is a odd number
(define (start-prime-test n starttime count)
  (cond ((= count 12)
         (report-prime-test "" (- (current-milliseconds) starttime)))
        ((prime? n 20)
         (start-prime-test (+ n 1) starttime (+ count 1)))
        (else
         (start-prime-test (+ n 1) starttime count))))

;report
(define (report-prime-test n time)
  (display n)
  (display " *** ")
  (display time)
  (display "ms\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Ex 1.27
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Don't know how to control the squences!!!!!!!!
; The Miller-Rabin test for primality
(define (mrexp bas exp n)
  (cond ((= exp 0) 1)
        ((even? exp)
         (let ((calculate (mrexp bas (/ exp 2) n)) (r1 (remainder calculate n)) (sqcal (square calculate)) (r2 (remainder sqcal n)))
           (cond ((= calculate 0)
                  0)
                 ((and (= r2 1) (not (= r1 1)) (not (= r1 (- n 1))))
                  0)
                 (else
                  r2))))
        (else
         (remainder (* bas (mrexp bas (- exp 1) n)) n))))

(define (mr n k)
  (define tmp (+ (random (- n 2)) 1))
  (cond ((= k 0)
         #t)
        ((= (mrexp tmp (- n 1) n) 0)
         #f)
        (else (mr n (- k 1)))))
