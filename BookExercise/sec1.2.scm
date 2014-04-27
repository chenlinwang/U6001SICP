;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 1.10
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;The following procedure computes a mathematical function called Ackermann's function.

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

;;Give concise mathematical definitions for the functions computed by the procedures f, g, and h for positive integer values of n. For example, (k n) computes 5n2.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 1.11
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;A function f is defined by the rule that f(n) = n if n<3 and f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n> 3. Write a procedure that computes f by means of a recursive process. Write a procedure that computes f by means of an iterative process.

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
;; Exercise  1.12
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;The following pattern of numbers is called Pascal's triangle.

;;The numbers at the edge of the triangle are all 1, and each number inside the triangle is the sum of the two numbers above it. Write a procedure that computes elements of Pascal's triangle by means of a recursive process.

;;Pasca Triangle
(define (p-r n i)
  (if (or (= i 1) (= i n))
      1
      (+ (p-r (- n 1) (- i 1)) (p-r (- n 1) i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise  1.15
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;The sine of an angle (specified in radians) can be computed by making use of the approximation sinx =￼x if x is sufficiently small, and the trigonometric identity:

;; sin r = 3 sin r/3  - 4 sin^3 r/3

;; to reduce the size of the argument of sin. (For purposes of this exercise an angle is considered ``sufficiently small'' if its magnitude is not greater than 0.1 radians.) These ideas are incorporated in the following procedures:

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
;; Exercise  1.16
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Design a procedure that evolves an iterative exponentiation process that uses successive squaring and uses a logarithmic number of steps, as does fast-expt. (Hint: Using the observation that
;; (b^{n/2})^2 = (b^2){n/2}, keep, along with the exponent n and the base b, an additional state variable a, and define the state transformation in such a way that the product a bn is unchanged from state to state. At the beginning of the process a is taken to be 1, and the answer is given by the value of a at the end of the process. In general, the technique of defining an invariant quantity that remains unchanged from state to state is a powerful way to think about the design of iterative algorithms.)

;;iterative version of the fast exponent
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
;; Exercise  1.17 1.18
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The exponentiation algorithms in this section are based on performing exponentiation by means of repeated multiplication. In a similar way, one can perform integer multiplication by means of repeated addition. The following multiplication procedure (in which it is assumed that our language can only add, not multiply) is analogous to the expt procedure:

;; (define (* a b)
;;   (if (= b 0)
;;       0
;;       (+ a (* a (- b 1)))))

;; This algorithm takes a number of steps that is linear in b. Now suppose we include, together with addition, operations double, which doubles an integer, and halve, which divides an (even) integer by 2. Using these, design a multiplication procedure analogous to fast-expt that uses a logarithmic number of steps.

;; Using the results of exercises 1.16 and 1.17, devise a procedure that generates an iterative process for multiplying two integers in terms of adding, doubling, and halving and uses a logarithmic number of steps.40

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
;; Exercise  1.19
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; There is a clever algorithm for computing the Fibonacci numbers in a logarithmic number of steps. Recall the transformation of the state variables a and b in the fib-iter process of section 1.2.2: a \leftarrow a + b and b \leftarrow a. Call this transformation T, and observe that applying T over and over again n times, starting with 1 and 0, produces the pair Fib(n + 1) and Fib(n). In other words, the Fibonacci numbers are produced by applying Tn, the nth power of the transformation T, starting with the pair (1,0). Now consider T to be the special case of p = 0 and q = 1 in a family of transformations Tpq, where T_{pq}
;; transforms the pair (a,b) according to a \leftarrow￼ bq + aq + ap and b \leftarrow￼ bp + aq. Show that if we apply such a transformation Tpq twice, the effect is the same as using a single transformation T_{p'q'} of the same form, and compute p' and q' in terms of p and q. This gives us an explicit way to square these transformations, and thus we can compute T^n using successive squaring, as in the =fast-expt= procedure. Put this all together to complete the following procedure, which runs in a logarithmic number of steps:

;;square
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
;; Exercise  1.21
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
;; Exercise  1.22
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Most Lisp implementations include a primitive called runtime that returns an integer that specifies the amount of time the system has been running (measured, for example, in microseconds). The following timed-prime-test procedure, when called with an integer n, prints n and checks to see if n is prime. If n is prime, the procedure prints three asterisks followed by the amount of time used in performing the test.

;; (define (timed-prime-test n)
;;   (newline)
;;   (display n)
;;   (start-prime-test n (runtime)))
;; (define (start-prime-test n start-time)
;;   (if (prime? n)
;;       (report-prime (- (runtime) start-time))))
;; (define (report-prime elapsed-time)
;;   (display " *** ")
;;   (display elapsed-time))

;; Using this procedure, write a procedure search-for-primes that checks the primality of consecutive odd integers in a specified range. Use your procedure to find the three smallest primes larger than 1000; larger than 10,000; larger than 100,000; larger than 1,000,000. Note the time needed to test each prime.
;; Since the testing algorithm has order of growth of \Theta(sqrt n), you should expect that testing for primes around 10,000 should take about sqrt 10 times as long as testing for primes around 1000. Do your timing data bear this out? How well do the data for 100,000 and 1,000,000 support the sqrt￼n prediction? Is your result compatible with the notion that programs on your machine run in time proportional to the number of steps required for the computation?

;;find prime in normal ways
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
;; Exercise  1.23
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;The smallest-divisor procedure shown at the start of this section does lots of needless testing: After it checks to see if the number is divisible by 2 there is no point in checking to see if it is divisible by any larger even numbers. This suggests that the values used for test-divisor should not be 2, 3, 4, 5, 6, ..., but rather 2, 3, 5, 7, 9, .... To implement this change, define a procedure next that returns 3 if its input is equal to 2 and otherwise returns its input plus 2. Modify the smallest- divisor procedure to use (next test-divisor) instead of (+ test-divisor 1). With timed-prime-test incorporating this modified version of smallest-divisor, run the test for each of the 12 primes found in exercise 1.22. Since this modification halves the number of test steps, you should expect it to run about twice as fast. Is this expectation confirmed? If not, what is the observed ratio of the speeds of the two algorithms, and how do you explain the fact that it is different from 2?

;;another version of the smallest dividor
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
;; Exercise  1.24
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Modify the timed-prime-test procedure of exercise 1.22 to use fast-prime? (the Fermat method), and test each of the 12 primes you found in that exercise. Since the Fermat test has \Thate(\log n) growth, how would you expect the time to test primes near 1,000,000 to compare with the time needed to test primes near 1000? Do your data bear this out? Can you explain any discrepancy you find?

;;exponent module
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
;; Exercise  1.28
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;One variant of the Fermat test that cannot be fooled is called the Miller-Rabin test (Miller 1976; Rabin 1980). This starts from an alternate form of Fermat's Little Theorem, which states that if n is a prime number and a is any positive integer less than n, then a raised to the (n - 1)st power is congruent to 1 modulo n. To test the primality of a number n by the Miller-Rabin test, we pick a random number a<n and raise a to the (n - 1)st power modulo n using the expmod procedure. However, whenever we perform the squaring step in expmod, we check to see if we have discovered a ``nontrivial square root of 1 modulo n,'' that is, a number not equal to 1 or n - 1 whose square is equal to 1 modulo n. It is possible to prove that if such a nontrivial square root of 1 exists, then n is not prime. It is also possible to prove that if n is an odd number that is not prime, then, for at least half the numbers a<n, computing a^{n-1} in this way will reveal a nontrivial square root of 1 modulo n. (This is why the Miller-Rabin test cannot be fooled.) Modify the expmod procedure to signal if it discovers a nontrivial square root of 1, and use this to implement the Miller-Rabin test with a procedure analogous to fermat-test. Check your procedure by testing various known primes and non-primes. Hint: One convenient way to make expmod signal is to have it return 0.

; The Miller-Rabin test for primality
(define (mrexp bas exp n)
  (cond ((= exp 0) 1)
        ((even? exp)
         (begin (define calculate (mrexp bas (/ exp 2) n))
                (define r1 (remainder calculate n))
                (define sqcal (square calculate))
                (define r2 (remainder sqcal n))
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
