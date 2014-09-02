(load "../BookExercise/basic")
(define (loadexe31) (load ".../BookExercise/sec3.1.scm"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3.1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; An accumulator is a procedure that is called repeatedly with a single numeric argument and accumulates its arguments into a sum. Each time it is called, it returns the currently accumulated sum. Write a procedure make-accumulator that generates accumulators, each maintaining an independent sum. The input to make-accumulator should specify the initial value of the sum; for example:

;; (define A (make-accumulator 5))
;; (A 10)
;; 15
;; (A 10)
;; 25

(define (make-accumulator base)
  (lambda (addend)
    (set! base (+ base addend))
    base))

;; test
;; (define A (make-accumulator 5))
;; (print-out (A 10))
;; (print-out (A 10))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3.2
;; In software-testing applications, it is useful to be able to count the number of times a given procedure is called during the course of a computation. Write a procedure make-monitored that takes as input a procedure, f, that itself takes one input. The result returned by make-monitored is a third procedure, say mf, that keeps track of the number of times it has been called by maintaining an internal counter. If the input to mf is the special symbol how-many-calls?, then mf returns the value of the counter. If the input is the special symbol reset-count, then mf resets the counter to zero. For any other input, mf returns the result of calling f on that input and increments the counter. For instance, we could make a monitored version of the sqrt procedure:

;; (define s (make-monitored sqrt))
;; (s 100)
;; 10
;; (s 'how-many-calls?)
;; 1

(define (make-monitored f)
  (let ((count 0))

    (define (dispatch m . more)
      (define (do-f para)
        (set! count (+ count 1))
        (apply f para))

      (cond ((eq? m 'how-many-calls?) count)
            (else
             (do-f (cons m more)))))

    dispatch))

;; test
;; (define s (make-monitored sqrt))
;; (print-out (s 100))
;; (print-out (s 225))
;; (print-out (s 'how-many-calls?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3.3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modify the make-account procedure so that it creates password-protected accounts. That is, make-account should take a symbol as an additional argument, as in

;; (define acc (make-account 100 'secret-password))

;; The resulting account object should process a request only if it is accompanied by the password with which the account was created, and should otherwise return a complaint:

;; ((acc 'secret-password 'withdraw) 40)
;; 60
;; ((acc 'some-other-password 'deposit) 50)
;; "Incorrect password"

(define (make-account base real-passwd)

  (define (withdraw num)
    (cond ((> num base)
           "Not enough money!")
          (else
            (set! base (- base num))
            base)))

  (define (deposit num)
    (set! base (+ base num))
    base)

  (lambda (passwd m)
    (cond ((eq? passwd real-passwd)
           (cond ((eq? m 'withdraw) withdraw)
                 ((eq? m 'deposit) deposit)
                 (else (error "Undefined Command -- " m))))
          (else (lambda (x) "Incorrect password!")))))

;; (define acc (make-account 100 'dad))
;; (print-out ((acc 'dad 'withdraw) 30))
;; (print-out ((acc 'dad 'withdraw) 70))
;; (print-out ((acc 'dad 'withdraw) 50))
;; (print-out ((acc 'mon 'deposit) 30))
;; (print-out ((acc 'dad 'deposit) 30))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3.4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modify the make-account procedure of exercise 3.3 by adding another local state variable so that, if an account is accessed more than seven consecutive times with an incorrect password, it invokes the procedure call-the-cops.

(define (make-account-alert base real-passwd)

  (define wrongpasswd 0)

  (define (withdraw num)
    (cond ((> num base)
           "Not enough money!")
          (else
            (set! base (- base num))
            base)))

  (define (deposit num)
    (set! base (+ base num))
    base)

  (lambda (passwd m)
    (cond ((eq? passwd real-passwd)
           (set! wrongpasswd 0)
           (cond ((eq? m 'withdraw) withdraw)
                 ((eq? m 'deposit) deposit)
                 (else (error "Undefined Command -- " m))))
          (else (lambda (x)
                  (set! wrongpasswd (+ wrongpasswd 1))
                  (if (>= wrongpasswd 7)
                      "7 Consecutive Wrong Password! Calling the Police!"
                      "Incorrect password!"))))))

;; ;;Test
;; (define alert-count (make-account-alert 100 'abc))

;; (let iter ((num 0))
;;   (print-out ((alert-count 'a 'withdraw) 100))
;;   (if (= num 2)
;;       (print-out ((alert-count 'abc 'withdraw) 100)))
;;   (if (= num 9)
;;       (print-out "Over!")
;;       (iter (+ num 1))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3.5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; /Monte Carlo integration/ is a method of estimating definite integrals by means of Monte Carlo simulation. Consider computing the area of a region of space described by a predicate =P(x, y)= that is true for points =(x, y)= in the region and false for points not in the region. For example, the region contained within a circle of radius 3 centered at (5, 7) is described by the predicate that tests whether (x - 5)^{2} + (y - 7)^{2} < 32. To estimate the area of the region described by such a predicate, begin by choosing a rectangle that contains the region. For example, a rectangle with diagonally opposite corners at (2, 4) and (8, 10) contains the circle above. The desired integral is the area of that portion of the rectangle that lies in the region. We can estimate the integral by picking, at random, points (x,y) that lie in the rectangle, and testing P(x, y) for each point to determine whether the point lies in the region. If we try this with many points, then the fraction of points that fall in the region should give an estimate of the proportion of the rectangle that lies in the region. Hence, multiplying this fraction by the area of the entire rectangle should produce an estimate of the integral.

;; Implement Monte Carlo integration as a procedure estimate-integral that takes as arguments a predicate P, upper and lower bounds x_{1}, x_{2}, y_{1}, and y_{2} for the rectangle, and the number of trials to perform in order to produce the estimate. Your procedure should use the same monte-carlo procedure that was used above to estimate \pi. Use your estimate-integral to produce an estimate of \pi by measuring the area of a unit circle.

;; You will find it useful to have a procedure that returns a number chosen at random from a given range. The following random-in-range procedure implements this in terms of the random procedure used in section 1.2.6, which returns a nonnegative number less than its input.8

;; #+BEGIN_SRC scheme
;; (define (random-in-range low high)
;;   (let ((range (- high low)))
;;     (+ low (random range))))
;; #+END_SRC

(load "../BookImplementation/sec3.1.scm")

(define (random-in-range low1 high1 low2 high2)
  (let ((random-seed (random-mtzig:init random-init))
        (range1 (- high1 low1))
        (range2 (- high2 low2)))
    (lambda ()
      (cons (+ low1 (* range1 (random-mtzig:randu! random-seed)))
           (+ low2 (* range2 (random-mtzig:randu! random-seed)))))))

(define (make-integration-test x1 x2 y1 y2 formula)
  (let ((point-generator (random-in-range x1 x2 y1 y2)))
    (lambda ()
      (let ((p (point-generator)))
        (formula (car p) (cdr p))))))

(define (monte-carlo-integration x1 x2 y1 y2 formula trials)
  (* (* (- x2 x1) (- y2 y1))
     (monte-carlo trials
                  (make-integration-test x1 x2 y1 y2 formula))))

;; Test for the circle in the problem
;; (define (circle35 x y)
;;   (< (+ (square (- x 5)) (square (- y 7)))
;;      9))

;; (print-out (monte-carlo-integration 2 8 4 10 circle35 100000))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3.6
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; It is useful to be able to reset a random-number generator to produce a sequence starting from a given value. Design a new rand procedure that is called with an argument that is either the symbol generate or the symbol reset and behaves as follows: (rand 'generate) produces a new random number ; ((rand 'reset) <new-value>) resets the internal state variable to the designated <new-value>. Thus, by resetting the state, one can generate repeatable sequences. These are very handy to have when testing and debugging programs that use random numbers.

(define (new-rand random-init)
  (let ((random-seed (random-mtzig:init random-init)))

    (define (rand)
      (random-mtzig:random! random-seed))

    (define (dispatch m)
      (cond ((eq? m 'generate) (rand))
            ((eq? m 'reset)
             (lambda (new-init)
               (set! random-seed (random-mtzig:init new-init))
               "Resetting the Seed!"))
            (else (error "Undefined Operations for new-rand!" m))))
    dispatch))

;; ;; Test
;; (define r1 (new-rand 1234))
;; (print-out (r1 'generate))
;; (print-out (r1 'generate))
;; (print-out ((r1 'reset) 1234))
;; (print-out (r1 'generate))
;; (print-out (r1 'generate))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3.7
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Consider the bank account objects created by make-account, with the password modification described in exercise 3.3. Suppose that our banking system requires the ability to make joint accounts. Define a procedure make-joint that accomplishes this. Make-joint should take three arguments. The first is a password-protected account. The second argument must match the password with which the account was defined in order for the make-joint operation to proceed. The third argument is a new password. Make-joint is to create an additional access to the original account using the new password. For example, if peter-acc is a bank account with password open-sesame, then

;; (define paul-acc
;;   (make-joint peter-acc 'open-sesame 'rosebud))

;; will allow one to make transactions on peter-acc using the name paul-acc and the password rosebud. You may wish to modify your solution to exercise 3.3 to accommodate this new feature.


;; The response of the incorrect password is "Incorrect password!"
;; The function could be used for both normal account and the alert account
(define (make-joint account old-passwd new-passwd)

  (cond ((or (equal? "Incorrect password!"
                     ((account old-passwd 'withdraw) 0))
             (equal? "7 Consecutive Wrong Password! Calling the Police!"
                     ((account old-passwd 'withdraw) 0)))
         "Incorrect password for the old-account!")
        (else

         (define wrongpasswd 0)

         (lambda (passwd m)
           (cond ((eq? passwd new-passwd)
                  (set! wrongpasswd 0)
                  (account old-passwd m))
                 (else (lambda (x)
                         (set! wrongpasswd (+ wrongpasswd 1))
                         (if (>= wrongpasswd 7)
                             "7 Consecutive Wrong Password! Calling the Police!"
                             "Incorrect password!"))))))))

;; ;; Test
;; (define peter-acc (make-account-alert 1000 'peter))
;; (print-out (make-joint peter-acc 'abc 'a))
;; (define paul-acc (make-joint peter-acc 'peter 'paul))

;; (print-out ((paul-acc 'paul 'withdraw) 100))
;; (print-out ((paul-acc 'paul 'deposit) 150))

;; (print-out ((peter-acc 'peter 'withdraw) 150))
;; (print-out ((peter-acc 'peter 'deposit) 250))

;; (let iter ((time 8))
;;   (print-out ((paul-acc 'a 'withdraw) 100))
;;   (if (= 0 time)
;;       (print-out "done")
;;       (iter (- time 1))))

;; (let iter ((time 8))
;;   (print-out ((peter-acc 'a 'withdraw) 100))
;;   (if (= 0 time)
;;       (print-out "done")
;;       (iter (- time 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3.8
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; When we defined the evaluation model in section 1.1.3, we said that the first step in evaluating an expression is to evaluate its subexpressions. But we never specified the order in which the subexpressions should be evaluated (e.g., left to right or right to left). When we introduce assignment, the order in which the arguments to a procedure are evaluated can make a difference to the result. Define a simple procedure f such that evaluating (+ (f 0) (f 1)) will return 0 if the arguments to + are evaluated from left to right but will return 1 if the arguments are evaluated from right to left.

(define f1 (let ((p (cons 0 0)))
             (lambda (number)
               (set! p (cons number (car p)))
               (cdr p))))

;; Test
;; (print-out (+ (f1 0) (f1 1)))
;; (print-out (+ (f1 1) (f1 0)))

(exit)
