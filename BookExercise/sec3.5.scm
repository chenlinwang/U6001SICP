(load "../BookExercise/basic")
(define (loadexe35) (load "sec3.5"))
(load "../BookImplementation/sec3.5")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3.50
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Complete the following definition, which generalizes stream-map to allow procedures that take multiple arguments, analogous to map in section 2.2.3, footnote 12.

(define (stream-map proc . argstreams)
  ;; (write "stream-map-argstreams:")
  ;; (print argstreams)
  (if (any (map stream-null? argstreams))
      the-empty-stream
      (cons
       (apply proc (map (lambda (s) (stream-car s)) argstreams))
       (memo-proc (lambda ()
                    (apply stream-map
                           (cons proc (map (lambda (s) (stream-cdr s))
                                           argstreams))))))))

;; ;; test
;; (define int1to3 (stream-enumerate-interval 1 3))
;; ;;(define t (stream-map (lambda (n) n) int1to3))
;; ;; (print t)
;; ;; (print (stream-cdr t))

;; ;; single
;; (display-stream (stream-map (lambda (n) n) int1to3))
;; ;; double
;; (display-stream (stream-map + int1to3 int1to3))
;; ;; triple
;; (display-stream (stream-map + int1to3 int1to3 int1to3))
;; (exit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3.51
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; In order to take a closer look at delayed evaluation, we will use the following procedure, which simply returns its argument after printing it:
;; (define (show x)
;;   (print x)
;;   x)
;; ;; What does the interpreter print in response to evaluating each expression in the following sequence?
;; (define x (stream-map show (stream-enumerate-interval 0 10)))
;; (stream-ref x 5)
;; (stream-ref x 7)
;; (exit)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3.52
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (define sum 0)
;; (print sum)
;; (define (accum x)
;;   (set! sum (+ x sum))
;;   sum)
;; (print accum)
;; (define seq (stream-map accum (stream-enumerate-interval 1 20)))
;; (print seq)
;; (define y (stream-filter even? seq))
;; (print y)
;; (define z (stream-filter (lambda (x) (= (remainder x 5) 0))
;; seq))
;; (print z)
;; (print (stream-ref y 7))
;; (print (display-stream z))
;; (exit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3.54
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define a procedure mul-streams, analogous to add-streams, that produces the elementwise product of its two input streams. Use this together with the stream of integers to complete the following definition of the stream whose nth element (counting from 0) is n + 1 factorial:

(define factorials (cons 1 (memo-proc (lambda ()
                                        (stream-multiply (intsinitwith 2)
                                                         factorials)))))

;; ;; test
;; (display-stream-ref factorials 6)
;; (exit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3.55
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Define a procedure partial-sums that takes as argument a stream S and returns the stream whose elements are S0, S0 + S1, S0 + S1 + S2, .... For example, (partial-sums integers) should be the stream 1, 3, 6, 10, 15, ....

(define partial-sums (cons 0 (memo-proc (lambda ()
                                          (stream-plus (intsinitwith 1)
                                                       partial-sums)))))

;; ;; test
;; (display-stream-ref partial-sums 5)
;; (exit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3.56
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A famous problem, first raised by R. Hamming, is to enumerate, in ascending order with no repetitions, all positive integers with no prime factors other than 2, 3, or 5. One obvious way to do this is to simply test each integer in turn to see whether it has any factors other than 2, 3, and 5. But this is very inefficient, since, as the integers get larger, fewer and fewer of them fit the requirement. As an alternative, let us call the required stream of numbers S and notice the following facts about it.
;; 1. S begins with 1.
;; 2. The elements of (scale-stream S 2) are also elements of S.
;; 3. The same is true for (scale-stream S 3) and (scale-stream 5 S).
;; 4. These are all the elements of S.
;; Now all we have to do is combine elements from these sources. For this we define a procedure merge that combines two ordered streams into one ordered result stream, eliminating repetitions:
;; #+BEGIN_SRC scheme
;; (define (merge s1 s2)
;;   (cond ((stream-null? s1) s2)
;;         ((stream-null? s2) s1)
;;         (else
;;          (let ((s1car (stream-car s1))
;;                (s2car (stream-car s2)))
;;            (cond ((< s1car s2car)
;;                   (cons s1car (memo-proc (lambda ()
;;                                            (merge (stream-cdr s1) s2)))))
;;                  ((> s1car s2car)
;;                   (cons s2car (memo-proc (lambda ()
;;                                            (merge s1 (stream-cdr s2))))))
;;                  (else
;;                   (cons s1car
;;                         (memo-proc (lambda ()
;;                                      (merge (stream-cdr s1)
;;                                             (stream-cdr s2)))))))))))
;; #+END_SRC
;; Then the required stream may be constructed with merge, as follows:
;; #+BEGIN_SRC scheme
;; (define S (cons 1 (memo-proc (lambda ()
;;                                (merge (stream-scale S 2)
;;                                       (merge (stream-scale S 3)
;;                                              (stream-scale S 5)))))))
;; test
;; (display-stream-ref S 15)
;; (exit)
;; #+END_SRC
;; Fill in the missing expressions in the places marked <??> above.


;; The text gives only merge between two streams, let's expand it to multiple first with repetition.

;; stream merger according to special function
(define (stream-merge order same? . streams)
  ;; (write "all streams:")
  ;; (print streams)
  (cond ((null? streams) the-empty-stream)
        ((let* ((non-null-streams (filter (lambda (s) (not (stream-null? s)))
                                          streams))
                ;; (tmp (begin (write "non-null-streams:")
                ;;             (print non-null-streams)))
                (chosen-element (apply order (map stream-car non-null-streams))))
           ;; (write "chosen element:")
           ;; (print chosen-element)
           ;; (print "ok")
           (cons chosen-element (memo-proc (lambda ()
                                             (let ((rest-streams (foundmap (lambda (s) (same? chosen-element (stream-car s)))
                                                                           cons stream-cdr the-empty-stream non-null-streams)))
                                               (apply stream-merge (cons order (cons same? rest-streams)))))))))))

;; ;; test
;; (define intsinitwith0 (intsinitwith 0))
;; (define intsinitwith1 (intsinitwith 1))
;; (define int0to3 (stream-enumerate-interval 0 3))
;; (define m (stream-merge min = intsinitwith0 intsinitwith1 int0to3))
;; (display-stream-ref m 15)
;; (exit)

(define (stream-remove-redundant stream same?)
  (cons (stream-car stream)
        (memo-proc (lambda ()
                     (stream-remove-redundant (stream-filter (lambda (n) (not (same? n (stream-car stream))))
                                                             (stream-cdr stream))
                                              same?)))))

;; (define s (stream-remove-redundant (cons 1
;;                                          (memo-proc (lambda ()
;;                                                       (cons 2 (memo-proc (lambda ()
;;                                                                            (stream-merge min
;;                                                                                          =
;;                                                                                          (stream-scale s 2)
;;                                                                                          (stream-scale s 3)
;;                                                                                          (stream-scale s 5))))))))
;;                                    =))

(define s (stream-remove-redundant (cons 1 (memo-proc (lambda ()
                                                        (stream-merge min
                                                                      =
                                                                      (stream-scale s 2)
                                                                      (stream-scale s 3)
                                                                      (stream-scale s 5)))))
                                   =))
;; (print 'start)
;; (print (stream-cdr s))
;; (print 'end)
;; (display-stream-ref s 15)
;; (exit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3.59
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; In section 2.5.3 we saw how to implement a polynomial arithmetic system representing polynomials as lists of terms. In a similar way, we can work with power series, such as whose elements are the coefficients a0, a1, a2, a3, ....

;; a. The integral of the series a0 + a1 x + a2 x2 + a3 x3 +···is the series where c is any constant. Define a procedure integrate-series that takes as input a stream a0, a1, a2, ... representing a power series and returns the stream a0, (1/2)a1, (1/3)a2, ... of coefficients of the non-constant terms of the integral of the series. (Since the result has no constant term, it doesn't represent a power series; when we use integrate-series, we will cons on the appropriate constant.)
;; b. The function x ￼ ex is its own derivative. This implies that ex and the integral of ex are the same series, except for the constant term, which is e0 = 1. Accordingly, we can generate the series for ex as
;; (define exp-series
;;   (cons-stream 1 (integrate-series exp-series)))

;; Show how to generate the series for sine and cosine, starting from the facts that the derivative of sine is cosine and the derivative of cosine is the negative of sine:
;; (define cosine-series
;;   (cons-stream 1 <??>))
;; (define sine-series
;;   (cons-stream 0 <??>))
;; (define exp-series
;;   (cons-stream 1 (integrate-series exp-series)))

;; a
(define (integrate-series stream)
  (stream-divide stream (intsinitwith 1)))
;; ;; test
;; (define tmp (integrate-series ones))
;; (display-stream-ref tmp 10)
;; (exit)

(define exp-series (cons 1 (memo-proc (lambda ()
                                        (integrate-series exp-series)))))
;; ;; test
;; (display-stream-ref exp-series 15)
;; (exit)

;; b
(define cosine-series
  (cons 1 (memo-proc (lambda ()
                       (stream-scale (integrate-series sine-series)
                                     -1)))))
(define sine-series
  (cons 0 (memo-proc (lambda ()
                       (integrate-series cosine-series)))))

;; ;; ;; test
;; (print "the cosine:")
;; (display-stream-ref cosine-series 10)
;; (print "the sine:")
;; (display-stream-ref sine-series 10)
;; (exit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3.60
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; With power series represented as streams of coefficients as in exercise 3.59, adding series is implemented by add-streams. Complete the definition of the following procedure for multiplying series:

;; (define (mul-series s1 s2)
;;   (cons-stream <??> (add-streams <??> <??>)))

;; You can test your procedure by verifying that sin^{2}x + cos^{2}x = 1, using the series from exercise 3.59.

(define (mul-series s1 s2)
  (cons (* (stream-car s1)
           (stream-car s2))
        (memo-proc (lambda ()
                     (stream-plus (stream-scale (stream-cdr s1)
                                                (stream-car s2))
                                  (mul-series s1
                                              (stream-cdr s2)))))))

;; ;; test
;; (define one-sum (stream-plus (mul-series sine-series sine-series)
;;                              (mul-series cosine-series cosine-series)))
;; (print "one-sum:")
;; (display-stream-ref one-sum 10)

;; (define powerx (exponient-stream 0.5))

;; (define one-accumulate (stream-integral (stream-multiply one-sum
;;                                                         powerx)
;;                                        0))

;; (print "ones:")
;; (display-stream-ref one-accumulate 10)
;; (exit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3.61
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Let S be a power series (exercise 3.59) whose constant term is 1. Suppose we want to find the power series 1/S, that is, the series X such that S · X = 1. Write S = 1 + SR where SR is the part of S after the constant term. Then we can solve for X as following.
;; n other words, X is the power series whose constant term is 1 and whose higher-order terms are given by the negative of SR times X. Use this idea to write a procedure invert-unit-series that computes 1/S for a power series S with constant term 1. You will need to usemul-seriesfrom exercise 3.60.

(define (invert-unit-series stream)
  (cond ((= (stream-car stream) 0)
         (error "first element equal to 0 -- " (stream-car stream)))
        (else
         (let ((scalar (/ 1 (stream-car stream)))
               (stream-rest (stream-cdr stream))
               (invert-stream #f))
           (set! invert-stream (cons scalar
                                     (memo-proc (lambda ()
                                                  (stream-scale (mul-series stream-rest
                                                                            invert-stream)
                                                                (- scalar))))))
           invert-stream))))

;; ;; test
;; (define invert-cosine-series (invert-unit-series cosine-series))
;; (print "invert cosine series:")
;; (display-stream-ref invert-cosine-series 10)
;; (define power2 (exponient-stream 0.5))
;; (define one-sum (mul-series cosine-series
;;                             invert-cosine-series))
;; (define one-accumulate (stream-integral (stream-multiply power2 one-sum) 0))
;; (print "one accumulate:")
;; (display-stream-ref one-accumulate 10)
;; (exit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3.62
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use the results of exercises 3.60 and 3.61 to define a procedure div-series that divides two power series. Div-series should work for any two series, provided that the denominator series begins with a nonzero constant term. (If the denominator has a zero constant term, then div-series should signal an error.) Show how to use div-series together with the result of exercise 3.59 to generate the power series for tangent.

;; Division is just multiplication with the invert.

(define (div-series s1 s2)
  (let ((invert-s2 (invert-unit-series s2)))
    (mul-series s1 invert-s2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3.63
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Louis Reasoner asks why the sqrt-stream procedure was not written in the following more straightforward way, without the local variable guesses:
;; (define (sqrt-stream x)
;;   (cons-stream 1.0
;;                (stream-map (lambda (guess)
;;                              (sqrt-improve guess x))
;;                            (sqrt-stream x))))
;; Alyssa P. Hacker replies that this version of the procedure is considerably less efficient because it performs redundant computation. Explain Alyssa's answer. Would the two versions still differ in efficiency if our implementation of delay used only (lambda () <exp>) without using the optimization provided by memo-proc (section 3.5.1)?

;; The process uses a new =sqrt-stream= for map every time, thus it repeatedly recalculate the sequence again. So for one iteration, it needs to redo from the first x, wasting a lot of efficiency since the values are all memorized. It is of the same order of the growth as the non-memorized version of the lazy evaluation. We could proof it by:
(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (let iter ((former 1.0))
    (cons former
          (memo-proc (lambda ()
                       (iter (sqrt-improve former x)))))))

;; test
;; (define sqrt2 (sqrt-stream 2))
;; (display-stream-ref sqrt2 15)
;; (exit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3.64
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write a procedure stream-limit that takes as arguments a stream and a number (the tolerance). It should examine the stream until it finds two successive elements that differ in absolute value by less than the tolerance, and return the second of the two elements. Using this, we could compute square roots up to a given tolerance by
(define (stream-limit s tolerance)
  (let ((next-s (stream-cdr s)))
    (cond ((stream-null? next-s) (stream-car s))
          ((< (square (- (stream-car s)
                         (stream-car next-s)))
              tolerance) (stream-car next-s))
          (else (stream-limit next-s tolerance)))))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

;; test
;; (print (sqrt 2 0.01))
;; (exit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3.65
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use the series￼to compute three sequences of approximations to the natural logarithm of 2, in the same way we did above for \ln. How rapidly do these sequences converge?
(define (ln2-sum n)
  (cons (/ 1 n)
        (memo-proc (lambda ()
                     (stream-scale (ln2-sum (+ n 1))
                                   -1)))))

(define ln-sum-sequence (ln2-sum 1))

(define ln2-stream (stream-integral ln-sum-sequence
                                    0))
(define ln2-aitken (aitken-delte-square ln2-stream))
(define ln2-aitken-recursion (accelerate-stream ln2-stream aitken-delte-square))

;; (print "origin:")
;; (display-stream-ref ln2-stream 5)
;; (print "aitken:")
;; (display-stream-ref ln2-aitken 5)
;; (print "repeated aitken:")
;; (display-stream-ref ln2-aitken-recursion 5)
;; (exit)
