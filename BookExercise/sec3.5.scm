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

;; (define powerx (exponent-stream 0.5))

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
;; (define power2 (exponent-stream 0.5))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3.66
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Examine the stream (pairs integers integers). Can you make any general comments about the order in which the pairs are placed into the stream? For example, about how many pairs precede the pair (1,100)? the pair (99,100)? the pair (100,100)? (If you can make precise mathematical statements here, all the better. But feel free to give more qualitative answers if you find yourself getting bogged down.)
;; ** Answer
;; It is easy to prove using the induction on k, and we have:
;; (s(k),t(k)) = T(2^{k+1}-2),
;; (s(k),t(k+l)) = T(2^{k+1}l+2^{k}-2),
;; (k > -1), (l > 0)

;; Thus:
;; (1,100) = (s(0),t(0+99)) = T(197)
;; (99,100) = (s(98),t(98+1)) = T(2^{98}3-2)
;; (100,100) = (s(99),t(99)) = T(2^{100}-2)

;; (print (stream-ref ordered-ints 197))
;; (print (stream-ref ordered-ints 94))
;; (print (stream-ref ordered-ints (- (* 3 (exponentiation 2 98)) 2))) too big to calculate
;; (print (stream-ref ordered-ints (- (exponentiation 2 100) 2))) too big to calculate
;; (exit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3.67
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modify the pairs procedure so that (pairs integers integers) will produce the stream of all pairs of integers (i,j) (without the condition i < j). Hint: You will need to mix in an additional stream.

;; Still we will implement it as four parts: the first diagnoal element, the row followed, the column followed and the rest.

(define (all-interleave first . rest)
  (cond ((null? rest) first)
        ((stream-null? first)
         (apply all-interleave rest))
        (else
         (cons (stream-car first)
               (memo-proc (lambda ()
                            (apply all-interleave (append rest (list (stream-cdr first))))))))))

;; ;; test
;; (define one-twos (all-interleave ones twos))
;; (define one-two-threes (all-interleave ones twos threes))
;; (display-stream-ref one-twos 10)
;; (print "\n")
;; (display-stream-ref one-two-threes 10)
;; (exit)

(define (all-pairs s t)
  (cons (cons (stream-car s)
              (stream-car t))
        (memo-proc (lambda ()
                     (all-interleave (stream-map (lambda (m) (cons (stream-car s) m))
                                                 (stream-cdr t))
                                     (stream-map (lambda (n) (cons n (stream-car t)))
                                                 (stream-cdr s))
                                     (all-pairs (stream-cdr s)
                                                (stream-cdr t)))))))

;; (define all-ints (all-pairs ints ints))
;; (display-stream-ref all-ints 20)
;; (exit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3.68
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Louis Reasoner thinks that building a stream of pairs from three parts is unnecessarily complicated. Instead of separating the pair (S0,T0) from the rest of the pairs in the first row, he proposes to work with the whole first row, as follows:

;; (define (pairs s t)
;;   (display "enter pair with:")
;;   (display s)
;;   (display t)
;;   (newline)
;;   (interleave
;;    (stream-map (lambda (x) (cons (stream-car s) x))
;;                t)
;;    (pairs (stream-cdr s) (stream-cdr t))))

;; Does this work? Consider what happens if we evaluate (pairs integers integers) using Louis's definition of pairs.

;; (print 'started)
;; (define l-ordered-pairs (pairs ints ints))
;; (print (stream-car l-ordered-pairs))
;; ;;(display-stream-ref l-ordered-pairs 15)
;; (exit)

;; as the applicative order, the scheme will go into pairs again and again with infinity looping.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3.69
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write a procedure triples that takes three infinite streams, S, T, and U, and produces the stream of triples (Si,Tj,Uk) such that i < j < k. Use triples to generate the stream of all Pythagorean triples of positive integers, i.e., the triples (i,j,k) such that i < j and i2 + j2 = k2.

;; we will have to use the ordered pairs
(define (ordered-triplet s t u)
  (cons (cons (stream-car s)
              (cons (stream-car t)
                    (stream-car u)))
        (memo-proc (lambda ()
                     (all-interleave (stream-map (lambda (x) (cons (stream-car s)
                                                                   x))
                                                 (stream-cdr (ordered-pairs t u)))
                                     (ordered-triplet (stream-cdr s)
                                                      (stream-cdr t)
                                                      (stream-cdr u)))))))

;; test
(define tri-ordered-ints (ordered-triplet ints ints ints))
;; (display-stream-ref tri-ordered-ints 20)
;; (exit)

(define pythogorean-triple (stream-filter (lambda (s) (let ((f (car s))
                                                            (s (cadr s))
                                                            (t (cddr s)))
                                                        (= (square t) (+ (square f)
                                                                         (square s)))))
                                          tri-ordered-ints))
;; (display-stream-ref pythogorean-triple 3)
;; (exit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3.70
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; It would be nice to be able to generate streams in which the pairs appear in some useful order, rather than in the order that results from an ad hoc interleaving process. We can use a technique similar to the merge procedure of exercise 3.56, if we define a way to say that one pair of integers is ``less than'' another. One way to do this is to define a ``weighting function'' W(i,j) and stipulate that (i1,j1) is less than (i2,j2) if W(i1,j1) < W(i2,j2). Write a procedure merge-weighted that is like merge, except that merge-weighted takes an additional argument weight, which is a procedure that computes the weight of a pair, and is used to determine the order in which elements should appear in the resulting merged stream. Using this, generalize pairs to a procedure weighted-pairs that takes two streams, together with a procedure that computes a weighting function, and generates the stream of pairs, ordered according to weight. Use your procedure to generate
;; a. the stream of all pairs of positive integers (i,j) with i < j ordered according to the sum i + j
;; b. the stream of all pairs of positive integers (i,j) with i < j, where neither i nor j is divisible by 2, 3, or 5, and the pairs are ordered according to the sum 2 i + 3 j + 5 i j.


(define (stream-compare-merge rank . streams)
  (cond ((null? streams) the-empty-stream)
        (else
         (let ((ordered-streams (ext-quick-sort streams (lambda (x)
                                                          (rank (stream-car x))))))
           (cons (stream-car (car ordered-streams))
                 (memo-proc (lambda ()
                              (apply stream-compare-merge (cons rank (cons (stream-cdr (car ordered-streams))
                                                                           (cdr ordered-streams)))))))))))

(define pair-rank (lambda (ele) (+ (car ele) (cdr ele))))

(define (ranked-pairs s t rank)
  (cons (cons (stream-car s)
              (stream-car (stream-cdr t)))
        (memo-proc (lambda ()
                     (stream-compare-merge rank
                                           (stream-map (lambda (x) (cons (stream-car s) x))
                                                       (stream-cdr (stream-cdr t)))
                                           (ranked-pairs (stream-cdr s)
                                                               (stream-cdr t)
                                                               rank))))))

;; test
(define sum-sequenced-int (ranked-pairs ints ints pair-rank))
;;(display-stream-ref sum-sequenced-int 10)
;;(exit)

;; the desired pair
(define desired-pair (stream-filter (lambda (s) (let ((f (car s))
                                                      (s (cdr s)))
                                                  (all (append (map (lambda (x) (not (divide? f x)))
                                                                    (list 2 3 5))
                                                               (map (lambda (x) (not (divide? s x)))
                                                                    (list 2 3 5))))))
                                    (ranked-pairs ints ints (lambda (ele) (+ (* 2 (car ele))
                                                                                   (* 3 (cdr ele))
                                                                                   (* 5 (car ele)
                                                                                      (cdr ele)))))))

;; (display-stream-ref desired-pair 10)
;; (exit)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3.71
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Numbers that can be expressed as the sum of two cubes in more than one way are sometimes called Ramanujan numbers, in honor of the mathematician Srinivasa Ramanujan.70 Ordered streams of pairs provide an elegant solution to the problem of computing these numbers. To find a number that can be written as the sum of two cubes in two different ways, we need only generate the stream of pairs of integers (i,j) weighted according to the sum i3 + j3 (see exercise 3.70), then search the stream for two consecutive pairs with the same weight. Write a procedure to generate the Ramanujan numbers. The first such number is 1,729. What are the next five?
(define (cube-sum ele)
  (+ (cube (car ele))
     (cube (cdr ele))))

(define ordered-by-cube-sum (ranked-pairs ints ints cube-sum))

(define (remanujan-stream stream lastpairs sum)
  (cond ((null? lastpairs) (remanujan-stream (stream-cdr stream)
                                             (list (stream-car stream))
                                             (cube-sum (stream-car stream))))
        ((= sum
            (cube-sum (stream-car stream)))
         (remanujan-stream (stream-cdr stream)
                           (cons (stream-car stream)
                                 lastpairs)
                           sum))
        ((> (length lastpairs) 1)
         (cons (cons sum lastpairs)
               (memo-proc (lambda ()
                            (remanujan-stream (stream-cdr stream)
                                              (list (stream-car stream))
                                              (cube-sum (stream-car stream)))))))
        (else
         (remanujan-stream (stream-cdr stream)
                           (list (stream-car stream))
                           (cube-sum (stream-car stream))))))

;; (define rs (remanujan-stream ordered-by-cube-sum (list) 0))
;; (display-stream-ref rs 4)
;; (exit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3.72
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; In a similar way to exercise 3.71 generate a stream of all numbers that can be written as the sum of two squares in three different ways (showing how they can be so written).

(define (square-sum ele)
  (+ (square (car ele))
     (square (cdr ele))))

(define ordered-by-square-sum (ranked-pairs ints ints square-sum))

(define (select-length-pairs len stream rank)
  (let iter ((stream stream)
             (lastpairs (list))
             (sum #f))
    (cond ((null? lastpairs)
           (iter (stream-cdr stream)
                 (list (stream-car stream))
                 (rank (stream-car stream))))
          ((= sum
              (rank (stream-car stream)))
           (iter (stream-cdr stream)
                 (cons (stream-car stream)
                       lastpairs)
                 sum))
          ((>= (length lastpairs) len)
           (cons (cons sum lastpairs)
                 (memo-proc (lambda ()
                              (iter (stream-cdr stream)
                                    (list (stream-car stream))
                                    (rank (stream-car stream)))))))
          (else
           (iter (stream-cdr stream)
                 (list (stream-car stream))
                 (rank (stream-car stream)))))))

(define triple-square-sum (select-length-pairs 3 ordered-by-square-sum square-sum))
;; (display-stream-ref triple-square-sum 1)
;; (exit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3.73
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Write a procedure RC that models this circuit. RC should take as inputs the values of R, C, and dt and should return a procedure that takes as inputs a stream representing the current i and an initial value for the capacitor voltage v0 and produces as output the stream of voltages v. For example, you should be able to use RC to model an RC circuit with R = 5 ohms, C = 1 farad, and a 0.5-second time step by evaluating (define RC1 (RC 5 1 0.5)). This defines RC1 as a procedure that takes a stream representing the time sequence of currents and an initial capacitor voltage and produces the output stream of voltages.

(define (RC r c dt)
  (lambda (v0 i)
    (stream-map +
                (constant-stream v0)
                (stream-scale (stream-integral-dt i 0 dt)
                              (/ 1 c))
                (stream-scale i r))))

(define rc1 (RC 5 1 0.5))
;; (display-stream-ref (rc1 0 ones) 10)
;; (exit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3.74
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Alyssa P. Hacker is designing a system to process signals coming from physical sensors. One important feature she wishes to produce is a signal that describes the zero crossings of the input signal. That is, the resulting signal should be + 1 whenever the input signal changes from negative to positive, - 1 whenever the input signal changes from positive to negative, and 0 otherwise. (Assume that the sign of a 0 input is positive.) For example, a typical input signal with its associated zero-crossing signal would be In Alyssa's system, the signal from the sensor is represented as a stream sense-data and the stream zero-crossings is the corresponding stream of zero crossings. Alyssa first writes a procedure sign- change-detector that takes two values as arguments and compares the signs of the values to produce an appropriate 0, 1, or - 1. She then constructs her zero-crossing stream as follows:
;; (define (make-zero-crossings input-stream last-value)
;;   (cons-stream
;;    (sign-change-detector (stream-car input-stream) last-value)
;;    (make-zero-crossings (stream-cdr input-stream)
;;                         (stream-car input-stream))))

;; (define zero-crossings (make-zero-crossings sense-data 0))
;; Alyssa's boss, Eva Lu Ator, walks by and suggests that this program is approximately equivalent to the following one, which uses the generalized version of stream-map from exercise 3.50:
;; (define zero-crossings
;;   (stream-map sign-change-detector sense-data <expression>))
;; Complete the program by supplying the indicated <expression>.

;; (define zero-crossings
;;   (stream-map sign-change-detector sense-data (stream-cons 0 sense-data)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3.75
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unfortunately, Alyssa's zero-crossing detector in exercise 3.74 proves to be insufficient, because the noisy signal from the sensor leads to spurious zero crossings. Lem E. Tweakit, a hardware specialist, suggests that Alyssa smooth the signal to filter out the noise before extracting the zero crossings. Alyssa takes his advice and decides to extract the zero crossings from the signal constructed by averaging each value of the sense data with the previous value. She explains the problem to her assistant, Louis Reasoner, who attempts to implement the idea, altering Alyssa's program as follows:

;; (define (make-zero-crossings input-stream last-value)
;;   (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
;;     (cons-stream (sign-change-detector avpt last-value)
;;                  (make-zero-crossings (stream-cdr input-stream)
;;                                       avpt))))

;; This does not correctly implement Alyssa's plan. Find the bug that Louis has installed and fix it without changing the structure of the program. (Hint: You will need to increase the number of arguments tomake- zero-crossings.)

(define (make-zero-crossings input-stream last-value last-average)
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
    n(cons-stream (sign-change-detector avpt last-average)
                 (make-zero-crossings (stream-cdr input-stream)
                                      (stream-car input-stream)
                                      avpt))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3.76
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Eva Lu Ator has a criticism of Louis's approach in exercise 3.75. The program he wrote is not modular, because it intermixes the operation of smoothing with the zero-crossing extraction. For example, the extractor should not have to be changed if Alyssa finds a better way to condition her input signal. Help Louis by writing a procedure smooth that takes a stream as input and produces a stream in which each element is the average of two successive input stream elements. Then use smooth as a component to implement the zero-crossing detector in a more modular style.

(define (smooth-streams method . streams)
  (cond ((null? streams) the-empty-stream)
        (else
         (cons (apply method (map (lambda (x) (stream-car x))
                                  streams))
               (memo-proc (lambda ()
                            (apply smooth-streams (cons method (map (lambda (x) (stream-cdr x))
                                                                    streams)))))))))

(define (arma n)
  (lambda (input-stream)
    (apply smooth-streams (cons average (cons input-stream
                                              (let iter ((number (- n 1))
                                                         (current (stream-cdr input-stream)))
                                                (cond ((= number 0) (list))
                                                      (else (cons current (iter (- number 1)
                                                                                (stream-cdr current)))))))))))

;; (define arma-2-int ((arma 2) ints))
;; (display-stream-ref arma-2-int 10)
;; (exit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3.77
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The integral procedure used above was analogous to the ``implicit'' definition of the infinite stream of integers in section 3.5.2. Alternatively, we can give a definition of integral that is more like integers-starting-from (also in section 3.5.2):

;; When used in systems with loops, this procedure has the same problem as does our original version of integral. Modify the procedure so that it expects the integrand as a delayed argument and hence can be used in the solve procedure shown above.

;; (define (integral integrand initial-value dt)
;;   (cons-stream initial-value
;;                (let ((real-int (force integrand)))
;;                  (if (stream-null? real-int)
;;                    the-empty-stream
;;                    (integral (stream-cdr real-int)
;;                              (+ (* dt (stream-car real-int))
;;                                 initial-value)
;;                              dt)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3.78
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Consider the problem of designing a signal-processing system to study the homogeneous second-order linear differential equation The output stream, modeling y, is generated by a network that contains a loop. This is because the value of d2y/dt2 depends upon the values of y and dy/dt and both of these are determined by integrating d2y/dt2. The diagram we would like to encode is shown in figure 3.35. Write a procedure solve-2nd that takes as arguments the constants a, b, and dt and the initial values y0 and dy0 for y and dy/dt and generates the stream of successive values of y.

(define (solve-2nd y0 dy0 a b dt)
  (let ((y #f)
        (dy #f)
        (d2y #f))
    (set! y (stream-integral-dt-delay (lambda () dy) y0 dt))
    (set! dy (stream-integral-dt-delay (lambda () d2y) dy0 dt))
    (set! d2y (stream-map (lambda (x y) (+ (* a x)
                                           (* b y)))
                          dy
                          y))
    y))


;; (define y (solve-2nd 1 1 4 -3 0.1))
;; (display-stream-ref y 10)
;; (exit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3.79
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Generalize the solve-2nd procedure of exercise 3.78 so that it can be used to solve general second-order differential equations d2 y/dt2 = f(dy/dt, y).

(define (sovle-2nd y0 dy0 f dt)
  (let ((y #f)
        (dy #f)
        (d2y #f))
    (set! y (stream-integral-dt-delay (lambda () dy) y0 dt))
    (set! dy (stream-integral-dt-delay (lambda () d2y) dy0 dt))
    (set! d2y (stream-map (lambda (x y) (f x y))
                          dy
                          y))
    y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3.80
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write a procedure RLC that takes as arguments the parameters R, L, and C of the circuit and the time increment dt. In a manner similar to that of the RC procedure of exercise 3.73, RLC should produce a procedure that takes the initial values of the state variables, vC0 and iL0, and produces a pair (using cons) of the streams of states vC and iL. Using RLC, generate the pair of streams that models the behavior of a series RLC circuit with R = 1 ohm, C = 0.2 farad, L = 1 henry, dt = 0.1 second, and initial values iL0 = 0 amps and vC0 = 10 volts.

(define (rlc r l c dt)
  (lambda (i0 v0)
    (let ((i #f)
          (v #f))
      (set! v (stream-integral-dt-delay (lambda () (stream-scale i (- (/ 1 c))))
                                        v0
                                        dt))
      (set! i (stream-integral-dt-delay (lambda () (stream-plus (stream-scale v (/ 1 l))
                                                                (stream-scale i (- (/ r l)))))
                                        i0
                                        dt))
      (cons v i))))

;; (define rlc0 (rlc 1 0.2 1 0.1))
;; (define vi0 (rlc0 0 10))
;; (print "v:")
;; (display-stream-ref (car vi0) 10)
;; (display-stream-ref (cdr vi0) 10)
;; (exit)
