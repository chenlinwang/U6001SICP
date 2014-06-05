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
;; ;;test for pi
;; (newline)
;; (display pical)
;; (newline)
;; (display (map (lambda (x) (cal-precision x 3.1415926)) pical))
;; (newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Exercise 1.32
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1. Show that sum and product (exercise 1.31) are both special cases of a still more general notion called accumulate that combines a collection of terms, using some general accumulation function:
;;Accumulate takes as arguments the same term and range specifications as sum and product, together with a combiner procedure (of two arguments) that specifies how the current term is to be combined with the accumulation of the preceding terms and a null-value that specifies what base value to use when the terms run out. Write accumulate and show how sum and product can both be defined as simple calls to accumulate.
;; 2. (accumulate combiner null-value term a next b)If your accumulate procedure generates a recursive process, write one that generates an iterative process. If it generates an iterative process, write one that generates a recursive process.

(define (accumulate combiner null-value term next)
    (define (accuiter a b s)
        (if (> a b)
            s
            (accuiter (next a) b (combiner (term a) s))))
  (lambda (a b) (accuiter a b null-value)))

(define (sum term next)
    (accumulate + 0 term next))

(define (product term next)
    (accumulate * 1 term next))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Exercise 1.33
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; You can obtain an even more general version of accumulate (exercise 1.32) by introducing the notion of a filter on the terms to be combined. That is, combine only those terms derived from values in the range that satisfy a specified condition. The resulting =filtered-accumulate= abstraction takes the same arguments as accumulate, together with an additional predicate of one argument that specifies the filter. Write =filtered-accumulate= as a procedure. Show how to express the following using =filtered-accumulate=:
;; 1. the sum of the squares of the prime numbers in the interval a to b (assuming that you have a prime? predicate already written)
;; 2. the product of all the positive integers less than n that are relatively prime to n (i.e., all positive integers i < n such that GCD(i,n) = 1).

(define (filtered-accumulate combiner null-value term next filter)
    (define (faiter a b s)
        (if (> a b)
            s
            (faiter (next a) b (combiner (if (filter a)
                                             (term a)
                                             null-value)
                                         s))))
  (lambda (a b) (faiter a b null-value)))

;;1 prime?
(define (divide? a b)
    (= 0 (remainder a b)))

(define (all-not-divide? a plist)
    (if (null? plist)
        #t
        (if (divide? a (car plist))
            #f
            (all-not-divide? a (cdr plist)))))

(define (listor a)
    (cond ((null? a) #t)
          ((car a) (listor (cdr a)))
          (else (car a))))

(define (find-prime n)
    (define (finditer i plist)
        (if (> i n)
            plist
            (let ((start (+ i 1)) (end (square i)))
              (do ((num start (+ num 1))
                   (doplist plist (if (all-not-divide? num doplist)
                                    (cons num doplist)
                                    doplist)))
                  ((or (> num end) (> num n)) (finditer end doplist))
                ;; (begin
                ;;  (newline)
                ;;  (display num)
                ;;  (display ":")
                ;;  (display doplist)
                ;;  (newline))
                ))))
  (if (< n 3)
      '()
      (finditer 2 '(2))))

(define (prime? n)
    (if (< n 2)
        #f
        (all-not-divide? n (find-prime (floor (sqrt n))))))

(define (square x) (* x x))

;; sum of square of prime number from a to b
(define ssp (filtered-accumulate + 0 square increment prime?))

;;2 relative prime
;;term
(define (identity x) x)
;;gcd
(define (gcd a b)
    (if (divide? a b)
        b
        (gcd b (remainder a b))))

(define (relative-prime? a b)
    (= 1 (gcd a b)))

;;relative prime filter maker
(define (relative-prime-fileter-maker n)
    (lambda (a) (relative-prime? n a)))

;; product of relatively prime number to n from 0 to n
(define prp (lambda (n) ((filtered-accumulate * 1 identity increment (relative-prime-fileter-maker n)) 1 n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Exercise 1.35
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Show that the golden \phi ratio (section 1.2.2) is a fixed point of the transformation:
;; x_{n+1} = 1 +1/x_{n}
;; and use this fact to compute by means of the =fixed-point= procedure.
(define (close-enough? a b tolerance)
    (< (abs (- a b)) tolerance))

(define (fixpoint f init tolerance)
    (let ((next (f init)))
      (if (close-enough? init next tolerance)
          init
          (fixpoint f next tolerance))))

(define (gr x)
    (+ 1 (/ 1.0 x)))

(define (check-error-rate input real)
    (* 100 (/ (abs (- input real)) real)))

(define grv (/ (+ 1 (sqrt 5)) 2.0))

;; (define d135 (map (lambda (t) (fixpoint gr 1 t)) '(0.1 0.01 0.001 0.0001)))
;; (display d135)
;; (newline)
;; (display (map (lambda (input) (check-error-rate input grv)) d135))
;; (newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Exercise 1.36
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (print-fixpoint f init tolerance step)
    (let ((next (f init)))
      (if (close-enough? init next tolerance)
          init
          (begin (newline)
                 (display step)
                 (display "--From init value: ")
                 (display init)
                 (display " to next value: ")
                 (display next)
                 (print-fixpoint f next tolerance (+ step 1))))))

(define (average a b)
    (/ (+ a b) 2.0))

(define (iter1 x)
    (/ (log 1000) (log x)))

(define (iter2 x)
    (average x (iter1 x)))

;; (display "\nWithout damping (t=0.001):")
;; (print-fixpoint iter1 10 0.001 1)
;; (display "\n\nWith damping (t=0.001):")
;; (print-fixpoint iter2 10 0.001 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 1.37
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1. An infinite continued fraction is an expression of the form in book page 79.As an example, one can show that the infinite continued fraction expansion with the N_{i} and the D_{i} all equal to 1 produces 1/ \phi , where \phi is the golden ratio (described in section 1.2.2). One way to approximate an infinite continued fraction is to truncate the expansion after a given number of terms. Such a truncation -- a so-called =k-term finite continued fraction= -- has the form. Suppose that =n= and =d= are procedures of one argument (the term index =i=) that return the N_{i} and D_{i} of the terms of the continued fraction. Define a procedure =cont-frac= such that evaluating =(cont-frac n d k)= computes the value of the k-term finite continued fraction. Check your procedure by approximating 1/ \phi using:
;; #+BEGIN_SRC scheme
;; (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) k)
;; #+END_SRC
;; for successive values of k. How large must you make k in order to get an approximation that is accurate to 4 decimal places?

;; 2. If your =cont-frac= procedure generates a recursive process, write one that generates an iterative process.

(define (make-cont-frac n d)
    (define (cfiter k s)
        (if (= k 0)
            s
            (cfiter (- k 1) (/ (n k) (+ (d k) s)))))
  (lambda (i) (cfiter i 0)))

(define (nphi x) 1.0)
(define (dphi x) 1.0)

(define (phi i)
    (/ 1.0 ((make-cont-frac nphi dphi) i)))

;;high-order procedure to test the necessary steps it takes to approach certain accuracy
(define (find-near p r tolerance next init)
    (if (close-enough? (p init) r tolerance)
        init
        (find-near p r tolerance next (next init))))

;(display (find-near phi grv 0.0001 increment 2))

;; (define t137 '(0.1 0.01 0.001 0.0001 0.00001))
;; (display t137)
;; (newline)
;; (display (map (lambda (k)  (find-near phi grv k increment 2))
;;               t137))
;; (newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 1.38
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; In 1737, the Swiss mathematician Leonhard Euler published a memoir /De Fractionibus Continuis/, which included a continued fraction expansion for =e-2=, where e is the base of the natural logarithms. In this fraction, the N_{i}i are all 1, and the D_{i} are successively 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, .... Write a program that uses your =cont-frac= procedure from exercise 1.37 to approximate e, based on Euler's expansion.
(define (ne i) 1.0)
(define (de i)
        (let ((flag (remainder i 3)))
          (cond ((= flag 2) (* 2.0 (ceiling (/ i 3.0))))
                (else 1.0))))
(define p138 (make-cont-frac ne de))
(define ecal (lambda (i) (+ 2 ((make-cont-frac ne de) i))))

;; (define e 2.718281828459)
;; (define t138 '(10 100 1000 10000))
;; (define r1381  (map ecal t138))
;; (display r1381)
;; (newline)
;; (define r1382 (map (lambda (input) (check-error-rate input e)) r1381))
;; (display r1382)
;; (newline)

(define (print-test-accuracy f testinput realvalue)
    (define calvalue  (map ecal testinput))
  (display calvalue)
  (newline)
  (define calerror (map (lambda (input) (check-error-rate input realvalue)) calvalue))
  (display calerror)
  (newline)
  (list testinput calvalue calerror))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 1.39
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A continued fraction representation of the tangent function was published in 1770 by the
;; German mathematician J.H. Lambert. Define a procedure =(tan-cf x k)= that computes an approximation to the tangent function based on Lambert's formula. K specifies the number of terms to compute, as in exercise 1.37.
(define (tan-cf x k)
    (define xsquare (* x x))
    (define (ntan i)
        (if (= i k)
            (- x)
            (- xsquare)))
    (define (dtan i)
        (- (* 2 i) 1))
    ((make-cont-frac ntan dtan) k))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 1.40
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define a procedure cubic that can be used together with the newtons-method procedure in expressions of the form:
;; (newtons-method (cubic a b c) 1)
;; to approximate zeros of the cubic x^{3} + ax_{2} + bx + c.
(load "../BookImplementation/sec1.3.scm")

(define (make-cube-fix a b c)
    (lambda (x) (+ (cube x) (* a (square x)) (* b x) c (- x))))

(define (solve-cube init tolerance a b c)
    (find-fix2 (make-cube a b c) init tolerance))

(define (solve-cube-newton init tolerance a b c)
    (find-fix2 (newton (make-cube a b c)) init tolerance))

;(display (solve-cube-newton 10 0.001 -30 20 5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 1.41
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define a procedure =double= that takes a procedure of one argument as argument and returns a procedure that applies the original procedure twice. For example, if =inc= is a procedure that adds 1 to its argument, then =(double inc)= should be a procedure that adds 2. What value is returned by
;; (((double (double double)) inc) 5)
;;This assume that inc-like procedure takes a single input
(define (double p)
    (lambda (x) (p (p x))))

(define (inc x) (+ x 1))

;(display (((double (double double)) inc) 5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 1.42
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Let =f= and =g= be two one-argument functions. The composition =f= after =g= is defined to be the function =x = f(g(x))=. Define a procedure compose that implements composition. For example, if inc is a procedure that adds 1 to its argument.
;; ((compose square inc) 6)
(define (compose g f)
    (lambda (x) (g (f x))))

;(display ((compose square inc) 6))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 1.43
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If =f= is a numerical function and =n= is a positive integer, then we can form the nth repeated application of =f=, which is defined to be the function whose value at =x= is =f(f(...(f(x))...))=. For example, if =f= is the function =f(x) = x + 1=, then the nth repeated application of f is the function =f(x) = x + n=. If =f= is the operation of squaring a number, then the nth repeated application of f is the function that raises its argument to the 2n th power. Write a procedure that takes as inputs a procedure that computes =f= and apositive integer n and returns the procedure that computes the nth repeated application of =f=. Your procedure should be able to be used as follows:
;; ((repeated square 2) 5)
;; : 625
;; Hint: You may find it convenient to use compose from exercise 1.42.
(define (repeat fun n)
    (define (repeat-iter base i max)
        (if (= i max)
            base
            (repeat-iter (compose fun base) (+ i 1) max)))
  (repeat-iter fun 1 n))

(define (repeat-fast fun n)
    (define (identity x) x)
  (define (fast-iter base sum i)
      (cond ((= i 1) (compose base sum))
            ((even? i) (fast-iter (double base) sum (/ i 2)))
            (else (fast-iter base (compose base sum) (- i 1)))))
  (fast-iter fun identity n))

;(display ((repeat square 2) 5))
;; (display ((repeat-fast square 3) 5))
;; (newline)
;; (display (* 625 625))
;; (newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 1.44
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The idea of smoothing a function is an important concept in signal processing. If =f= is a function and =dx= is some small number, then the smoothed version of =f= is the function whose value at a point x is the average of f(x - dx), f(x), and f(x + dx). Write a procedure smooth that takes as input a procedure that computes f and returns a procedure that computes the smoothed f. It is sometimes valuable to repeatedly smooth a function (that is, smooth the smoothed function, and so on) to obtained the n-fold smoothed function. Show how to generate the n-fold smoothed function of any given function using smooth and repeated from exercise 1.43.

(define (smooth f dx)
    (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3.0)))

(define (smooth-repeat f dx n)
    (repeat (smooth f dx) n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 1.45
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; We saw in section 1.3.3 that attempting to compute square roots by naively finding a fixed x/y does not converge, and that this can be fixed by average damping. The same method point of y works for finding cube roots as fixed points of the average-damped x/y^{2} . Unfortunately, the process does not work for fourth roots -- a single average damp is not enough to make a fixed-point search for x/y^{3} converge. On the other hand, if we average damp twice (i.e., use the average damp of the average damp of x/y_{3} ) the fixed-point search does converge. Do some experiments to determine how many average damps are required to compute nth roots as a fixed-point search based upon repeated average damping of x/y^{n-1} . Use this to implement a simple procedure for computing nth roots using fixed-point, average-damp, and the repeated procedure of exercise 1.43. Assume that any arithmetic operations you need are available as primitives.

(define (fast-expr base n)
    (define (fast-iter base sum i)
        (cond ((= i 1) (* base sum))
              ((even? i) (fast-iter (square base) sum (/ i 2)))
              (else (fast-iter base (* sum base) (- i 1)))))
  (fast-iter base 1 n))

(define (make-nth-root x n)
    (define n-1 (- n 1))
    (lambda (y) (/ x (fast-expr y n-1))))

(define (make-nth-damping fun n)
    ((repeat-fast average-damping (- n 1)) fun))

(define (find-nth-root x n init tolerance)
    (find-fix2 (make-nth-damping (make-nth-root x n) n) init tolerance))

;; (display (find-nth-root 16 4 1 0.01))
;; (newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 1.46
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Several of the numerical methods described in this chapter are instances of an extremely general computational strategy known as iterative improvement. Iterative improvement says that, to compute something, we start with an initial guess for the answer, test if the guess is good enough, and otherwise improve the guess and continue the process using the improved guess as the new guess. Write a procedure iterative-improve that takes two procedures as arguments: a method for telling whether a guess is good enough and a method for improving a guess. Iterative-improve should return as its value a procedure that takes a guess as argument and keeps improving the guess until it is good enough. Rewrite the sqrt procedure of section 1.1.7 and the fixed-point procedure of section 1.3.3 in terms of iterative-improve.

(define (make-iterative close? improve)
    (define (iter init)
      (let ((next (improve init)))
        (if (close? init next)
            init
            (iter next))))
  iter)

;; tolerance maker for close?
(define (tolerance-close? tolerance)
    (lambda (a b) (close-enough? a b tolerance)))

;; rewrite the find fixpoint
(define (find-fix3 fun init tolerance)
    ((make-iterative (tolerance-close? tolerance) fun) init))

;; square root improve function
(define (sqrt-improve y)
    (lambda (x) (average x (/ y x))))

;; rewrite square improvement
(define (sqrt146 y init tolerance)
    ((make-iterative (tolerance-close? tolerance) (sqrt-improve y)) init))

;; test
;;(display (sqrt146 2 1 0.01))
