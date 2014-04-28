;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Define a better version of make-rat that handles both positive and negative arguments. Make-rat should normalize the sign so that if the rational number is positive, both the numerator and denominator are positive, and if the rational number is negative, only the numerator is negative.

;;We shall just examine the positive-negative thing in the make-rat and make sure that rat-add and rat-times will not break the rules and use them in rat-minus and rat-divide

;; greatest common dividor
(define (gcd n m)
  (if (= m 0)
      n
      (gcd m (remainder n m))))

;;Constructor
(define (make-rat n m)
  ;Get them relatively prime
  (define g (gcd n m))
  ;Check the positive-negative thing
  (if (> (* n m) 0)
      (cons (/ n g) (/ m g))
      (cons (- (abs (/ n g))) (abs (/ m g)))))

;;Seclector
(define (numer x)
  (car x))

(define (denom x)
  (cdr x))

(define (print-rat x)
  (display (numer x))
  (display " / ")
  (display (denom x))
  (newline))

;;Basic Operators
(define (rat-add x y)
  (make-rat (+ (* (numer x) (denom y)) (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (rat-minus x y)
  (rat-add x
           (make-rat (- (numer y)) (denom y))))

(define (rat-times x y)
  (make-rat (* (numer x) (numer y)) (* (denom x) (denom y))))

(define (rat-divid x y)
  (rat-times x
             (make-rat (denom y) (numer y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Consider the problem of representing line segments in a plane. Each segment is represented as a pair of points: a starting point and an ending point. Define a constructor make-segment and selectors start-segment and end-segment that define the representation of segments in terms of points. Furthermore, a point can be represented as a pair of numbers: the x coordinate and the y coordinate. Accordingly, specify a constructor make-point and selectors x-point and y-point that define this representation. Finally, using your selectors and constructors, define a procedure midpoint-segment that takes a line segment as argument and returns its midpoint (the point whose coordinates are the average of the coordinates of the endpoints). To try your procedures, you'll need a way to print points:

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

;;Points

;Constructor
(define (make-point x y)
  (cons x y))

;Selector
(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

;operator
(define (point-add p1 p2)
  (make-point (+ (x-point p1) (x-point p2))
              (+ (y-point p1) (y-point p2))))

(define (point-scalar-times p s)
  (make-point (* (x-point p) s)
              (* (y-point p) s)))

(define (point-minus p1 p2)
  (point-add p1
             (point-scalar-times p2 -1)))

;;Line-segment

;Constructor
(define (make-segment p1 p2)
  ;natural the left point first, if same bottom first
  (cond ((< (x-point p1) (x-point p2)) (cons p1 p2))
        ((> (x-point p1) (x-point p2)) (cons p2 p1))
        (else (cond ((< (y-point p1) (y-point p2)) (cons p1 p2))
                    ((> (y-point p1) (y-point p2)) (cons p2 p1))
                    (else (display "Starting Point equals to Ending Point, Not a Line Segement!\n"))))))

;selector
(define (start-segment ls)
  (car ls))

(define (end-segment ls)
  (cdr ls))

;operator
(define (midpoint-segment ls)
  (point-scalar-times
   (point-add (start-segment ls)
              (end-segment ls))
   0.5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercie 2.3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Implement a representation for rectangles in a plane. (Hint: You may want to make use of exercise 2.2.) In terms of your constructors and selectors, create procedures that compute the perimeter and the area of a given rectangle. Now implement a different representation for rectangles. Can you design your system with suitable abstraction barriers, so that the same perimeter and area procedures will work using either representation?
;;Point
;;operator

;;test of the same point
(define (same-point p1 p2)
  (and (= (x-point p1) (x-point p2))
       (= (y-point p1) (y-point p2))))

;;Line Segment
;;operator

;;test of meeting at starting
(define (meet-start l1 l2)
  (same-point (start-segment l1)
              (start-segment l2)))

;;test of meeting at ending
(define (meet-end l1 l2)
  (same-point (end-segment l1)
              (end-segment l2)))

;;test of meeting at start for l1 and end for l2
(define (meet-start-end l1 l2)
  (same-point (start-segment l1)
              (end-segment l2)))

;;test which segment is upper
(define (upper-segment l1 l2 loc1 loc2 )
  (> (y-point (loc1 l1))
     (y-point (loc2 l2))))

;;Vector
;;Constructor
(define (make-vec ls)
  (point-minus (end-segment ls)
               (start-segment ls)))

;;Selector
;;end point of the vector
(define (end-vec v) v)

;;operator
;;scalar product
(define (vec-scalar-product v s)
  (point-scalar-times v s))

;;the opposite vector
(define (opp-vec v)
  (vec-scalar-product v -1))

;;point product
(define (vec-point-product v1 v2)
  (+ (* (x-point v1) (x-point v2))
     (* (y-point v1) (y-point v2))))

;;Move the point along with the vector
(define (move-segment v l)
  (make-segment (point-add (start-segment l) v)
                (point-add (end-segment l) v)))

;;Constructor
(define (make-rec l1 l2)
  (define v1 (make-vec l1))
  (define v2 (make-vec l2))
  (if (= (vec-point-product v1 v2) 0)
      (cond ((meet-start l1 l2)
             (if (upper-segment l1 l2 end-segment end-segment)
                 (cons l1 l2)
                 (cons l2 l1)))
            ((meet-end l1 l2)
             (if (upper-segment l1 l2 start-segment start-segment)
                 (cons (move-segment (opp-vec v1) l2)
                       (move-segment (opp-vec v2) l1))
                 (cons (move-segment (opp-vec v2) l1)
                       (move-segment (opp-vec v1) l2))))
            ((meet-start-end l2 l1)
             (if (upper-segment l1 l1 start-segment end-segment)
                 (cons (move-segment (opp-vec v1) l2)
                       l1)
                 (cons l1
                       (move-segment (opp-vec v1) l2))))
            ((meet-start-end l1 l2)
             (if (upper-segment l2 l2 start-segment end-segment)
                 (cons (move-segment (opp-vec v2) l1)
                       l2)
                 (cons l2
                       (move-segment (opp-vec v2) l1))))
            (else
             (display "The two segment are not meet. No way of composing a rectangle!\n")))
      (display "Not a pair of perpendicular line segments. No way of composing a rectangle!\n")))

;;Selector
;;the upper side
(define (rec-upper-side r)
  (car r))

;;the lower side
(define (rec-lower-side r)
  (cdr r))

;;;;;;;;;;Testing
(define p1 (make-point 0 0))
(define p2 (make-point 0 1))
(define p3 (make-point 1 0))
(define p4 (make-point 1 1))

(define l1 (make-segment p1 p2))
(define l2 (make-segment p1 p3))
(define l3 (make-segment p2 p4))
(define l4 (make-segment p3 p4))

(define v1 (make-vec l1))
(define v2 (make-vec l2))
(define v3 (make-vec l3))
(define v4 (make-vec l4))
;;;;;;;;;;End of Testing

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Here is an alternative procedural representation of pairs. For this representation, verify that (new-car (new-cons x y)) yields x for any objects x and y.

(define (new-cons x y)
  (lambda (m) (m x y)))

(define (new-car z)
  (z (lambda (p q) p)))

(define (new-cdr z)
  (z (lambda (p q) q)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Show that we can represent pairs of nonnegative integers using only numbers and arithmetic operations if we represent the pair a and b as the integer that is the product 2a 3b. Give the corresponding definitions of the procedures cons, car, and cdr.

;;test for even odd property
 (define (even? num)
   (= (remainder num 2) 0))

 ;;fast-exp
 (define (fast-exp bas exp)
   (define (fei l b e)
     (cond ((= e 1) (* l b))
           ((even? e) (fei l (* b b) (/ e 2)))
           (else (fei (* l b) b (- e 1)))))
   (fei 1 bas exp))

 ;;new cons 2
 (define (new-cons2 a b)
   (* (fast-exp 2 a)
      (fast-exp 3 b)))

 ;;has exponient
 (define (has-exp num bas)
   (define (left? x)
     (= (remainder x bas) 0))

   (define (hei num exp)
     (if (left? num)
         (hei (/ num bas) (+ exp 1))
         exp))
  (hei num 0))

;;new car 2
(define (new-car2 num)
  (has-exp num 2))

;;new cdr 2
(define (new-cdr2 num)
  (has-exp num 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.6
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;In case representing pairs as procedures wasn't mind-boggling enough, consider that, in a language that can manipulate procedures, we can get by without numbers (at least insofar as nonnegative integers are concerned) by implementing 0 and the operation of adding 1 as

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
    (lambda (f) (lambda (x) (f ((n f) x)))))

;;This representation is known as Church numerals, after its inventor, Alonzo Church, the logician who invented the lambda calculus.
;;Define one and two directly (not in terms of zero and add-1). (Hint: Use substitution to evaluate (add-1 zero)). Give a direct definition of the addition procedure + (not in terms of repeated application of add-1).

;The one is:
(define one (lambda (f) (lambda (x) (f x))))

;The plus is:
(define (church-add x y)
  (lambda (f) ((x f) ((y f) x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.7
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Alyssa's program is incomplete because she has not specified the implementation of the interval abstraction. Here is a definition of the interval constructor:

;; (define (make-interval a b) (cons a b))

;; Define selectors upper-bound and lower-bound to complete the implementation.

;;Constructor
(define (make-interval a b)
  (if (< a b)
      (cons a b)
      (cons b a)))

;;Selector
;;lower boundary
(define (lower-bound i)
  (car i))

;;upper boundary
(define (upper-bound i)
  (cdr i))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.8
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Using reasoning analogous to Alyssa's, describe how the difference of two intervals may be computed. Define a corresponding subtraction procedure, called sub-interval.

;;Operator
;;differ
(define (int-differ i1 i2)
  (make-interval (- (lower-bound i1) (upper-bound i2))
                 (- (upper-bound i1) (lower-bound i2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.9
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;The width of an interval is half of the difference between its upper and lower bounds. The width is a measure of the uncertainty of the number specified by the interval. For some arithmetic operations the width of the result of combining two intervals is a function only of the widths of the argument intervals, whereas for others the width of the combination is not a function of the widths of the argument intervals. Show that the width of the sum (or difference) of two intervals is a function only of the widths of the intervals being added (or subtracted). Give examples to show that this is not true for multiplication or division.

;;Selector
;;Width
(define (int-width i)
  (- (upper-bound i) (lower-bound i)))

;;Operator
;;Sum
(define (int-sum i1 i2)
  (make-interval (+ (lower-bound i1) (lower-bound i2))
                 (+ (upper-bound i1) (upper-bound i2))))


;;Display a formula
(define (disformula a b c op)
  (display a)
  (display " ")
  (display op)
  (display " ")
  (display b)
  (display " = ")
  (display c))

;;A Left Right Arrow
(define (lrarrow) (display " <=> "))

;;to verify just test
(define (test-for-width n max sign? sf df)
  ;; Generate the negtive or the positive sign
  (define (gen-negpos x)
    (if (= 0 (random 2))
        (- x)
        x))

  ;; Generate the number
  (define (gen-num)
    (if (= 0 sign?)
        (gen-negpos (* (/ (random 1000) 1000.0) max))
        (* (/ (random 1000) 1000.0) max)))

  ;;A single test
  (define (single-test)

    ;;Test-result
    (define (test-result sum op1 ws op ca st)
      (newline)
      (disformula i1 i2 sum op1)
      (lrarrow)
      (op w1 w2 ws)
      (display "?")
      (display ca)
      (display " ")
      (display st))

    (define i1 (make-interval (gen-num) (gen-num)))
    (define i2 (make-interval (gen-num) (gen-num)))
    (define w1 (int-width i1))
    (define w2 (int-width i2))
    (define differ (int-differ i1 i2))
    (define wd (int-width differ))
    (define sum (int-sum i1 i2))
    (define ws (int-width sum))
    (define cs ((guess-op sf) w1 w2))
    (define cd ((guess-op df) w1 w2))
    (define st (= cs ws))
    (define dt (= cd wd))
    (test-result sum "+" ws (guess-pr sf) cs st)
    (test-result differ "-" wd (guess-pr df)  cd dt)
    (newline)
    (and st dt))

  (if (< n 1)
      (display "Assumption Correct!\n")
      (if (single-test)
          (test-for-width (- n 1) max sign? sf df)
          (display "Error, Assumption not correct!\n"))))

;;guess for addion, is a cons, the first part tells how to operator a two-input function, the second tells how to print it.
;;Constructor
(define (make-guess x y)
  (cons x y))

;;Selector
;;get the operator part
(define (guess-op x)
  (car x))
;;get the print part
(define (guess-pr x)
  (cdr x))

;;Instance
(define (print-add x y d)
  (disformula x y d "+"))

(define (print-minus x y d)
  (disformula x y d "-"))

(define simple-add (make-guess +
                               print-add))

(define simple-minus (make-guess -
                                 print-minus))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.10
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Ben Bitdiddle, an expert systems programmer, looks over Alyssa's shoulder and comments that it is not clear what it means to divide by an interval that spans zero. Modify Alyssa's code to check for this condition and to signal an error if it occurs.

;;Operator
;;multply interval
(define (int-mul i1 i2)
  (let ((p1 (* (lower-bound i1) (lower-bound i2)))
        (p2 (* (lower-bound i1) (upper-bound i2)))
        (p3 (* (upper-bound i1) (upper-bound i2)))
        (p4 (* (upper-bound i1) (lower-bound i2))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

;;has 0, determind whether 0 is in the interval
(define (has-0 i)
  (or (= 0 (lower-bound i))
      (= 0 (upper-bound i))
      (and (> 0 (lower-bound i))
           (< 0 (upper-bound i)))))

;;divide interval
(define (int-div i1 i2)
  (if (has-0 i)
      (display "Error: Interval has 0 inside, causing the interval to become two!\n")
      (int-mul i1
               (make-interval (/ 1.0 (upper-bound i2))
                              (/ 1.0 (lower-bound i2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.11
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
