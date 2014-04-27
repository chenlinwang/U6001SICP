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
