(load "../BookExercise/basic")
(define (loadlec10) (load "lec10"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Imaginary Numbers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Two Constructors
(define (make-complex-number-cart real imaginary)
    ;; Making complex number using Cartesian Representation.
    ;; (number,number) -> (list)
    (list 'cart real imaginary))

(define (make-complex-number-polar magnitude angle)
    ;; Making complex number using Polar Representation, the angle is measured in rad.
    ;; (number,number) -> (list)
    (list 'polar magnitude angle))

;; Selectors
(define (get-tag c)
    ;; Getting the tag of the data
    ;; (list) -> (symbol)
    (if (null? c)
        (errormsg "get-tag: empty data" c)
        (car c)))

(define (get-real-part c)
    ;; Getting the real parts of the complex number c.
    ;; (list) -> (number)
    (let ((ctag (get-tag c)))
      (cond ((eq? ctag 'cart) (cadr c))
            ((eq? ctag 'polar) (* (cadr c) (cos (caddr c))))
            (else (errormsg "get-real-part: unknown tag name" c)))))

(define (get-imaginary-part c)
    ;; Getting the imaginary parts of the complex number c.
    ;; (list) -> (number)
    (let ((ctag (get-tag c)))
      (cond ((eq? ctag 'cart) (caddr c))
            ((eq? ctag 'polar) (* (cadr c) (sin (caddr c))))
            (else (errormsg "get-imaginary-part: unknown tag name" c)))))

(define (get-magnitude-part c)
    ;; Getting the magnitude parts of the complex number c.
    ;; (list) -> (number)
    (let ((ctag (get-tag c)))
      (cond ((eq? ctag 'polar) (cadr c))
            ((eq? ctag 'cart) (sqrt (+ (square (cadr c))
                                       (square (caddr c)))))
            (else (errormsg "get-magnitude-part: unknown tag name" c)))))

(define (get-angle-part c)
    ;; Getting the angle parts of the complex number c.
    ;; (list) -> (number)
    (let ((ctag (get-tag c)))
      (cond ((eq? ctag 'polar) (caddr c))
            ((eq? ctag 'cart) (atan (caddr c)
                                    (cadr c)))
            (else (errormsg "get-angle-part: unknown tag name" c)))))

;; Operators
(define (complex-add c1 c2)
    ;; Adding two complex numbers c1 and c2.
    ;; (list,list) -> (list)
    (make-complex-number-cart
     (+ (get-real-part c1)
        (get-real-part c2))
     (+ (get-imaginary-part c1)
        (get-imaginary-part c2))))

(define (complex-multiply c1 c2)
    ;; Multiplying two complex numbers c1 and c2.
    ;; (list,list) -> (list)
    (make-complex-number-polar
     (* (get-magnitude-part c1)
        (get-magnitude-part c2))
     (+ (get-angle-part c1)
        (get-angle-part c2))))

;; ;; Testing
;; (define cl
;;     (list (make-complex-number-cart 1 2)
;;           (make-complex-number-cart 3 4)
;;           (make-complex-number-polar 5 0)
;;           (make-complex-number-polar 6 (atan 1))))

;; (map (lambda (c)
;;        (begin (display c)
;;               (display ":\n")
;;               (let iter ((remain (cdr cl))
;;                          (present (car cl)))
;;                    (begin (display "Adding ")
;;                           (display present)
;;                           (display ":\t")
;;                           (display (complex-add c present))
;;                           (newline)
;;                           (display "Multiplying ")
;;                           (display present)
;;                           (display ":\t")
;;                           (display (complex-multiply c present))
;;                           (newline)
;;                           (if (not (null? remain))
;;                               (iter (cdr remain)
;;                                     (car remain))
;;                               (list))))))
;;      cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Number Calculation System
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operator for tag
(define tag-get car)
(define tag? pair?)

;; Constructor for the constant
(define constant-tag 'constant)

(define (constant-generator n)
    ;; Convert a scheme number into a constant.n: number
    ;; (number) -> (list)
    (list constant-tag n))

;; Selector
(define constant-val cadr)

;; Operator
(define (constant? c)
    ;; Test whether it is a constant
    (and (tag? c) (eq? (tag-get c) constant-tag)))

(define (constant-add c1 c2)
    ;; Add two constants, c1,c2: constants
    ;; (list,list) -> (list)
    (constant-generator (+ (constant-val c1)
                           (constant-val c2))))

(define (constant-larger? c1 c2)
    ;; tells whether c1 is larger than c2.
    (> (constant-val c1) (constant-val c2)))

(define (constant-equal? c1 c2)
    ;; tells whether c1 is equal than c2.
    (= (constant-val c1) (constant-val c2)))

(define (constant-smaller? c1 c2)
    ;; tells whether c1 is smaller than c2.
    (< (constant-val c1) (constant-val c2)))

;; Constructor for the sum
(define sum-tag '+)

(define (sum-generator e1 e2)
    ;; To put two constants into a sum form. e1,e2: expression objects.
    ;; (list,list) -> (list)
    (list sum-tag e1 e2))

;; Selector
(define sum-augend cadr)
(define sum-addent caddr)

;; Operator
(define (sum? s)
    ;; Test whether it is a sum
    (and (tag? s) (eq? (tag-get s) sum-tag)))

;; Evaluation Function
(define (eval-1 e)
    ;; Evaluating the expression and gives out a constant.e:expression
    ;; (list) -> (list)
    (cond ((constant? e) e)
          ((sum? e) (constant-add (eval-1 (sum-augend e))
                                    (eval-1 (sum-addent e))))
          (else (errormsg "eval-1: unknown tag!" e))))

;; ;; eval-1 testing
;; (define c1 (constant-generator 1))
;; (define c2 (constant-generator 2))
;; (define c3 (constant-generator 3))
;; (define s1 (sum-generator c1 c2))
;; (define s2 (sum-generator s1 c3))
;; (display (eval-1 c1))
;; (newline)
;; (display (eval-1 c2))
;; (newline)
;; (display (eval-1 s1))
;; (newline)
;; (display (eval-1 c3))
;; (newline)
;; (display (eval-1 s2))
;; (newline)

;; Constructor for range
(define range-tag 'range)

(define (range-generator min max)
    ;; Define a range with a min and a max number. min,max: scheme number.
    ;;(list,list) -> (list)
    (if (< min max)
            (list range-tag min max)
            (list range-tag max min)))

;; Selector
(define range-min cadr)
(define range-max caddr)

;; Operator
(define (range? r)
    ;; Test wheter r is a range.
    (and (tag? r) (eq? (tag-get r) range-tag)))

(define (range-add r1 r2)
    ;; Adding two ranges.r1,r2: range
    ;; (list,list) -> (list)
    (range-generator (+ (range-min r1)
                        (range-min r2))
                     (+ (range-max r1)
                        (range-max r2))))

;; Value Objects for both constant and range
;; Operators
(define (value? v) (or (constant? v) (range? v)))

(define (value-2range v)
    ;; Convert value object to range object.v:value object.
    ;; (list) -> (list)
    (cond ((range? v) v)
          ((constant? v) (range-generator (constant-val v)
                                          (constant-val v)))
          (else (errormsg "value-2range: unknown tag" v))))

(define (value-add v1 v2)
    ;; Adding values after converting values into range.
    (range-add (value-2range v1)
               (value-2range v2)))

;; Evaluation Function
(define (eval-2 e)
    ;; Evaluate the expression
    (cond ((value? e) e)
          ((sum? e)
           (value-add (eval-2 (sum-augend e))
                      (eval-2 (sum-addent e))))
          (else (errormsg "eval-2: unknown tag" e))))

;; ;; eval-2 testing
;; (define r1 (range-generator 1 2))
;; (define r2 (range-generator 4 3))
;; (define c1 (constant-generator 5))
;; (define s1 (sum-generator r1 r2))
;; (define s2 (sum-generator s1 c1))
;; (display (eval-2 r1))
;; (newline)
;; (display (eval-2 r2))
;; (newline)
;; (display (eval-2 s1))
;; (newline)
;; (display (eval-2 c1))
;; (newline)
;; (display (eval-2 s2))
;; (newline)

;; Constructor for the limitted procision value
(define limit-tag 'limit)

(define (limit-generator n1 n2)
    ;; Generate limitted precision number from two scheme numbers. n1: the number of measure; n2: the error range of measure.
    (list limit-tag n1 n2))

;; Selector
(define limit-center cadr)
(define limit-error caddr)

;; Operator
(define (limit? l)
    ;; Test whether it is a limited precision number.
    (and (tag? l) (eq? (tag-get l) limit-tag)))

;; Rewrite Operators for value
(define (value? v) (or (constant? v) (range? v) (limit? v)))

(define (value-2range v)
    ;; Convert value object to range object.v:value object.
    ;; (list) -> (list)
    (cond ((range? v) v)
          ((constant? v) (range-generator (constant-val v)
                                          (constant-val v)))
          ((limit? v) (let ((center (limit-center v))
                            (error (limit-error v)))
                        (range-generator (- center error)
                                         (+ center error))))
          (else (errormsg "value-2range: unknown tag" v))))

;; ;; eval-2 testing again
;; (define l1 (limit-generator 2 1))
;; (define l2 (limit-generator 4 3))
;; (define r1 (range-generator 6 5))
;; (define c1 (constant-generator 7))
;; (define s1 (sum-generator l1 l2))
;; (define s2 (sum-generator l1 r1))
;; (define s3 (sum-generator l1 c1))
;; (display (eval-2 l1))
;; (newline)
;; (display (eval-2 l2))
;; (newline)
;; (display (eval-2 s1))
;; (newline)
;; (display (eval-2 s2))
;; (newline)
;; (display (eval-2 s3))
;; (newline)

;; new operator for value
(define (value-2limit v)
    ;; Convert value object to limit precision object.v value object.
    ;; (list) -> (list)
    (cond ((limit? v) v)
          ((constant? v) (limit-generator (constant-val v) 0))
          ((range? v) (let ((min (range-min v))
                           (max (range-max v)))
                       (let ((center (average min max)))
                         (limit-generator center
                                          (- max center)))))
          (else (errormsg "value-limit: unknown tag" v))))

;; ;; Test for new operator
;; (display (value-2limit (constant-generator 5)))
;; (newline)
;; (display (value-2limit (range-generator 2 5)))
;; (newline)
