;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; section 2.1.1 rational number arithmetic operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;; Section 2.1.3 Build up the cons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;New cons
(define (new-cons x y)
  (define (pair m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else
           (display "No data with that No.\n"))))
  pair)

;;New car
(define (new-car p)
  (p 0))

;;New cdr
(define (new-cdr p)
  (p 1))
