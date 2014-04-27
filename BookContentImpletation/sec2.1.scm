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
