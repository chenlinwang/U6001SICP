;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic operations that scheme ought to provide but fail
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Determind
(define (even? x) (= (remainder x 2) 0))
(define (odd? x) (= (remainder x 2) 1))

;; Mathmatics
(define (square x) (* x x))
(define (cube x) (* x x x))
(define (exponent-iter base exp product)
    (cond ((= exp 0) product)
          ((= exp 1) (* base product))
          ((even? exp) (exponent-iter (square base) (/ exp 2) product))
          (else (exponent-iter base (- exp 1) (* product base)))))

(define (exponentiation base exp)
    (if (< exp 0)
        (exponent-iter (/ 1 base) (- exp) 1)
        (exponent-iter base exp 1)))

;; Error message handler
(define (errormsg msg info)
    ;; Display message and information.
    (display "Error:\t")
    (display msg)
  (newline)
  (display "\t")
  (display info)
  (newline))
