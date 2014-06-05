;;high-order procedure to calculate the sum of a serie
(define (sumserie term next)
    (define (sumspe a b s)
        (if (> a b)
            s
            (sumspe (next a) b (+ (term a) s))))
  (define (newsum a b)
      (sumspe a b 0))
  newsum)

;;term options
(define (identity x) x)
(define (square x) (* x x))
(define (cube x) (* x x x))
(define (pi-8 x) (/ 1.0 (+ (* 16 (square x)) (- (* 16 x)) (- 3))))

;;next options
(define (increment x) (+ 1 x))

(define (integral f dx)
    (define (intiter a b s)
        (if (> a b)
            s
            (intiter (+ a dx) b (+ (f a) s))))
  (define (int a b)
      (* (intiter a b 0) dx))
  int)

(define (integral2 f dx)
    (define (incrementdx x)
        (+ x dx))
  (define withoutdx (sumserie f incrementdx))
  (define (int a b) (* dx (withoutdx a b)))
  int)

;;middle point procedure
(define (close-enough? a b tolerance)
    (< (abs (- a b)) tolerance))

(define (average a b) (/ (+ a b) 2.0))

(define (find-root f pos neg tolerance)
    (let ((mid (average pos neg)))
      (if (close-enough? pos neg tolerance)
          mid
          (let ((midf (f mid)))
            (cond ((> midf 0) (find-root f midf neg tolerance))
                  ((< midf 0) (find-root f pos midf tolerance))
                  (else mid))))))

(define (x a) a)
(define (x2 a) (* a a))
(define (rx a) (+ 1 (/ 1.0 a)))

;;Fixed point
;;1: by find-root
(define (find-fix1 f pos neg tolerance)
    (find-root (lambda (x) (- (f x) x)) pos neg tolerance))

;;2: by iterative method
(define find-fix2-max-iter 100000)
(define (find-fix2 f init tolerance)
    (define (inside init count)
        (let ((next (f init)))
          (if (or (close-enough? init next tolerance) (> count find-fix2-max-iter))
              (list init count)
              (inside next (+ count 1)))))
  (inside init 0))

;;To improve some iterative methods
;;average damping
(define (average-damping f)
    (lambda (x) (average x (f x))))

;;newton's methods
;;derivatives
(define (differentiate g dx)
    (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define dx 0.001)
(define (diff-cur g)
    (differentiate g dx))

(define (newton f)
    (define (g x) (- (f x) x))
    (lambda (x) (- x (/ (g x) (let ((dg ((diff-cur g) x)))
                                 (if (= 0 dg)
                                     1.0
                                     dg))))))

(define (fixpoint-improve impro fun init tolerance)
    (find-fix2 (impro fun) init tolerance))

(define (make-sqrt1 x)
    (lambda (y) (/ (* x 1.0) y)))

(define (make-sqrt2 x)
    (lambda (y) (+ (- x (square y)) y)))

(define makefunlist (list make-sqrt1 make-sqrt2))

(define init 10)
(define tolerance 0.001)
(define (test-on-sqrt input)
    (define functionlist (map (lambda (f) (f input))
                              makefunlist))
    (newline)
  (display "With only the fix-point iterative method:\n")
  (define ofp (map (lambda (f) (find-fix2 f init tolerance))
                   functionlist))
  (display ofp)
  (display "\nWith average damping:\n")
  (define afp (map (lambda (f) (find-fix2 (average-damping f) init tolerance))
                   functionlist))
  (display afp)
  (display "\nWith newton's method:\n")
  (define nfp (map (lambda (f) (find-fix2 (newton f) init tolerance))
                   functionlist))
  (display nfp)
  (newline)
  (list ofp afp nfp))

;(test-on-sqrt 2)
