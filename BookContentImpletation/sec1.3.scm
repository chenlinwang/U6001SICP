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

;;Fixed point
;;1: by find-root
(define (find-fix1 f pos neg tolerance)
    (find-root (lambda (x) (- (f x) x)) pos neg tolerance))

;;2: by iterative method
(define (find-fix2 f init tolerance)
    (let ((next (f init)))
      (if (close-enough? init next tolerance)
          init
          (find-fix2 f next tolerance))))
