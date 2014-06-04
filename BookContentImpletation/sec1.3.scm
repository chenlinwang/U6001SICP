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
