(load "../BookExercise/basic.scm")
(define (loadimp24) (load "../BookImplementation/sec2.4.scm"))
(require-extension mathh)
(include "mathh-constants")
;; utility for the angle from two dimension coordinates
(define (xy2angle x y)
  (cond ((= x 0)
         (cond ((= y 0) 0)
               ((> y 0) (/ PI 2))
               ((< y 0) (- (/ PI 2)))))
        ((> x 0) (atan (/ y x)))
        ((< x 0)
         (cond ((= y 0) PI)
               ((> y 0) (+ PI (atan (/ y x))))
               ((< y 0) (- (atan (/ y x)) PI))))))

;; ;; test
;; (print (map xy2angle
;;             (list 0 1 1 0 -1 -1 -1 0 1)
;;             (list 0 0 1 1 1 0 -1 -1 -1)))
;; (exit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; section 2.4.1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; omit the underlying data representation, using the wishful thinkng
;; suppost for any complex data, we have the generator make-from-rectangular and make-from-polar, operation the  real-part, imag-part, magn-part and angl-part already, we could implement the data arithmetic as:

;; complex data arithmetic
(define (complex-add z1 z2)
  (make-from-rectangular (+ (real-part z1)
                            (real-part z2))
                         (+ (imag-part z1)
                            (imag-part z2))))

(define (complex-sub z1 z2)
  (make-from-rectangular (- (real-part z1)
                            (real-part z2))
                         (- (imag-part z1)
                            (imag-part z2))))

(define (complex-mul z1 z2)
  (make-from-polar (* (magn-part z1)
                      (magn-part z2))
                   (+ (angl-part z1)
                      (angl-part z2))))

(define (complex-div z1 z2)
  (make-from-polar (/ (magn-part z1)
                      (magn-part z2))
                   (- (angl-part z1)
                      (angl-part z2))))

;; so now we have the choices to implement the generator and the selector. We could be using two sorts of implementations

;; 1 all from rectangular represetation
(define (make-from-rectangular real imag)
  (cons real imag))
(define real-part car)
(define imag-part cdr)
(define (make-from-polar magn angl)
  (cons (* magn (cos angl))
        (* magn (sin angl))))
(define (magn-part z)
  (sqrt (+ (square (real-part z))
           (square (imag-part z)))))
(define (angl-part z)
  (xy2angle (real-part z)
            (imag-part z)))

;; ;; test
;; (define x (make-from-rectangular 1 1))
;; (define y (make-from-polar (sqrt 2) (/ PI 4)))
;; (print (map (lambda (p)
;;               (print (apply p (list x)))
;;               (print (apply p (list y))))
;;             (list real-part imag-part magn-part angl-part)))
;; (exit)

;; 2 all from polar representation
(define (make-from-polar magn angl)
  (cons magn angl))
(define magn-part car)
(define angl-part cdr)
(define (make-from-rectangular real imag)
  (cons (sqrt (+ (square real)
                 (square imag)))
        (xy2angle real
                  imag)))
(define (real-part z)
  (* (magn-part z)
     (cos (angl-part z))))
(define (imag-part z)
  (* (magn-part z)
     (sin (angl-part z))))

;; ;; test
;; (define x (make-from-rectangular 1 1))
;; (define y (make-from-polar (sqrt 2) (/ PI 4)))
;; (print (map (lambda (p)
;;               (print (apply p (list x)))
;;               (print (apply p (list y))))
;;             (list real-part imag-part magn-part angl-part)))
;; (exit)

;; ;; test for the whole part
;; (define x (make-from-rectangular 2 3))
;; (define y (make-from-polar 4 (/ PI 6)))
;; (print (list x y))
;; (print (map (lambda (p) (apply p (list x y)))
;;             (list complex-add complex-sub complex-mul complex-div)))
;; (exit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; section 2.4.2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; using the taged data to represent implement the generic function
;; tag data
(define (make-tag-gen tag data)
  (cons tag data))

(define (make-tag-rec tag)
  (lambda (data) (and (pair? data)
                      (eq? tag (car data)))))

(define tag-name car)

;; the rectangular form
(define (make-from-rectangular real imag)
  (make-tag-gen 'rectangular (cons real imag)))
(define rectangular-complex? (make-tag-rec 'rectangular))
(define rect-real-part cadr)
(define rect-imag-part cddr)
(define (rect-magn-part z)
  (sqrt (+ (square (rect-real-part z))
           (square (rect-imag-part z)))))
(define (rect-angl-part z)
  (xy2angle (rect-real-part z)
            (rect-imag-part z)))


;; the polar
(define (make-from-polar magn angl)
  (make-tag-gen 'polar (cons magn angl)))
(define polar-complex? (make-tag-rec 'polar))
(define pola-magn-part cadr)
(define pola-angl-part cddr)
(define (pola-real-part z) (* (pola-magn-part z)
                              (cos (pola-angl-part z))))
(define (pola-imag-part z) (* (pola-magn-part z)
                              (sin (pola-angl-part z))))

;; generic selector
(define (real-part z)
  (cond ((rectangular-complex? z) (rect-real-part z))
        ((polar-complex? z) (pola-real-part z))
        (else (error "undefined tagged data -- " z))))

(define (imag-part z)
  (cond ((rectangular-complex? z) (rect-imag-part z))
        ((polar-complex? z) (pola-imag-part z))
        (else (error "undefined tagged data -- " z))))

(define (magn-part z)
  (cond ((rectangular-complex? z) (rect-magn-part z))
        ((polar-complex? z) (pola-magn-part z))
        (else (error "undefined tagged data -- " z))))

(define (angl-part z)
  (cond ((rectangular-complex? z) (rect-angl-part z))
        ((polar-complex? z) (pola-angl-part z))
        (else (error "undefined tagged data -- " z))))

;; ;; test for the whole part
;; (define x (make-from-rectangular 2 3))
;; (define y (make-from-polar 4 (/ PI 6)))
;; (print (list x y))
;; (print (map (lambda (p) (apply p (list x y)))
;;             (list complex-add complex-sub complex-mul complex-div)))
;; (exit)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; section 2.4.3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; using the put and get method instead of the recreating the generic methods

;; the put and get, we will be using the property list
(define (put-complex op type proc)
  (put! 'complex-number (symbol-append op type) proc))

(define (get-complex op type)
  (get 'complex-number (symbol-append op type)))


;; the rectangular form
(define (make-from-rectangular real imag)
  (make-tag-gen 'rectangular (cons real imag)))

;; install the rectangular package
(define (install-rectangular-package)
  ;; the rectangluar form
  (define complex? (make-tag-rec 'rectangular))
  (define real-part cadr)
  (define imag-part cddr)
  (define (magn-part z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angl-part z)
    (xy2angle (real-part z)
              (imag-part z)))
  (map (lambda (op proc)
         (put-complex op 'rectangular proc))
       (list 'complex? 'real-part 'imag-part 'magn-part 'angl-part)
       (list complex? real-part imag-part magn-part angl-part))
  (print "rectangular complex number installed!"))

;; the polar form
(define (make-from-polar magn angl)
  (make-tag-gen 'polar (cons magn angl)))

;; install the polar package
(define (install-polar-package)
  ;; the polar form
  (define complex? (make-tag-rec 'polar))
  (define magn-part cadr)
  (define angl-part cddr)
  (define (real-part z) (* (magn-part z)
                           (cos (angl-part z))))
  (define (imag-part z) (* (magn-part z)
                           (sin (angl-part z))))
  (map (lambda (op proc)
         (put-complex op 'polar proc))
       (list 'complex? 'real-part 'imag-part 'magn-part 'angl-part)
       (list complex? real-part imag-part magn-part angl-part))
  (print "polar complex number installed!"))

;; generic apply
(define (generic-apply op type . arg)
  (apply (get-complex op type) arg))

;; generic selector
(define (real-part z)
  (generic-apply 'real-part (tag-name z) z))
(define (imag-part z)
  (generic-apply 'imag-part (tag-name z) z))
(define (magn-part z)
  (generic-apply 'magn-part (tag-name z) z))
(define (angl-part z)
  (generic-apply 'angl-part (tag-name z) z))

;; ;; install
;; (install-rectangular-package)
;; (install-polar-package)
;; ;; test
;; (define x (make-from-rectangular 1 2))
;; (print x)
;; (print (map (lambda (proc) (apply proc (list x)))
;;             (list real-part imag-part magn-part angl-part)))

;; (define y (make-from-polar 1 (/ PI 4)))
;; (print y)
;; (print (map (lambda (proc) (apply proc (list y)))
;;             (list real-part imag-part magn-part angl-part)))

;; (exit)
