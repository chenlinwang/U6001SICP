(load "../BookExercise/basic.scm")
(load "../BookImplementation/sec2.4.scm")
(define (loadimp25) (load "../BookImplementation/sec2.5.scm"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; section 2.5.1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; in this section we are going to implement a generic arithmetic system with rational numbers and complex numbers and single scheme numbers

;; wishful thinking
(define (add a1 a2) (generic-apply 'add a1 a2))
(define (sub a1 a2) (generic-apply 'sub a1 a2))
(define (mul a1 a2) (generic-apply 'mul a1 a2))
(define (div a1 a2) (generic-apply 'div a1 a2))

;; the rational number installer
(define (install-rational)
  ;; the generator
  (define (generate numerator denominator)
    (cond ((= denominator 0)
           (error "rational number denominator should not be zero -- " numerator denominator))
          (else
           (let ((absolute-n (absolute numerator))
                 (absolute-d (absolute denominator)))
             (let ((nd-gcd (gcd absolute-n absolute-d)))
               (if (< (* numerator denominator) 0)
                   (cons 'rational (cons (- (/ absolute-n nd-gcd)) (/ absolute-d nd-gcd)))
                   (cons 'rational (cons (/ absolute-n nd-gcd) (/ absolute-d nd-gcd)))))))))

  ;; the selector
  (define numerator cadr)
  (define denominator cddr)

  ;; the operator
  (define (add r1 r2)
    (let ((n1 (numerator r1))
          (n2 (numerator r2))
          (d1 (denominator r1))
          (d2 (denominator r2)))
      (make-rational (+ (* n1 d2) (* n2 d1))
                     (* d1 d2))))
  (define (sub r1 r2)
    (add r1 (make-rational (- (numerator r2))
                           (denominator r2))))
  (define (mul r1 r2)
    (let ((n1 (numerator r1))
          (n2 (numerator r2))
          (d1 (denominator r1))
          (d2 (denominator r2)))
      (make-rational (* n1 n2) (* d1 d2))))
  (define (div r1 r2)
    (cond ((= (numerator r2) 0) (error "rational number divide by zero -- " r2))
          (else (mul r1 (make-rational (denominator r2)
                                       (numerator r2))))))

  ;; register the selectors
  (map (lambda (op proc) (put-wrap op 'rational proc))
       (list 'numerator 'denominator)
       (list numerator denominator))

  ;; register the operators
  (map (lambda (op proc) (put-wrap op 'rationalrational proc))
       (list 'add 'sub 'mul 'div)
       (list add sub mul div))

  ;; register the generator
  (put-wrap 'generate 'rational generate)

  ;; print result
  (print "rational number arithmetic installed!"))

;; install the installer
(install-rational)

;; install rational number generator
(define (make-rational n d) ((get-wrap 'generate 'rational) n d))

;; install the selectors
(define (nume r) (generic-apply 'numerator r))
(define (deno r) (generic-apply 'denominator r))

;; ;; test
;; (define x (make-rational 1 -4))
;; (print (nume x))
;; (print (deno x))
;; (define y (make-rational 2 3))
;; (print (map (lambda (p) (p x y))
;;             (list add sub mul div)))
;; (exit)

;; the complex number installer
(define (install-complex)
  ;; the generator
  (define (generate c) (cons 'complex c))

  ;; the selector
  (define complex-number cdr)

  ;; register the operators
  (map (lambda (op proc)
         (put-wrap op 'complexcomplex
                   (lambda (c1 c2)
                     (generate (proc (complex-number c1)
                                     (complex-number c2))))))
       (list 'add 'sub 'mul 'div)
       (list complex-add complex-sub complex-mul complex-div))

  ;; register the selectors on rectangular or polar
  (map (lambda (op proc) (put-wrap op 'complex (lambda (c)
                                                 (proc (complex-number c)))))
       (list 'real 'imag 'magn 'angl)
       (list real-part imag-part magn-part angl-part))

  ;; register the selector on complex number
  (put-wrap 'complex-number 'complex complex-number)

  ;; register the generator
  (put-wrap 'generate 'complex generate)

  ;; print the result
  (print "complex number arithemtic installed!"))


;; install the installer
(install-complex)

;; install complex number generators
(define (make-complex-rectangular real imag)
  ((get-wrap 'generate 'complex) (make-from-rectangular real imag)))
(define (make-complex-polar magn angl)
  ((get-wrap 'generate 'complex) (make-from-polar magn angl)))

;; install the selectors
(define (real c) (generic-apply 'real c))
(define (imag c) (generic-apply 'imag c))
(define (magn c) (generic-apply 'magn c))
(define (angl c) (generic-apply 'angl c))

;; ;; test
;; (define x (make-complex-rectangular 1 2))
;; (print (map (lambda (p) (p x))
;;             (list real imag magn angl)))
;; (define y (make-complex-polar 1 (/ PI 4)))
;; (print (map (lambda (p) (p x y))
;;             (list add sub mul div)))
;; (exit)

;; the ordinary operator installer
(define (install-ordinary)
  ;; the generator
  (define (generate n) (cons 'number n))

  ;; the selector
  (define num cdr)

  ;; register the generator
  (put-wrap 'generate 'number generate)

  ;; register the selector
  (put-wrap 'number 'number num)

  ;; register the operators
  (map (lambda (op proc) (put-wrap op 'numbernumber
                                   (lambda (n1 n2) (proc (num n1)
                                                         (num n2)))))
       (list 'add 'sub 'mul 'div)
       (list + - * /))

  ;; print result
  (print "ordinary number arithemtic installed!"))


;; install the installer
(install-ordinary)

;; install the generator
(define (make-ordinary n) ((get-wrap 'generate 'number) n))

;; install the selector
(define (num n) (generic-apply 'number n))

;; ;; test
;; (define x (make-ordinary 1))
;; (print (num x))
;; (define y (make-ordinary 2))
;; (print (map (lambda (p) (p x y))
;;             (list add sub mul div)))
;; (exit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; section 2.5.2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;------------------------------
;; working on the coercion
;; we have: r->o, r->c, o->c

(define (put-coercion t1 t2 proc)
  (put! 'coercion (symbol-append t1 t2) proc))

(define (get-coercion t1 t2)
  (get 'coercion (symbol-append t1 t2) #f))

;; coercion installer
(define (install-coercion)
  ;; coercion operator
  (define (r->o r) (make-ordinary (/ (nume r) (deno r))))
  (define (r->c r) (make-complex-rectangular (/ (nume r) (deno r)) 0))
  (define (o->c o) (make-complex-rectangular (num o) 0))

  ;; register with the coercion
  (map put-coercion
       (list 'rational 'rational 'number)
       (list 'number 'complex 'complex)
       (list r->o r->c o->c))
  ;; print result
  (print "coercion operator installed!"))

;; install
(install-coercion)

;; raise table to get the upper class coercion name
(define (put-raise t1 t2) (put! 'raise t1 t2))
(define (get-raise t) (get 'raise t #f))

;; raise table install for existing type
(define (install-raise)
  (map put-raise
       (list 'rational 'number)
       (list 'number 'complex))
  (print "raise hierarchy installed!"))
;; install
(install-raise)

;; rewrite the generic-apply to one or two
(define (generic-apply op . arg)
  (let ((type (apply symbol-append (map tag-name arg))))
    (let ((proc (get-wrap op type)))
      (cond (proc (apply proc arg))
            ((= 1 (length arg))
             (let ((raise-type (get-raise type)))
               (cond (raise-type (apply generic-apply (list op ((get-coercion type
                                                                              raise-type)
                                                                (car arg))))))))
            ((= 2 (length arg))
             (let ((type (map tag-name arg)))
               (cond ((get-coercion (car type)
                                    (cadr type))
                      (apply generic-apply (list op ((get-coercion (car type)
                                                                   (cadr type))
                                                     (car arg))
                                                 (cadr arg))))
                     ((get-coercion (cadr type)
                                    (car type))
                      (apply generic-apply (list op (car arg)
                                                 ((get-coercion (cadr type)
                                                                (car type))
                                                  (cadr arg)))))
                     (else (error "can't cover these type -- " type))))
             )
            (else (error "three arguments conversion not implemented!"))))))

;; ;; test
;; (define n1 (make-ordinary 1))
;; (define r1 (make-rational 1 2))
;; (define c1 (make-complex-rectangular 1 1))
;; (define c2 (make-complex-polar 2 (/ PI 4)))
;; (define testlist (list n1 r1 c1 c2))
;; ;; single test
;; (print (map real testlist))
;; (print (map magn testlist))

;; ;; double test
;; (print (map add
;;             (list r1 r1 r1 n1 n1)
;;             (list n1 c1 c2 c1 c2)))
;; (print (map add testlist testlist))
;; (exit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; section 2.5.3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; polynomials packages
(define (install-polynomials-arithmetic)
  ;; terms data
  (define term-connect cons)
  (define exp-gen cons)
  (define (term-gen exp scalar)
    (cond ((not (= (length exp)
                   (length scalar)))
           (error "input exp term length not equal to scalar term length -- " exp scalar))
          (else (ext-quick-sort (map exp-gen exp scalar) car))))

  (define term-empty? null?)
  (define term-null (list))

  (define (term-append t e s)
    (ext-quick-sort (term-connect (exp-gen e s) t) car))

  ;; selector
  (define term-first car)
  (define term-rest cdr)
  (define exp-exp car)
  (define exp-scalar cdr)

  ;; operator
  (define (term-add t1 t2)
    (cond ((term-empty? t1) t2)
          ((term-empty? t2) t1)
          (else (let iter ((r1 t1)
                           (r2 t2))
                  (cond ((term-empty? r1) r2)
                        ((term-empty? r2) r1)
                        (else (let ((e1 (exp-exp (term-first r1)))
                                    (e2 (exp-exp (term-first r2))))
                                (cond ((= e1 e2) (term-connect (exp-gen e1 (+ (exp-scalar (term-first r1))
                                                                              (exp-scalar (term-first r2))))
                                                               (iter (term-rest r1)
                                                                     (term-rest r2))))
                                      ((< e1 e2) (term-connect (term-first r1)
                                                               (iter (term-rest r1)
                                                                     r2)))
                                      ((> e1 e2) (term-connect (term-first r2)
                                                               (iter r1
                                                                     (term-rest r2))))
                                      (else (error "no idea why can't compare =_= -- " e1 e2))))))))))

  (define (term-sub t1 t2)
    (term-add t1 (map (lambda (e) (exp-gen (exp-exp e) (- (exp-scalar e)))) t2)))

  (define (term-mul t1 t2)
    (let iter ((r term-null)
               (l t2))
      (cond ((term-empty? l) r)
            (else (iter (term-add r
                                  (map (lambda (e)
                                         (exp-gen (+ (exp-exp e) (exp-exp (term-first l)))
                                                  (* (exp-scalar e) (exp-scalar (term-first l)))))
                                       t1)
                            )
                        (term-rest l))))))

  ;; ;;test
  ;; (define p1 (term-gen (list 1 3 0) (list 2 1 3)))
  ;; (define p2 (term-gen (list 0) (list 1)))
  ;; (print p1)
  ;; (print p2)
  ;; (define p3 (term-append p2 1 1))
  ;; (print p3)
  ;; (print (term-empty? term-null))
  ;; (print (term-empty? p3))
  ;; (print (term-first p1))
  ;; (print (term-rest p1))
  ;; (print (exp-exp (term-first p1)))
  ;; (print (exp-scalar (term-first p1)))
  ;; (print (term-add p1 p3))
  ;; (print (term-sub p1 p3))
  ;; (print (term-mul p1 p3))
  ;; (exit)

  ;;----------
  ;; the polynomial
  (define (poly-gen variable exp scalar) (list 'polynomial variable (term-gen exp scalar)))

  ;; selector
  (define poly-var cadr)
  (define poly-term caddr)

  ;; operator
  (define poly-empty? (lambda (p) (term-empty? (poly-term p))))
  (define (poly-same-var? p1 p2) (eq? (poly-var p1)
                                      (poly-var p2)))
  (define (poly-null var) (list 'polynomial var term-null))

  ;; register the calculation
  (map (lambda (op proc)
         (put-wrap op 'polynomialpolynomial
                   (lambda (p1 p2)
                     (cond ((poly-same-var? p1 p2)
                            (list 'polynomial (poly-var p1)
                                  (proc (poly-term p1)
                                        (poly-term p2))))))))
       (list 'add 'sub 'mul)
       (list term-add term-sub term-mul))

  ;; register the generator
  (map (lambda (op proc) (put-wrap op 'polynomial proc))
       (list 'generate 'vari 'term 'poly-empty?)
       (list poly-gen poly-var poly-term poly-empty?))

  ;; finish install the polynimal
  (print "polynomial packages installed!"))

;; install and wrap
(install-polynomials-arithmetic)
(define make-polynomial (get-wrap 'generate 'polynomial))
(define vari (get-wrap 'vari 'polynomial))
(define term (get-wrap 'term 'polynomial))

;; ;;test
;; (define p1 (make-polynomial 'x (list 1 3 0) (list 2 1 3)))
;; (define p2 (make-polynomial 'x (list 0 1) (list 1 1)))
;; (print p1)
;; (print p2)
;; (print (vari p1))
;; (print (term p1))
;; (print (map (lambda (p) (p p1 p2))
;;             (list add sub mul)))
;; (exit)

;;--------------------
;; generic within
;; since the ordinary number here is not directly the scheme number, so we need to rewrite the add and others
;; wishful thinking
(define (add a1 a2)
  (cond ((and (number? a1) (number? a2)) (+ a1 a2))
        ((number? a1) (generic-apply 'add (make-ordinary a1) a2))
        ((number? a2) (generic-apply 'add (make-ordinary a2) a1))
        (else (generic-apply 'add a1 a2))))
(define (sub a1 a2)
  (cond ((and (number? a1) (number? a2)) (- a1 a2))
        ((number? a1) (generic-apply 'sub (make-ordinary a1) a2))
        ((number? a2) (generic-apply 'sub (make-ordinary a2) a1))
        (else (generic-apply 'sub a1 a2))))
(define (mul a1 a2)
  (cond ((and (number? a1) (number? a2)) (* a1 a2))
        ((number? a1) (generic-apply 'mul (make-ordinary a1) a2))
        ((number? a2) (generic-apply 'mul (make-ordinary a2) a1))
        (else (generic-apply 'mul a1 a2))))
(define (div a1 a2)
  (cond ((and (number? a1) (number? a2)) (/ a1 a2))
        ((number? a1) (generic-apply 'div (make-ordinary a1) a2))
        ((number? a2) (generic-apply 'div (make-ordinary a2) a1))
        (else (generic-apply 'div a1 a2))))

;; ;; test
;; (define n1 (make-ordinary 1))
;; (define r1 (make-rational 1 2))
;; (define c1 (make-complex-rectangular 1 1))
;; (define c2 (make-complex-polar 2 (/ PI 4)))
;; (define testlist (list n1 r1 c1 c2))
;; ;; single test
;; (print (map real testlist))
;; (print (map magn testlist))

;; ;; double test
;; (print (map add
;;             (list r1 r1 r1 n1 n1)
;;             (list n1 c1 c2 c1 c2)))
;; (print (map add testlist testlist))

;; ;; polynomials
;; (define p1 (make-polynomial 'x (list 1 3 0) (list 2 1 3)))
;; (define p2 (make-polynomial 'x (list 0 1) (list 1 1)))
;; (print p1)
;; (print p2)
;; (print (vari p1))
;; (print (term p1))
;; (print (map (lambda (p) (p p1 p2))
;;             (list add sub mul)))
;; (exit)

;;--------------------
;; reinstall the polynomials within
(define (re-install-polynomials-arithmetic)
  ;; terms data
  (define term-connect cons)
  (define exp-gen cons)
  (define (term-gen exp scalar)
    (cond ((not (= (length exp)
                   (length scalar)))
           (error "input exp term length not equal to scalar term length -- " exp scalar))
          (else (ext-quick-sort (map exp-gen exp scalar) car))))

  (define term-empty? null?)
  (define term-null (list))

  (define (term-append t e s)
    (ext-quick-sort (term-connect (exp-gen e s) t) car))

  ;; selector
  (define term-first car)
  (define term-rest cdr)
  (define exp-exp car)
  (define exp-scalar cdr)

  ;; operator
  (define (term-add t1 t2)
    (cond ((term-empty? t1) t2)
          ((term-empty? t2) t1)
          (else (let iter ((r1 t1)
                           (r2 t2))
                  (cond ((term-empty? r1) r2)
                        ((term-empty? r2) r1)
                        (else (let ((e1 (exp-exp (term-first r1)))
                                    (e2 (exp-exp (term-first r2))))
                                (cond ((= e1 e2) (term-connect (exp-gen e1 (add (exp-scalar (term-first r1))
                                                                                (exp-scalar (term-first r2))))
                                                               (iter (term-rest r1)
                                                                     (term-rest r2))))
                                      ((< e1 e2) (term-connect (term-first r1)
                                                               (iter (term-rest r1)
                                                                     r2)))
                                      ((> e1 e2) (term-connect (term-first r2)
                                                               (iter r1
                                                                     (term-rest r2))))
                                      (else (error "no idea why can't compare =_= -- " e1 e2))))))))))

  (define (term-sub t1 t2)
    (cond ((term-empty? t1) t2)
          ((term-empty? t2) t1)
          (else (let iter ((r1 t1)
                           (r2 t2))
                  (cond ((term-empty? r1) r2)
                        ((term-empty? r2) r1)
                        (else (let ((e1 (exp-exp (term-first r1)))
                                    (e2 (exp-exp (term-first r2))))
                                (cond ((= e1 e2) (term-connect (exp-gen e1 (sub (exp-scalar (term-first r1))
                                                                                (exp-scalar (term-first r2))))
                                                               (iter (term-rest r1)
                                                                     (term-rest r2))))
                                      ((< e1 e2) (term-connect (term-first r1)
                                                               (iter (term-rest r1)
                                                                     r2)))
                                      ((> e1 e2) (term-connect (term-first r2)
                                                               (iter r1
                                                                     (term-rest r2))))
                                      (else (error "no idea why can't compare =_= -- " e1 e2))))))))))

  (define (term-mul t1 t2)
    (let iter ((r term-null)
               (l t2))
      (cond ((term-empty? l) r)
            (else (iter (term-add r
                                  (map (lambda (e)
                                         (exp-gen (+ (exp-exp e) (exp-exp (term-first l)))
                                                  (mul (exp-scalar e) (exp-scalar (term-first l)))))
                                       t1)
                                  )
                        (term-rest l))))))

  ;; ;;test
  ;; (define p1 (term-gen (list 1 3 0) (list 2 1 3)))
  ;; (define p2 (term-gen (list 0) (list 1)))
  ;; (print p1)
  ;; (print p2)
  ;; (define p3 (term-append p2 1 1))
  ;; (print p3)
  ;; (print (term-empty? term-null))
  ;; (print (term-empty? p3))
  ;; (print (term-first p1))
  ;; (print (term-rest p1))
  ;; (print (exp-exp (term-first p1)))
  ;; (print (exp-scalar (term-first p1)))
  ;; (print (term-add p1 p3))
  ;; (print (term-sub p1 p3))
  ;; (print (term-mul p1 p3))
  ;; (exit)

  ;;----------
  ;; the polynomial
  (define (poly-gen variable exp scalar) (list 'polynomial variable (term-gen exp scalar)))

  ;; selector
  (define poly-var cadr)
  (define poly-term caddr)

  ;; operator
  (define (poly-null var) (list 'polynomial var term-null))
  (define poly-scalar-polynomials-for-computation '*scalar-polynomial*)
  (define poly-empty? (lambda (p) (term-empty? (poly-term p))))
  (define (poly-same-var? p1 p2) (or (eq? (poly-var p1)
                                          (poly-var p2))
                                     (eq? '*scalar-polynomial*
                                          (poly-var p1))
                                     (eq? '*scalar-polynomial*
                                          (poly-var p2))))

  ;; register the calculation
  (map (lambda (op proc)
         (put-wrap op 'polynomialpolynomial
                   (lambda (p1 p2)
                     (cond ((poly-same-var? p1 p2)
                            (list 'polynomial (cond ((not (eq? '*scalar-polynomial* (poly-var p1)))
                                                     (poly-var p1))
                                                    ((not (eq? '*scalar-polynomial* (poly-var p2)))
                                                     (poly-var p2))
                                                    (else '*scalar-polynomial*))
                                  (proc (poly-term p1)
                                        (poly-term p2))))))))
       (list 'add 'sub 'mul)
       (list term-add term-sub term-mul))

  ;; register the generator
  (map (lambda (op proc) (put-wrap op 'polynomial proc))
       (list 'generate 'vari 'term 'poly-empty?)
       (list poly-gen poly-var poly-term poly-empty?))

  ;; finish install the polynimal
  (print "polynomial packages re-installed!")

  ;; the coercion
  ;; coercion operator
  (define (r->p r) (make-polynomial '*scalar-polynomial* (list 0) (list r)))
  (define (n->p n) (make-polynomial '*scalar-polynomial* (list 0) (list n)))
  (define (c->p c) (make-polynomial '*scalar-polynomial* (list 0) (list c)))

  ;; register with the coercion
  (map put-coercion
       (list 'rational 'number 'complex)
       (list 'polynomial 'polynomial 'polynomial)
       (list r->p n->p c->p))
  ;; print result
  (print "coercion operator for the polynomial installed!"))

;; install and wrap
(re-install-polynomials-arithmetic)
(define make-polynomial (get-wrap 'generate 'polynomial))
(define vari (get-wrap 'vari 'polynomial))
(define term (get-wrap 'term 'polynomial))

;; ;; polynomials
;; (define p1 (make-polynomial 'x (list 1 3 0) (list 2 1 3)))
;; (define p2 (make-polynomial 'x (list 0 1) (list 1 1)))
;; (print p1)
;; (print p2)
;; (print (vari p1))
;; (print (term p1))
;; (print (map (lambda (p) (p p1 p2))
;;             (list add sub mul)))
;; (exit)
;; polynomials with
;; (define s1 (make-polynomial 'y (list 1 0) (list 1 1)))
;; (define s2 (make-polynomial 'y (list 2 0) (list 1 1)))
;; (define p1 (make-polynomial 'x (list 2 1 0) (list s1 s2 1)))
;; (define p2 (make-polynomial 'x (list 1 0) (list s1 1)))
;; (print p1)
;; (print p2)
;; (print (map (lambda (p) (p p1 p2))
;;             (list add sub mul)))
;; (exit)
