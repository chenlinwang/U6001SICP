(load "../BookExercise/basic.scm")
(define (loadexe25) (load "../BookExercise/sec2.5.scm"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.78
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The internal procedures in the scheme-number package are essentially nothing more than calls to the primitive procedures +, -, etc. It was not possible to use the primitives of the language directly because our type-tag system requires that each data object have a type attached to it. In fact, however, all Lisp implementations do have a type system, which they use internally. Primitive predicates such as symbol? and number? determine whether data objects have particular types. Modify the definitions of type-tag, contents, and attach-tag from section 2.4.2 so that our generic system takes advantage of Scheme's internal type system. That is to say, the system should work as before except that ordinary numbers should be represented simply as Scheme numbers rather than as pairs whose car is the symbol scheme-number.

;; We just need to rewrite the =tag-name= procedure to let it distinguish the number and return the right tag.
(load "../BookImplementation/sec2.5.scm")
(define (tag-name data)
  (cond ((number? data) 'number)
        (else (car data))))

;; the ordinary operator
(define (install-ordinary-again)
  ;; the generator
  (define (generate n) n)

  ;; the selector
  (define num (lambda (n) n))

  ;; register the generator
  (put-wrap 'generate 'number generate)

  ;; register the selector
  (put-wrap 'number 'number num)

  (map (lambda (op proc) (put-wrap op 'numbernumber proc))
       (list 'add 'sub 'mul 'div)
       (list + - * /))

  ;; print result
  (print "ordinary number arithemtic installed again for the simpler representation!"))

;; install
(install-ordinary-again)
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
;; Exercise 2.79
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define a generic equality predicate equ? that tests the equality of two numbers, and install it in the generic arithmetic package. This operation should work for ordinary numbers, rational numbers, and complex numbers.

;; the equal installer
(define (install-equal)
  (define (ordinary-equal o1 o2)
    (all (map (lambda (eq proc) (eq (proc o1)
                                    (proc o2)))
              (list =)
              (list num))))

  (define (rational-equal r1 r2)
    (all (map (lambda (eq proc) (eq (proc r1)
                                    (proc r2)))
              (list = =)
              (list nume deno))))

  (define (complex-equal c1 c2)
    (all (map (lambda (eq proc) (eq (proc c1)
                                    (proc c2)))
              (list = =)
              (list real imag))))

  ;; register the equals
  (map (lambda (type proc) (put-wrap 'equal (symbol-append type type) proc))
       (list 'number 'rational 'complex)
       (list ordinary-equal rational-equal complex-equal))

  ;; print result
  (print "equal operator installed!"))

;; install
(install-equal)
;; install the equ
(define (equ a1 a2) (generic-apply 'equal a1 a2))

;; ;; test
;; (print (map equ
;;             (list 1 1 (make-rational 1 1) (make-rational 1 2) (make-complex-rectangular 1 1) (make-complex-rectangular 1 2) (make-complex-rectangular 1 0) (make-complex-polar 1 (/ PI 4)) (make-complex-polar 1 (/ PI 3)))
;;             (list 1 2 (make-rational 1 1) (make-rational 1 1) (make-complex-rectangular 1 1) (make-complex-rectangular 1 1) (make-complex-polar 1 0) (make-complex-polar 1 (/ PI 4)) (make-complex-polar 1 (/ PI 4)))))
;; (exit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.80
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define a generic predicate =zero? that tests if its argument is zero, and install it in the generic arithmetic package. This operation should work for ordinary numbers, rational numbers, and complex numbers.

;; the zero installer
(define (install-zero)
  (define (ordinary-zero? o) (= (num o) 0))
  (define (rational-zero? r) (= (nume r) 0))
  (define (complex-zero? c) (and (= (real c) 0)
                                 (= (imag c) 0)))

  ;; register
  (map (lambda (type proc) (put-wrap 'zero? type proc))
       (list 'number 'rational 'complex)
       (list ordinary-zero? rational-zero? complex-zero?))
  ;; print result
  (print "zero? operator installed!"))

;; install
(install-zero)
;; install zero?
(define (zero? a) (generic-apply 'zero? a))

;; ;; test
;; (print (zero? 0))
;; (print (zero? (make-rational 0 4)))
;; (print (zero? (make-complex-polar 0 0)))
;; (print (zero? (make-complex-rectangular 0 0)))
;; (exit)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.81
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Louis Reasoner has noticed that apply-generic may try to coerce the arguments to each other's type even if they already have the same type. Therefore, he reasons, we need to put procedures in the coercion table to "coerce" arguments of each type to their own type. For example, in addition to the scheme-number->complex coercion shown above, he would do:
;; (define (scheme-number->scheme-number n) n)
;; (define (complex->complex z) z)
;; (put-coercion 'scheme-number 'scheme-number
;;               scheme-number->scheme-number)
;; (put-coercion 'complex 'complex complex->complex)
;; ** a
;; With Louis's coercion procedures installed, what happens if apply-generic is called with two arguments of type scheme-number or two arguments of type complex for an operation that is not found in the table for those types? For example, assume that we've defined a generic exponentiation operation:
;; (define (exp x y) (apply-generic 'exp x y))
;; and have put a procedure for exponentiation in the Scheme-number package but not in any other package:
;; ;; following added to Scheme-number package
;; (put 'exp '(scheme-number scheme-number)
;; (lambda (x y) (tag (expt x y)))) ; using primitive expt
;; What happens if we call exp with two complex numbers as arguments?
;; ** b
;; Is Louis correct that something had to be done about coercion with arguments of the same type, or does apply-generic work correctly as is?
;; ** c
;; Modify apply-generic so that it doesn't try coercion if the two arguments have the same type.

;; c
(define (generic-apply op . arg)
  (let ((type (apply symbol-append (map tag-name arg))))
    (let ((proc (get-wrap op type)))
      (cond (proc (apply proc arg))
            ((= 1 (length arg))
             (let ((raise-type (get-raise type)))
               (cond (raise-type (apply generic-apply (list op ((get-coercion type
                                                                              raise-type)
                                                                (car arg)))))
                     (else (error "there is no coersion for type with op -- " type op)))))
            ((= 2 (length arg))
             (let ((type (map tag-name arg)))
               (cond ((eq? (car type) (cadr type))
                      ;; raise both type to get there
                      (let ((raise-type (get-raise (car type))))
                        (cond (raise-type (apply generic-apply (cons op (map (get-coercion (car type) raise-type)
                                                                             arg))))
                              (else (error "there is no coersion for type with op -- " (car type) op)))))
                     ((get-coercion (car type)
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
                     (else (error "can't cover these type -- " type)))))
            (else (error "three arguments conversion not implemented!"))))))

;; ;; test the exp
;; (define (number-exp b e)
;;   (exponentiation (num b) (num e)))
;; (put-wrap 'exp 'numbernumber number-exp)
;; (define (exp b e) (generic-apply 'exp b e))

;; ;; test
;; (define n1 (make-ordinary 2))
;; (define r1 (make-rational 2 1))
;; (define c1 (make-complex-rectangular 2 1))
;; ;; test
;; (print (map exp (list n1 n1 r1) (list n1 r1 r1)))
;; ;; (print (exp c1 c1))
;; (exit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.85
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This section mentioned a method for ``simplifying'' a data object by lowering it in the tower of types as far as possible. Design a procedure drop that accomplishes this for the tower described in exercise 2.83. The key is to decide, in some general way, whether an object can be lowered. For example, the complex number 1.5 + 0i can be lowered as far as real, the complex number 1 + 0i can be lowered as far as integer, and the complex number 2 + 3i cannot be lowered at all. Here is a plan for determining whether an object can be lowered: Begin by defining a generic operation project that ``pushes'' an object down in the tower. For example, projecting a complex number would involve throwing away the imaginary part. Then a number can be dropped if, when we project it and raise the result back to the type we started with, we end up with something equal to what we started with. Show how to implement this idea in detail, by writing adropprocedure that drops an object as far as possible. You will need to design the various projection operations53 and install project as a generic operation in the system. You will also need to make use of a generic equality predicate, such as described in exercise 2.79. Finally, use drop to rewrite apply-generic from exercise 2.84 so that it ``simplifies'' its answers.
(define (put-drop type proc) (put! 'drop type proc))
(define (get-drop type) (get 'drop type #f))

(define (install-drop)
  (define (c->o c)
    (cond ((= (imag c) 0) (make-ordinary (real c)))
          (else #f)))

  (put-drop 'complex c->o)
  (print "drop installed!"))

;; install the package!
(install-drop)
(define (drop i)
  (cond ((number? i) (make-ordinary i))
        ((boolean? i) i)
        (else
         (let ((proc (get-drop (tag-name i))))
           (cond (proc (let ((simplified (proc i)))
                         (cond (simplified (drop simplified))
                               (else i))))
                 (else i))))))

;; rewrite
(define (generic-apply op . arg)
  (let ((result
         (let ((type (apply symbol-append (map tag-name arg))))
           (let ((proc (get-wrap op type)))
             (cond (proc
                    (apply proc arg))
                   ((= 1 (length arg))
                    (let ((raise-type (get-raise type)))
                      (cond (raise-type (apply generic-apply (list op ((get-coercion type
                                                                                     raise-type)
                                                                       (car arg)))))
                            (else (error "there is no coersion for type with op -- " type op)))))
                   ((= 2 (length arg))
                    (let ((type (map tag-name arg)))
                      (cond ((eq? (car type) (cadr type))
                             ;; raise both type to get there
                             (let ((raise-type (get-raise (car type))))
                               (cond (raise-type (apply generic-apply (cons op (map (get-coercion (car type) raise-type)
                                                                                    arg))))
                                     (else (error "there is no coersion for type with op -- " (car type) op)))))
                            ((get-coercion (car type)
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
                            (else (error "can't cover these type -- " type)))))
                   (else (error "three arguments conversion not implemented!")))))))
    (drop result)))

;; ;; test
;; (define c1 (make-complex-rectangular 1 1))
;; (define c2 (make-complex-rectangular 1 -1))
;; (print (add c1 c1))
;; (print (add c1 c2))
;; (exit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.86
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Suppose we want to handle complex numbers whose real parts, imaginary parts, magnitudes, and angles can be either ordinary numbers, rational numbers, or other numbers we might wish to add to the system. Describe and implement the changes to the system needed to accommodate this. You will have to define operations such as sine and cosine that are generic over ordinary numbers and rational numbers.


;; too much to do, will try to reinvent the system later ;P
;; will have to reload the add at the most basic level and then rewrite the most generic addition so that it will do the actual addition only when the two inputs are numbers.

;; complex data arithmetic, using the wishful thinking
(define (complex-add z1 z2)
  (make-from-rectangular (add (real-part z1)
                              (real-part z2))
                         (add (imag-part z1)
                              (imag-part z2))))

(define (complex-sub z1 z2)
  (make-from-rectangular (sub (real-part z1)
                              (real-part z2))
                         (sub (imag-part z1)
                              (imag-part z2))))

(define (complex-mul z1 z2)
  (make-from-polar (mul (magn-part z1)
                        (magn-part z2))
                   (add (angl-part z1)
                        (angl-part z2))))

(define (complex-div z1 z2)
  (make-from-polar (div (magn-part z1)
                        (magn-part z2))
                   (sub (angl-part z1)
                        (angl-part z2))))

(define (re-install-complex)
  ;; the generator
  (define (generate c) (cons 'complex c))

  ;; the selector
  (define complex-number cdr)

  (map (lambda (op proc)
         (put-wrap op 'complexcomplex
                   (lambda (c1 c2)
                     (generate (proc (complex-number c1)
                                     (complex-number c2))))))
       (list 'add 'sub 'mul 'div)
       (list complex-add complex-sub complex-mul complex-div))

  ;; print the result
  (print "complex number arithemtic reinstalled!"))

;; redo the reconstruction
(define (down-to-number a)
  (let ((t (tag-name a)))
    (cond ((eq? t 'number) a)
          ((eq? t 'rational) (/ (nume a) (deno a)))
          (else (error "can't change to calculating number -- " a)))))

;; install square
(define (install-square)
  (define (rational-square r) (let ((n (nume r))
                                    (d (deno r)))
                                (make-rational (* n n) (* d d))))
  (define (ordinary-square o) (* o o))
  (define (complex-square c) (let ((r (real c))
                                   (i (imag c)))
                               (make-complex-rectangular (sub (generic-square r)
                                                              (generic-square i))
                                                         (mul 2 (mul r i)))))
  (map (lambda (type proc) (put-wrap 'square type proc))
       (list 'rational 'number 'complex)
       (list rational-square ordinary-square complex-square))

  ;; print the result
  (print "square operator installed!"))
;; install and wrap
(install-square)
(define (generic-square a) (generic-apply 'square a))

;; sin operator
(define (install-sin)
  (define (rational-sin r) (sin (/ (nume r) (deno r))))
  (define (ordinary-sin o) (sin o))

  (map (lambda (type proc) (put-wrap 'sin type proc))
       (list 'rational 'number)
       (list rational-sin ordinary-sin))
  (print "sin operator installed!"))
;; install and wrap
(install-sin)
(define (generic-sin a) (generic-apply 'sin a))

;; cos operator
(define (install-cos)
  (define (rational-cos r) (cos (/ (nume r) (deno r))))
  (define (ordinary-cos o) (cos o))

  (map (lambda (type proc) (put-wrap 'cos type proc))
       (list 'rational 'number)
       (list rational-cos ordinary-cos))
  (print "cos operator installed!"))
;; install and wrap
(install-cos)
(define (generic-cos a) (generic-apply 'cos a))

;; install the rectangular package
(define (re-install-rectangular-package)
  ;; the rectangluar form
  (define complex? (make-tag-rec 'rectangular))
  (define real-part cadr)
  (define imag-part cddr)
  (define (magn-part z)
    (sqrt (down-to-number (add (generic-square (real-part z))
                               (generic-square (imag-part z))))))
  (define (angl-part z)
    (xy2angle (down-to-number (real-part z))
              (down-to-number (imag-part z))))
  (map (lambda (op proc)
         (put-wrap op 'rectangular proc))
       (list 'complex? 'real-part 'imag-part 'magn-part 'angl-part)
       (list complex? real-part imag-part magn-part angl-part))
  (print "rectangular complex number reinstalled!"))

;; the polar form
(define (make-from-polar magn angl)
  (make-tag-gen 'polar (cons magn (get-group (down-to-number angl) PI))))

;; install the polar package
(define (re-install-polar-package)
  ;; the polar form
  (define complex? (make-tag-rec 'polar))
  (define magn-part cadr)
  (define angl-part cddr)
  (define (real-part z) (mul (magn-part z)
                             (generic-cos (angl-part z))))
  (define (imag-part z) (mul (magn-part z)
                             (generic-sin (angl-part z))))
  (map (lambda (op proc)
         (put-wrap op 'polar proc))
       (list 'complex? 'real-part 'imag-part 'magn-part 'angl-part)
       (list complex? real-part imag-part magn-part angl-part))
  (print "polar complex number reinstalled!"))

;; reinstall
(re-install-complex)
(re-install-polar-package)
(re-install-rectangular-package)

;; ;; test
;; (define r1 (make-rational 1 2))
;; (define r2 (make-rational 1 3))
;; (define o1 (make-ordinary 1.5))
;; (define o2 (make-ordinary 3.9))
;; (define c1 (make-complex-rectangular r1 o1))
;; (define c2 (make-complex-rectangular o2 r2))
;; (define c3 (make-complex-polar r1 o1))
;; (define c4 (make-complex-polar o2 r2))
;; (print (add c1 c1))
;; (print (map (lambda (p) (p c1 c2))
;;             (list add sub mul div)))
;; (print (map (lambda (p) (p c3 c4))
;;             (list add sub mul div)))
;; (exit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.87
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Install =zero?= for polynomials in the generic arithmetic package. This will allow adjoin-term to work for polynomials with coefficients that are themselves polynomials.
(define (install-poly-zero)
  (define (z p)
    (let ((t (term p)))
      (and (= (length t) 1)
           (= (caar t) 0))))

  (put-wrap 'zero? 'polynomial z)
  (print "zero? for the polynomial installed!"))

(install-poly-zero)
;; ;; test
;; (define p0 (make-polynomial '*scalar-polynomial* (list 0) (list 1)))
;; (print (zero? p0))
;; (exit)
