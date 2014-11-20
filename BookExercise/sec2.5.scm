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
