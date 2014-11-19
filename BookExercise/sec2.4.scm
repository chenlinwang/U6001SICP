(load "../BookExercise/basic.scm")
(define (load24) (load "sec2.4"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.73
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section 2.3.2 described a program that performs symbolic differentiation:

;; (define (deriv exp var)
;;   (cond ((number? exp) 0)
;;         ((variable? exp) (if (same-variable? exp var) 1 0))
;;         ((sum? exp)
;;          (make-sum (deriv (addend exp) var)
;;                    (deriv (augend exp) var)))
;;         ((product? exp)
;;          (make-sum
;;           (make-product (multiplier exp)
;;                         (deriv (multiplicand exp) var))
;;           (make-product (deriv (multiplier exp) var)
;;                         (multiplicand exp))))
;;         ;; <more rules can be added here>
;;         (else (error "unknown expression type -- DERIV" exp))))

;; We can regard this program as performing a dispatch on the type of the expression to be differentiated. In this situation the ``type tag'' of the datum is the algebraic operator symbol (such as +) and the operation being performed is deriv. We can transform this program into data-directed style by rewriting the basic derivative procedure as
;; (define (deriv exp var)
;;    (cond ((number? exp) 0)
;;          ((variable? exp) (if (same-variable? exp var) 1 0))
;;          (else ((get 'deriv (operator exp)) (operands exp) var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;; Explain what was done above. Why can't we assimilate the predicates number? and same-variable? into the data-directed dispatch?
;; Write the procedures for derivatives of sums and products, and the auxiliary code required to install them in the table used by the program above.
(define variable? symbol?)
(define (same-variable? exp var) (eq? exp var))
(define (put-der op proc)
  (put! 'derivitive op proc))
(define (get-der op)
  (get 'derivitive op))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get-der (operator exp)) (operands exp) var))))

;; install the add
(define (install-derivitive-add)
  (define (add-drive arg var)
    (cons '+ (map (lambda (a) (deriv a var))
                  arg)))
  (put-der '+ add-drive)
  (print "add derivitive installed!"))

;; install the sum
(define (install-derivitive-mul)
  (define (mul-derive arg var)
    (cons '+ (map (lambda (a)
                    (cons '* (cons (deriv a var) (removemap (lambda (r) (eq? a r))
                                                            cons
                                                            (list)
                                                            arg))))
                  arg)))
  (put-der '* mul-derive)
  (print "multiply derivitive installed!"))

;; ;; test
(install-derivitive-add)
(install-derivitive-mul)
;; (print (deriv '(+ x (* x x)) 'x))
;; (exit)

;; Choose any additional differentiation rule that you like, such as the one for exponents (exercise 2.56), and install it in this data-directed system.
(define (exponent? exp) (and (pair? exp)
                             (eq? (car exp) 'exp)))

(define (install-derivitive-exp)
  (define (exp-derive arg var)
    (let ((base (car arg))
          (exp-num (cadr arg))
          (self (cons 'exp arg)))
      (cond ((number? base)
             (cond ((number? exp-num) 0)
                   ((variable? exp-num)
                    (if (eq? exp-num var)
                        (list '* self (list 'ln base))
                        0))
                   (else (list '* self (list 'ln base) (deriv exp-num)))))
            ((variable? base)
             (if (eq? base var)
                 (cond ((number? exp-num) (list '* exp-num
                                                   (list 'exp base (list '- exp-num 1))))
                       ((variable? exp-num)
                        (if (eq? exp-num var)
                            (list '* (list '+ (list 'ln base) 1) self)
                            (list '* exp-num
                                  (list 'exp base (list '- exp-num 1)))))
                       (else (list '* (list '+ (list 'ln base) 1) self (deriv exp-num))))
                 (cond ((number? exp-num) 0)
                       ((variable? exp-num)
                        (if (eq? exp-num var)
                            (list '* self (list 'ln base))
                            0))
                       (else (list '* self (list 'ln base) (deriv exp-num))))))
            (else
             (cond ((number? exp-num) (list '* exp-num
                                            (list 'exp base (list '- exp-num 1))
                                            (deriv base var)))
                   ((variable? exp-num)
                    (if (eq? exp-num var)
                        (list '+ (list '* base (list 'ln base))
                              (list '* (deriv base var)
                                    exp-num))
                        (list '* exp-num
                              (list 'exp base (list '- exp-num 1))
                              (deriv base var))))
                   (else (list '* self (list '+ (list '* (deriv exp-num var)
                                                     (list 'ln base))
                                            (list '* exp-num
                                                  (list '/ (deriv base var)
                                                        base))))))))))
  (put-der 'exp exp-derive)
  (print "exponent derivitive installed!"))

;; ;; test
(install-derivitive-exp)
;; (print (deriv '(exp y x) 'x))
;; (exit)

;; ** In this simple algebraic manipulator the type of an expression is the algebraic operator that binds it together. Suppose, however, we indexed the procedures in the opposite way, so that the dispatch line in deriv looked like:
;; #+BEGIN_SRC scheme
;; ((get (operator exp) 'deriv) (operands exp) var)
;; #+END_SRC
;; What corresponding changes to the derivative system are required?

;; Actually, according to the property of the property list in scheme, we just need to rewrite the =put!= procedure. It is basically the same.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.74
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insatiable Enterprises, Inc., is a highly decentralized conglomerate company consisting of a large number of independent divisions located all over the world. The company's computer facilities have just been interconnected by means of a clever network-interfacing scheme that makes the entire network appear to any user to be a single computer. Insatiable's president, in her first attempt to exploit the ability of the network to extract administrative information from division files, is dismayed to discover that, although all the division files have been implemented as data structures in Scheme, the particular data structure used varies from division to division. A meeting of division managers is hastily called to search for a strategy to integrate the files that will satisfy headquarters' needs while preserving the existing autonomy of the divisions.

;; Show how such a strategy can be implemented with data-directed programming. As an example, suppose that each division's personnel records consist of a single file, which contains a set of records keyed on employees' names. The structure of the set varies from division to division. Furthermore, each employee's record is itself a set (structured differently from division to division) that contains information keyed under identifiers such as address and salary. In particular:

(define (find id))
(define (get division op))
;; ** a
;;  Implement for headquarters a get-record procedure that retrieves a specified employee's record from a specified personnel file. The procedure should be applicable to any division's file. Explain how the individual divisions' files should be structured. In particular, what type information must be supplied?
(define (get-record id)
  (let ((data (find id)))
    ((get (car data) 'all) data)))

;; ** b
;; Implement for headquarters a get-salary procedure that returns the salary information from a give employee's record from any division's personnel file. How should the record be structured in order to make this operation work?
(define (get-salary id)
  (let ((data (find id)))
    ((get (car data) 'salary) data)))

;; ** c
;; Implement for headquarters a find-employee-record procedure. This should search all the divisions' files for the record of a given employee and return the record. Assume that this procedure takes as arguments an employee's name and a list of all the divisions' files.

;; ** d
;; When Insatiable takes over a new company, what changes must be made in order to incorporate the new personnel information into the central system?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.75
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implement the constructor make-from-mag-ang in message-passing style. This procedure should be analogous to the make-from-real-imag procedure given above.
(define (make-from-mag-ang mag ang)
  (define (dispatch m)
    (cond ((eq? m 'mag) mag)
          ((eq? m 'ang) ang)
          ((eq? m 'real) (* mag (cos ang)))
          ((eq? m 'imag) (* mag (sin ang)))
          (else
           (error "undefined operator -- " m))))
  dispatch)
