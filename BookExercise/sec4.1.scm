(load "../BookExercise/basic")
(define (loadexe41) (load "sec4.1"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Exercise 4.1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notice that we cannot tell whether the metacircular evaluator evaluates operands from left to right or from right to left. Its evaluation order is inherited from the underlying Lisp: If the arguments to cons in list-of-values are evaluated from left to right, then list-of-values will evaluate operands from left to right; and if the arguments to cons are evaluated from right to left, then list-of- values will evaluate operands from right to left.
;; Write a version of list-of-values that evaluates operands from left to right regardless of the order of evaluation in the underlying Lisp. Also write a version of list-of-values that evaluates operands from right to left.

;; This function is not really working for the eval and apply are not implemented.

;; (define (list-of-values-lr exp env)
;;   (map (lambda (e) (eval e env))
;;        exp))

;; (define (list-of-values-rl exp env)
;;   (define (reverse-list l)
;;     (if (null? l)
;;         l
;;         (append (reverse-list (cdr l)) (car l))))
;;   (reverse-list (list-of-values-lr (reverse-list exp) env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Exercise 4.4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Recall the definitions of the special forms and and or from chapter 1:
;; 1. and: The expressions are evaluated from left to right. If any expression evaluates to false, false is returned; any remaining expressions are not evaluated. If all the expressions evaluate to true values, the value of the last expression is returned. If there are no expressions then true is returned.
;; 2. or: The expressions are evaluated from left to right. If any expression evaluates to a true value, that value is returned; any remaining expressions are not evaluated. If all expressions evaluate to false, or if there are no expressions, then false is returned.
;; Install and and or as new special forms for the evaluator by defining appropriate syntax procedures and evaluation procedures eval-and and eval-or. Alternatively, show how to implement and and or as derived expressions.
(load "../BookImplementation/sec4.1.scm")

(define and? (tag-named 'and))
(define and-body cdr)
(define and-body-first car)
(define and-body-remove-first cdr)

(define (and-eval exp env)
  (let iter ((body (and-body exp)))
    (cond ((null? body) #t)
          ((not (eval (and-body-first body) env)) #f)
          (else (iter (and-body-remove-first body))))))

(define or? (tag-named 'or))
(define or-body cdr)
(define or-body-first car)
(define or-body-remove-first cdr)

(define (or-eval exp env)
  (let iter ((body (or-body exp)))
    (cond ((null? body) #f)
          ((eval (or-body-first body) env) #t)
          (else (iter (or-body-remove-first body))))))

(define (eval exp env)
  (cond ((self-evaluator? exp) (self-eval exp env))
        ((string? exp) (string-eval exp env))
        ((define? exp) (define-eval exp env))
        ((variable? exp) (variable-eval exp env))
        ((set? exp) (set-eval exp env))
        ((if? exp) (if-eval exp env))
        ((begin? exp) (begin-eval exp env))
        ((lambda? exp) (lambda-eval exp env))
        ((cond? exp) (cond-eval exp env))
        ((let? exp) (eval (let->application exp) env))
        ((and? exp) (and-eval exp env))
        ((or? exp) (or-eval exp env))
        ((application? exp) (application-eval exp env))
        (else
         (error "evaluation prefix not found" exp))))

;; ;; test
;; (geval '(and (= 1 1) (< 1 2) (= 0 1)))
;; (geval '(or (= 1 2) (< 2 1) (= 1 0)))
;; (exit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 4.5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheme allows an additional syntax for cond clauses, (<test> => <recipient>). If <test> evaluates to a true value, then <recipient> is evaluated. Its value must be a procedure of one argument ; this procedure is then invoked on the value of the <test>, and the result is returned as the value of the cond expression. For example

;; (cond ((assoc 'b '((a 1) (b 2))) => cadr)
;;       (else false))

;; returns 2. Modify the handling of cond so that it supports this extended syntax.

(define (cond-eval exp env)
  (let ((body (cond-body exp)))
    (if (null? body)
        'system-no-return-value
        (let ((rest (cond-body-remove-first body))
              (pre (cond-body-first-pre body))
              (true (cond-body-first-true body)))
          (if (and (not (null? true))
                   (eq? '=> (car true)))
              (set! true (cdr true)))
          (if (eq? 'else pre)
              (eval (begin-gen true) env)
              (eval (if-gen
                     (list pre (begin-gen true) (cond-gen rest))) env))))))

;; ;; test
;; (geval '(cond ((= 1 1) 3)
;;               (else 4)))
;; (exit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 4.7
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Let* is similar to let, except that the bindings of the let variables are performed sequentially from left to right, and each binding is made in an environment in which all of the preceding bindings are visible. For example
;; #+BEGIN_SRC scheme
;; (let* ((x 3)
;;        (y (+ x 2))
;;        (z (+ x y 5)))
;;   (* x z))
;; #+END_SRC
;; returns 39. Explain how a let* expression can be rewritten as a set of nested let expressions, and write a procedure let*->nested-lets that performs this transformation. If we have already implemented let (exercise 4.6) and we want to extend the evaluator to handle let*, is it sufficient to add a clause to eval whose action is
;; #+BEGIN_SRC scheme
;; (eval (let*->nested-lets exp) env)
;; #+END_SRC
;; or must we explicitly expand let* in terms of non-derived expressions?

;; selector
(define let*-param cadr)
(define let*-body cddr)
(define let*-param-first-pair car)
(define let*-param-remove-first-pair cdr)
(define let*-param-pair-name car)
(define let*-param-pair-value cadr)

;; let generator
(define let*-gen (tag-add 'let*))

;; operator
(define let*? (tag-named 'let*))

;; the transformation
(define (let*->nested-lets exp)
  (let ((param (let*-param exp))
        (body (let*-body exp)))
    (cond ((null? param) (begin-gen body))
          (else
           (let ((pair (let*-param-first-pair param))
                 (remain (let*-param-remove-first-pair param)))
             (let-gen (list (list pair)
                            (let*->nested-lets (let*-gen (cons remain body))))))))))

;; eval
(define (eval exp env)
  (cond ((self-evaluator? exp) (self-eval exp env))
        ((string? exp) (string-eval exp env))
        ((define? exp) (define-eval exp env))
        ((variable? exp) (variable-eval exp env))
        ((set? exp) (set-eval exp env))
        ((if? exp) (if-eval exp env))
        ((begin? exp) (begin-eval exp env))
        ((lambda? exp) (lambda-eval exp env))
        ((cond? exp) (cond-eval exp env))
        ((let? exp) (eval (let->application exp) env))
        ((let*? exp) (eval (let*->nested-lets exp) env))
        ((and? exp) (and-eval exp env))
        ((or? exp) (or-eval exp env))
        ((application? exp) (application-eval exp env))
        (else
         (error "evaluation prefix not found" exp))))

;; ;; test
;; (geval (let*->nested-lets '(let* ((a 1)
;;                                   (b (+ a 1)))
;;                              (+ a a)
;;                              (+ b b))))
;; (exit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 4.8
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ``Named let'' is a variant of let that has the form
;; (let <var> <bindings> <body>)
;; The <bindings> and <body> are just as in ordinary let, except that <var> is bound within <body> to a procedure whose body is <body> and whose parameters are the variables in the <bindings>. Thus, one can repeatedly execute the <body> by invoking the procedure named <var>. For example, the iterative Fibonacci procedure (section 1.2.2) can be rewritten using named let as follows:
;; (define (fib n)
;;   (let fib-iter ((a 1)
;; (b 0)
;;                  (count n))
;;     (if (= count 0)
;;         b
;;         (fib-iter (+ a b) a (- count 1)))))
;; Modify let->combination of exercise 4.6 to also support named let.

;; operator
(define (let-named? exp)
  (symbol? (cadr exp)))
;;selector
(define let-name cadr)
(define (let-without-name exp)
  (let ((exp (cdr exp)))
    (let-gen (cons (let-param exp)
                   (let-body exp)))))

;; rewrite the let
(define (let->application exp)
  (let* ((lambda-name (if (let-named? exp)
                          (let-name exp)
                          #f))
         (exp (if (let-named? exp)
                  (let-without-name exp)
                  exp))
         (param (let-param exp))
         (body (let-body exp))
         (lambda-param (list))
         (input-param (list)))
    ;; separate the parameter into application
    (do ((remain param (let-param-remove-first-pair remain)))
        ((null? remain))
      (let ((p (let-param-first-pair remain)))
        (set! lambda-param (append lambda-param (list (let-param-pair-name p))))
        (set! input-param (append input-param (list (let-param-pair-value p))))))
    ;; (print lambda-param)
    ;; (print input-param)
    ;; (exit)
    (let ((lambda-fun (lambda-gen (cons lambda-param body))))
      (cond (lambda-name
             (begin-gen (list (define-gen (list lambda-name lambda-fun))
                                     (cons lambda-name input-param))))
            (else (cons lambda-fun input-param))))))

;; ;; test
;; (geval '(let iter ((a 5)
;;                    (b 0))
;;           (cond ((= a 0) b)
;;                 (else (iter (- a 1) (+ b 1))))))
;; (exit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Exercise 4.9
;; Many languages support a variety of iteration constructs, such as do, for, while, and until. In Scheme, iterative processes can be expressed in terms of ordinary procedure calls, so special iteration constructs provide no essential gain in computational power. On the other hand, such constructs are often convenient. Design some iteration constructs, give examples of their use, and show how to implement them as derived expressions.

(define do? (tag-named 'do))

;;selector
(define do-param cadr)
(define do-param-remove-first cdr)
(define do-param-first-name caar)
(define do-param-first-value cadar)
(define do-param-first-step caddar)
(define do-check caddr)
(define do-body cdddr)


(define (do->let exp)
  (let ((param (do-param exp))
        (check (do-check exp))
        (body (do-body exp))
        (initial-param (list))
        (step-up (list)))
    ;; to the intial param with the step param
    (do ((param-remain param (do-param-remove-first param-remain)))
        ((null? param-remain))
      (set! initial-param (append initial-param (list (list (do-param-first-name param-remain)
                                                            (do-param-first-value param-remain)))))
      (set! step-up (append step-up (list (do-param-first-step param-remain)))))

    ;; (print initial-param)
    ;; (print step-up)
    ;; (exit)
    (let-gen (list 'iter initial-param (cond-gen (list check
                                                       (cons 'else (append body (list (cons 'iter step-up))))))))))

;; rewrite the eval
(define (eval exp env)
  (cond ((self-evaluator? exp) (self-eval exp env))
        ((string? exp) (string-eval exp env))
        ((define? exp) (define-eval exp env))
        ((variable? exp) (variable-eval exp env))
        ((set? exp) (set-eval exp env))
        ((if? exp) (if-eval exp env))
        ((begin? exp) (begin-eval exp env))
        ((lambda? exp) (lambda-eval exp env))
        ((cond? exp) (cond-eval exp env))
        ((and? exp) (and-eval exp env))
        ((or? exp) (or-eval exp env))
        ((let? exp) (eval (let->application exp) env))
        ((let*? exp) (eval (let*->nested-lets exp) env))
        ((do? exp) (eval (do->let exp) env))
        ((application? exp) (application-eval exp env))
        (else
         (error "evaluation prefix not found" exp))))

;; ;; test
;; (geval '(do ((a 5 (- a 1))
;;              (b 0 (+ b 1)))
;;             ((or (= a 0)
;;                  (= b 5))
;;              (+ a 1)
;;              (- b 1))
;;           (+ a a)
;;           (+ b b)))
;; (exit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 4.10
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; By using data abstraction, we were able to write an eval procedure that is independent of the particular syntax of the language to be evaluated. To illustrate this, design and implement a new syntax for Scheme by modifying the procedures in this section, without changing eval or apply.

;; I will illustrate it by changing the sequence of the do, the check will be at the front and them the initials and finally, the body

;; (define do-param caddr)
;; (define do-check cadr)

;; ;; test
;; (geval '(do ((or (= a 0)
;;                  (= b 5))
;;              (+ a 1)
;;              (- b 1))
;;             ((a 5 (- a 1))
;;              (b 0 (+ b 1)))
;;           (+ a a)
;;           (+ b b)))
;; (exit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 4.13
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheme allows us to create new bindings for variables by means of define, but provides no way to get rid of bindings. Implement for the evaluator a special form make-unbound! that removes the binding of a given symbol from the environment in which the make-unbound! expression is evaluated. This problem is not completely specified. For example, should we remove only the binding in the first frame of the environment? Complete the specification and justify any choices you make.

;; using the new environmet settings
(define (frame-delete f name)
  ;; function to remove the name
  (define (remove-n l n)
    (cond ((= n 0) (cdr l))
          (else (cons (car l) (remove-n (cdr l) (- n 1))))))

  ;; counting the location of the name
  (let ((number (let iter ((rest (frame-name f))
                           (number 0))
                  (cond ((null? rest) 'name-not-found)
                        ((eq? name (car rest)) number)
                        (else (iter (cdr rest) (+ number 1)))))))
    (cond ((eq? number 'name-not-found)
           'name-not-found)
          (else
           (set-car! f (remove-n (frame-name f) number))
           (set-cdr! f (remove-n (frame-value f) number))
           #t))))

(define (env-delete env name)
  (let ((result (frame-delete (env-frame env) name)))
    (cond ((eq? result 'name-not-found)
           (error "name not found -- " name))
          (else result))))

(define (env-delete-all-in-this env name)
  (let iter ((result (frame-delete (env-frame env) name)))
    (cond ((eq? result 'name-not-found)
           #t)
          (else (iter (frame-delete (env-frame env) name))))))

(define (env-delete-all-in-all env name)
  (env-delete-all-in-this env name)
  (cond ((env-parent-null? env) #t)
        (else (env-delete-all-in-all (env-parent-env env) name))))

;; ;; test
;; (define e1 (env-gen-global-env '(x y) '(1 2)))
;; (define e2 (env-gen e1 '(x y x x) '(3 4 5 6)))
;; (print e2)
;; (env-delete e2 'x)
;; (print e2)
;; ;; (env-delete-all-in-this e2 'x)
;; (env-delete-all-in-all e2 'x)
;; (print e2)
;; (exit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; exercise 4.21
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Amazingly, Louis's intuition in exercise 4.20 is correct. It is indeed possible to specify recursive procedures without using letrec (or even define), although the method for accomplishing this is much more subtle than Louis imagined. The following expression computes 10 factorial by applying a recursive factorial procedure:

;; ((lambda (n)
;;    ((lambda (fact)
;;       (fact fact n))
;;     (lambda (ft k)
;;       (if (= k 1) 1
;;           (* k (ft ft (- k 1)))))))
;;  10)

;; 1. Check (by evaluating the expression) that this really does compute factorials. Devise an analogous expression for computing Fibonacci numbers.
;; 2. Consider the following procedure, which includes mutually recursive internal definitions:

;; (define (f x)
;;   (define (even? n)
;;     (if (= n 0)
;;         true
;;         (odd? (- n 1))))
;;   (define (odd? n)
;;     (if (= n 0)
;;         false
;;         (even? (- n 1))))
;;   (even? x))

;; Fill in the missing expressions to complete an alternative definition of f, which uses neither internal definitions nor letrec:

;;   ((lambda (even? odd?)
;;      (even? even? odd? x))
;;    (lambda (ev? od? n)
;;      (if (= n 0) true (od? <??> <??> <??>)))
;;    (lambda (ev? od? n)
;;      (if (= n 0) false (ev? <??> <??> <??>)))))


;; 1 the fibonacci
(define (fib n)
  (if (or (= n 0)
          (= n 1))
      1
      (+ (fib (- n 1))
         (fib (- n 2)))))

;; the form without the define
((lambda (x)
  (lambda (n)
    (if (or (= n 0)
            (= n 1))
        1
        (+ ((x x) (- n 1))
           ((x x) (- n 2))))))
 (lambda (x)
  (lambda (n)
    (if (or (= n 0)
            (= n 1))
        1
        (+ ((x x) (- n 1))
           ((x x) (- n 2)))))))

;; extracte the y combinator
(define y-combinator (lambda (g)
                       ((lambda (f)
                          (f f))
                        (lambda (u)
                          (g
                           (lambda (v) ((u u) v)))))))

(define fib (y-combinator
             (lambda (x)
               (lambda (n)
                 (if (or (= n 0)
                         (= n 1))
                     1
                     (+ (x (- n 1))
                        (x (- n 2))))))))

;; ;; test
;; (print (map fib '(0 1 2 3 4 5 6 7)))
;; (exit)

;; 2 fill out
;; (print ((lambda (x)
;;           ((lambda (even? odd?)
;;              (even? even? odd? x))
;;            (lambda (ev? od? n)
;;              (if (= n 0) #t (od? ev? od? (- n 1))))
;;            (lambda (ev? od? n)
;;              (if (= n 0) #f (ev? od? ev? (- n 1))))))
;;         4))
;; (exit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 4.22
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extend the evaluator in this section to support the special form let.
;; done in the implementation

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 4.23
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Design and carry out some experiments to compare the speed of the original metacircular evaluator with the version in this section. Use your results to estimate the fraction of time that is spent in analysis versus execution for various procedures.

;; we could use the following function:
(define (r n)
  (if (< n 0)
      0
      (r (- n 1))))
