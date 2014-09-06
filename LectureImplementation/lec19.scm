(define (loadlec19) (load "../LectureImplementation/lec19"))
(load "../BookExercise/basic")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simple Number Evaluator -- simple-eval
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For recognizing the tags in the evaluation.
(define (tag-recognizer e symbol) (and (list? e) (eq? (car e) symbol)))

;; For the sum symbol in the simple-eval.
(define (sum-exp? e) (tag-recognizer e 'plus*))

;; The simple evaluator for only the plus
(define (simple-eval e)
  (cond ((number? e) e)
        ((sum-exp? e) (eval-sum e))
        (else (error "Undefined Symbol for the simple-eval -- " e))))

;; The eval-sum
(define (eval-sum e)
  (+ (simple-eval (cadr e))
     (simple-eval (caddr e))))

;; ;; Test
;; (print-out (simple-eval '(plus* 1 (plus* 8 9))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Frame class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (make-frame . eep)
  (if (not (null? eep))
      (if (eq? 'frame-class (ask (car eep) 'class-name))
          (set! eep (car eep))
          (error "Enclosing Environment Pointer not a frame -- " eep)))
  (define bindings (list))
  (lambda (method)
    (case method
      ((class-name) (class-variable 'frame-class))
      ((lookup) (lambda (self key)
                  (let ((result
                         (let iter ((rest bindings))
                           (cond ((null? rest)
                                  'value-not-found)
                                 (else
                                  (let ((tmp (car rest)))
                                    (if (eq? (car tmp) key)
                                        (cdr tmp)
                                        (iter (cdr rest)))))))))
                    (cond ((and (null? eep)
                                (eq? result 'value-not-found))
                           (error "Undefined Varible -- " key))
                          ((eq? result 'value-not-found)
                           (ask eep 'lookup key))
                          (else
                           result)))))
      ((store) (lambda (self key value)
                 (set! bindings (cons (cons key value) bindings))))
      ((locateframe) (lambda (self key)
                       (let ((result
                              (let iter ((rest bindings))
                                (cond ((null? rest)
                                       'value-not-found)
                                      (else
                                       (let ((tmp (car rest)))
                                         (if (eq? (car tmp) key)
                                             (cdr tmp)
                                             (iter (cdr rest)))))))))
                         (cond ((and (null? eep)
                                     (eq? result 'value-not-found))
                                (error "Undefined Varible -- " key))
                               ((eq? result 'value-not-found)
                                (ask eep 'locateframe key))
                               (else
                                self)))))
      ;; check all the bindings
      ((bindings) (class-variable bindings))
      (else (error "Undefined Method for Frame Class -- " method)))))

;; ;; Test
;; (define f (make-frame))
;; (ask f 'store 'x 1)
;; (ask f 'store 'y 2)
;; (define g (make-frame f))
;; (ask g 'store 'x 3)
;; (print-out (ask f 'lookup 'x))
;; (print-out (ask f 'lookup 'y))
;; (print-out (ask g 'lookup 'x))
;; (print-out (ask g 'lookup 'y))
;; (print-out (ask g 'lookup 'z))
;; (exit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The define evaluator -- define-eval
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The environment
(define environment (make-frame))

;; wraper for the environment
(define (lookup key)
  (ask environment 'lookup key))

;; define check
(define (define-exp? e)
  (tag-recognizer e 'define*))

;;;; store by name ;;;;
;; this define-eval will bind the expression to the variable and eval them only when needed.
;; define-sum-eval
(define (define-eval-sum1 e)
;;  (display "eva-sum1:") (print-out e)
  (+ (define-eval1 (cadr e))
     (define-eval1 (caddr e))))

;; eval-define1
(define (eval-define1 e)
  (let ((key (cadr e))
        (value (caddr e)))
    (ask environment 'store key value)
    (define-eval1 value)))

;; define-eval
(define (define-eval1 e)
;;  (display "eva1:") (print-out e)
  (cond ((number? e) e)
        ((sum-exp? e) (define-eval-sum1 e))
        ((define-exp? e) (eval-define1 e))
        ((symbol? e) (define-eval1 (lookup e)))
        (else (error "Undefined Symbol for the define-eval -- " e))))

;; ;; Test
;; (print-out (define-eval1 '(define* x* (plus* 1 2))))
;; (print-out (define-eval1 '(define* y* (plus* x* 3))))
;; (print-out (define-eval1 '(plus* y* 3)))
;; (exit)

;;;; store by value ;;;;
;; this define-eval will bind only the number to the variable.
;; define-sum-eval
(define (define-eval-sum2 e)
;;  (display "eva-sum2:") (print-out e)
  (+ (define-eval2 (cadr e))
     (define-eval2 (caddr e))))

;; eval-define2
(define (eval-define2 e)
  (let ((key (cadr e))
        (value (define-eval2 (caddr e))))
    (ask environment 'store key value)
    value))

;; define-eval
(define (define-eval2 e)
;;  (display "eva2:") (print-out e)
  (cond ((number? e) e)
        ((sum-exp? e) (define-eval-sum2 e))
        ((define-exp? e) (eval-define2 e))
        ((symbol? e) (lookup e))
        (else (error "Undefined Symbol for the define-eval -- " e))))

;; ;; Test
;; (print-out (define-eval2 '(define* x* (plus* 1 2))))
;; (print-out (define-eval2 '(define* y* (plus* x* 3))))
;; (print-out (define-eval2 '(plus* y* 3)))
;; (exit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the condition and the boolean evaluation -- condition-eval
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The environment
(define environment (make-frame))

;; wraper for the environment
(define (lookup key)
  (ask environment 'lookup key))

;; define check
(define (define-exp? e)
  (tag-recognizer e 'define*))

;; greater check
(define (greater-exp? e)
  (tag-recognizer e 'greater*))

;; if check
(define (if-exp? e)
  (tag-recognizer e 'if*))

;; condition-sum-eva
(define (condition-eval-sum e)
  (+ (condition-eval (cadr e))
     (condition-eval (caddr e))))

;; condition-eval-define
(define (condition-eval-define e)
  (let ((key (cadr e))
        (value (caddr e)))
    (ask environment 'store key value)
    (condition-eval value)))

;; condition-eval-greater
(define (condition-eval-greater e)
  (let ((first (condition-eval (cadr e)))
        (second (condition-eval (caddr e))))
    (> first second)))

;; condition-eval-if
(define (condition-eval-if e)
  ;; (display "eval-if:") (print-out e)
  (condition-eval
   (if (condition-eval (cadr e))
       (caddr e)
       (cadddr e))))

;; condition-eval
(define (condition-eval e)
  ;; (display "condition-eval:") (print-out e)
  (cond ((number? e) e)
        ((boolean? e) e)
        ((sum-exp? e) (condition-eval-sum e))
        ((greater-exp? e) (condition-eval-greater e))
        ((if-exp? e) (condition-eval-if e))
        ((define-exp? e) (condition-eval-define e))
        ((symbol? e) (condition-eval (lookup e)))
        (else (error "Undefined Symbol for the define-eval -- " e))))

;; ;; Test
;; (print-out (condition-eval '(define* x* (plus* 1 2))))
;; (print-out (condition-eval '(define* y* (plus* x* 3))))
;; (print-out (condition-eval '(plus* y* 3)))
;; (print-out (condition-eval '(if* (greater* (plus* 1 1) x*) 5 y*)))
;; (exit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; procedure-eval adding the standard procedure in the interpreter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The environment
(define environment (make-frame))

;; wraper for the environment
(define (lookup key)
  (ask environment 'lookup key))

;; define check
(define (define-exp? e)
  (tag-recognizer e 'define*))

;; if check
(define (if-exp? e)
  (tag-recognizer e 'if*))

;; procedure check
(define (procedure-exp? e)
  (list? e))

;; procedure-eval-define
(define (procedure-eval-define e)
  (let ((key (cadr e))
        (value (caddr e)))
    (ask environment 'store key value)
    (procedure-eval value)))

;; procedure-eval-define
(define (procedure-eval-if e)
  (cond ((procedure-eval (cadr e))
         (procedure-eval (caddr e)))
        (else (procedure-eval (cadddr e)))))

;; put the procedure into the frame
(ask environment 'store '+ +)
(ask environment 'store '- -)
(ask environment 'store '* *)
(ask environment 'store '/ /)
(ask environment 'store '< <)
(ask environment 'store '= =)
(ask environment 'store '> >)

;; procedure-eval-procedure
(define (procedure-eval-procedure e)
;;  (display "pep:") (print-out e)
  (let ((p (lookup (car e))))
    (apply p (map procedure-eval (cdr e)))))

;; procedure-eval
(define (procedure-eval e)
  (cond ((number? e) e)
        ((boolean? e) e)
        ((symbol? e) (procedure-eval (lookup e)))
        ((define-exp? e) (procedure-eval-define e))
        ((if-exp? e) (procedure-eval-if e))
        ((procedure-exp? e) (procedure-eval (procedure-eval-procedure e)))
        (else (error "Undefined Symbol for the procedure-eval -- " e))))

;; ;; Test
;; (print-out (procedure-eval '(define* x* (+ 1 2 3))))
;; (print-out (procedure-eval '(if* (< 1 2) x* 10)))
;; (exit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; procedure-define-eval: to implement the whole package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The global-environment
(define global-environment (make-frame))

;; wraper for the environment
(define (lookup key env)
  (ask env 'lookup key))

;; define check
(define (define-exp? e)
  (tag-recognizer e 'define*))

;; if check
(define (if-exp? e)
  (tag-recognizer e 'if*))

;; procedure check
(define (procedure-exp? e)
  (list? e))

;; lambda check
(define (lambda-exp? e)
  (tag-recognizer e 'lambda*))

;; procedure-define-eval-define
(define (procedure-define-eval-define e env)
;;  (display "pdef:") (print-out e)
  (let ((key (cadr e))
        (value (caddr e)))
    (ask env 'store key value)
    (procedure-define-eval value env)))

;; procedure-define-eval-define
(define (procedure-define-eval-if e env)
  (cond ((procedure-define-eval (cadr e) env)
         (procedure-define-eval (caddr e) env))
        (else (procedure-define-eval (cadddr e) env))))

;; put the procedure into the frame
(ask global-environment 'store '+ +)
(ask global-environment 'store '- -)
(ask global-environment 'store '* *)
(ask global-environment 'store '/ /)
(ask global-environment 'store '< <)
(ask global-environment 'store '= =)
(ask global-environment 'store '> >)

;; procedure-define-eval-procedure
(define (procedure-define-eval-procedure e env)
;;  (display "pdep:") (print-out e)
  (let ((procedure-name (car e)))
  ;;  (print-out procedure-name)
    (let ((p (if (lambda-exp? procedure-name)
                 procedure-name
                 (lookup procedure-name env)))
          (input-list (map (lambda (e) (procedure-define-eval e env))
                           (cdr e))))
      (cond ((procedure? p)
             (apply p input-list))
            ((lambda-exp? p)
             (let ((p-env (make-frame (if (lambda-exp? procedure-name)
                                          env
                                          (ask env 'locateframe procedure-name))))
                   (variable-list (cadr p))
                   (procedure-list (cddr p)))
               ;; Check the input length
               (if (not (= (length variable-list)
                           (length input-list)))
                   (error "procedure length not fit -- " variable-list input-list))
               ;; getting the input named
               (let name ((rest-v variable-list)
                          (rest-i input-list))
                 (cond ((null? rest-v)
                        #t)
                       (else
                        (ask p-env 'store (car rest-v) (car rest-i))
                        (name (cdr rest-v) (cdr rest-i)))))
               ;; evaluation the lines
               (let eval ((last-value (list))
                          (rest-p procedure-list))
                 (cond ((null? rest-p)
                        last-value)
                       (else
                        (eval (procedure-define-eval (car rest-p) p-env) (cdr rest-p)))))))
            (else
             (error "Undefined Procedure -- " p))))))

;; procedure-define-eval
(define (procedure-define-eval e env)
;;  (display "pde:") (print-out e)
  (cond ((number? e) e)
        ((symbol? e) (procedure-define-eval (lookup e env) env))
        ((lambda-exp? e) e)
        ((define-exp? e) (procedure-define-eval-define e env))
        ((if-exp? e) (procedure-define-eval-if e env))
        ((procedure-exp? e) (procedure-define-eval (procedure-define-eval-procedure e env) env))
        ((boolean? e) e)
        ((null? e) e)
        (else (error "Undefined Symbol for the procedure-define-eval -- " e))))

(define (pde e) (procedure-define-eval e global-environment))
;; Test
;; (print-out (pde '(define* x* (+ 1 2 3))))
;; (print-out (pde '(if* (< 1 2) x* 10)))
;; (print-out (pde '((lambda* (x*) (+ x* 1)) 1)))
;; (print-out (pde '(define* plus* (lambda* (x* y*) (+ x* y*)))))
;; (print-out (pde '(plus* 3 5)))
;; (print-out (pde '(define* x* 1)))
;; (print-out (pde '(define* test1* (lambda* () x*))))
;; (print-out (pde '(define* test2* (lambda* () (define* x* 2) (+ x* (test1*))))))
;; (print-out (pde '(test2*)))
;; (exit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; procedure-define-eval: modified, more elegent with the anonymous function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The global-environment
(define global-environment (make-frame))

;; wraper for the environment
(define (lookup key env)
;;  (display "lookup:") (print-out key) (print-out (ask env 'bindings))
  (ask env 'lookup key))

;; environment check
(define (environment? env)
  (eq? 'frame-class
       (ask env 'class-name)))

;; define check
(define (define-exp? e)
  (tag-recognizer e 'define*))

;; if check
(define (if-exp? e)
  (tag-recognizer e 'if*))

;; procedure check
(define (procedure-exp? e)
  (list? e))

;; lambda check
(define (lambda-code-exp? e)
  (tag-recognizer e 'lambda*))

(define (lambda-function-exp? e)
  (and (pair? e)
       (lambda-code-exp? (car e))
       (environment? (cdr e))))

;; procedure-define-eval-define
(define (procedure-define-eval-define e env)
;;  (display "pdef:") (print-out e)
  (let ((key (cadr e))
        (value (caddr e)))
    (cond ((lambda-code-exp? value)
           ;; (print-out (ask env 'bindings))
           (ask env 'store key (cons value env)))
          (else (ask env 'store key value)))
    (procedure-define-eval value env)))

;; procedure-define-eval-define
(define (procedure-define-eval-if e env)
  (cond ((procedure-define-eval (cadr e) env)
         (procedure-define-eval (caddr e) env))
        (else (procedure-define-eval (cadddr e) env))))

;; put the procedure into the frame
(ask global-environment 'store '+ +)
(ask global-environment 'store '- -)
(ask global-environment 'store '* *)
(ask global-environment 'store '/ /)
(ask global-environment 'store '< <)
(ask global-environment 'store '= =)
(ask global-environment 'store '> >)

;; procedure-define-eval-procedure
(define (procedure-define-eval-procedure e env)
;;  (display "pdep:") (print-out e)
  (let ((procedure-name (car e)))
  ;;  (print-out procedure-name)
    ;; set the procedure
    (let ((p (if (lambda-code-exp? procedure-name)
                 procedure-name
                 (lookup procedure-name env)))
          (input-list (map (lambda (e) (procedure-define-eval e env))
                           (cdr e))))
      ;; if it is a basic procedure in th scheme, evaluate it.
      (cond ((procedure? p)
             (apply p input-list))
            ;; if it is the anonymous function, use the eval-lambda with the current environment.
            ((lambda-code-exp? p)
             (procedure-define-eval-lambda p input-list env))
            ;; if it is the defined function, use the eval-lambda with the defined environment.
            ((lambda-function-exp? p)
             (procedure-define-eval-lambda (car p) input-list (cdr p)))
            (else
             (error "Undefined Procedure -- " p))))))

(define (procedure-define-eval-lambda p input-list env)
;;  (display "pdel:") (print-out p)
  (let ((p-env (make-frame env))
        (variable-list (cadr p))
        (procedure-list (cddr p)))
    ;; Check the input length
    (if (not (= (length variable-list)
                (length input-list)))
        (error "procedure length not fit -- " variable-list input-list))
    ;; getting the input named
    (let name ((rest-v variable-list)
               (rest-i input-list))
      (cond ((null? rest-v)
             #t)
            (else
             (ask p-env 'store (car rest-v) (car rest-i))
             (name (cdr rest-v) (cdr rest-i)))))
    ;; evaluation the lines
    (let eval ((last-value (list))
               (rest-p procedure-list))
      (cond ((null? rest-p)
             last-value)
            (else
             (eval (procedure-define-eval (car rest-p) p-env) (cdr rest-p)))))))

;; procedure-define-eval
(define (procedure-define-eval e env)
;;  (display "pde:") (print-out e)
  (cond ((number? e) e)
        ((symbol? e) (procedure-define-eval (lookup e env) env))
        ((lambda-code-exp? e) e)
        ((lambda-function-exp? e) (car e))
        ((define-exp? e) (procedure-define-eval-define e env))
        ((if-exp? e) (procedure-define-eval-if e env))
        ((procedure-exp? e) (procedure-define-eval (procedure-define-eval-procedure e env) env))
        ((boolean? e) e)
        ((null? e) e)
        (else (error "Undefined Symbol for the procedure-define-eval -- " e))))

(define (pde e) (procedure-define-eval e global-environment))

;; ;; Test
;; (print-out (pde '(define* x* (+ 1 2 3))))
;; (print-out (pde '(if* (< 1 2) x* 10)))
;; (print-out (pde '((lambda* (x*) (+ x* 1)) 1)))
;; (print-out (pde '(define* plus* (lambda* (x* y*) (+ x* y*)))))
;; (print-out (pde '(plus* 3 5)))
;; (print-out (pde '(define* x* 1)))
;; (print-out (pde '(define* test1* (lambda* () x*))))
;; (print-out (pde '(define* test2* (lambda* () (define* x* 2) (+ x* (test1*))))))
;; (print-out (pde '(test2*)))
;; (exit)
