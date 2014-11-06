(define (loadlec20) (load "../LectureImplementation/lec20"))
(load "../BookExercise/basic")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A new interpreter for the scheme language
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; first we start simple, we will only provide the lexical scoping with the following behavior:
;; 1. self-evaluation: numbers, build-in procedures, defined(compound) procedure, booleans,
;; 2. define special forms
;; 3. variable checking
;; 4. lambda evaluation
;; 5. set
;; 6. if
;; 7. application

;;----------------------------------------
;; building up the environment
;;----------------------------------------
;; This time I will simply use a tagged data to represent a environment

;; tag-data part
(define (tag? data)
  (and (list? data)
       (not (null? data))))

(define (tag-name? name)
  (lambda (data)
    (and (tag? data)
         (eq? name (car data)))))

(define env? (tag-name? 'environment))

(define (environment-init enclosed-env)
  (list 'environment (list) enclosed-env))

(define environment-pairs cadr)
(define environment-eep caddr)

(define environment? (tag-name? 'environment))

(define (environment-add env name value)
  (cond ((environment? env)
         (set! (environment-pairs env)
               (cons (cons name value)
                     (environment-pairs env))))
        (else (error "Input is not an environment! -- " env))))

(define (environment-internal-pair-search p name)
  (cond ((null? p) #f)
        ((eq? (caar p) name) (car p))
        (else (environment-internal-pair-search (cdr p) name))))

(define (environment-search env name)
  (if (environment? env)
      (let ((named-pair (environment-internal-pair-search
                        (environment-pairs env)
                        name)))
        (cond (named-pair
               (cdr named-pair))
              ((environment-eep env)
               (environment-search (environment-eep env) name))
              (else (error "Undefined variable name -- " name))))))

(define (environment-internal-pair-change p name value)
  (cond ((null? p) #f)
        ((eq? (caar p) name)
         (set-cdr! (car p) value)
         #t)
        (else
         (environment-internal-pair-change (cdr p) name value))))

(define (environment-change env name value)
  (if (environment? env)
      (cond ((environment-internal-pair-change
              (environment-pairs env) name value)
             #t)
            ((environment-eep env)
             (environment-change (environment-eep env) name value))
            (else #f))))

(define (make-global-environmet) (environment-init #f))

;; ;; test
;; (define genv (make-global-environmet))
;; (environment-add genv 'a 1)
;; (environment-add genv 'b 2)
;; (define menv (environment-init genv))
;; (environment-add menv 'a 3)
;; (environment-add menv 'c 4)
;; (print-out (environment-search genv 'a))  1
;; (print-out (environment-search genv 'b))  2
;; (print-out (environment-search menv 'a))  3
;; (print-out (environment-search menv 'b))  2
;; (print-out (environment-search menv 'c))  4
;; (print-out (environment-change menv 'a 5))
;; (print-out (environment-change menv 'b 6))
;; (print-out (environment-change menv 'c 7))
;; (print-out (environment-change menv 'd 8))
;; (print-out (environment-search genv 'a))  1
;; (print-out (environment-search genv 'b))  6
;; (print-out (environment-search menv 'a))  5
;; (print-out (environment-search menv 'b))  6
;; (print-out (environment-search menv 'c))  7
;; (exit)

;;----------------------------------------
;; The check list for the interpretor
;;----------------------------------------

;; self-evaluation
(define (self-evaluated? exp)
  (or (number? exp)
      (procedure? exp)
      (lambda-code? exp)
      (boolean? exp)))

;;--------------------
;; define special form
(define define? (tag-name? 'define))
(define define-name cadr)
(define define-value caddr)

;; will return the evaluation value
(define (define-eval exp env)
  (let ((name (define-name exp))
        (value (define-value exp)))
    ;; add the value to the environment
    (environment-add env name (eval value env))
    ;; return the value evaluation to the upper function
    (eval value env)))

;;--------------------
;; variable
(define variable? symbol?)
(define (variable-eval var env)
  (environment-search env var))

;;--------------------
;; lambda evaluation
;; there are two kinds of the lambda objects: one that is the raw text from the input, the other is the code that stored in the environment.
(define lambda? (tag-name? 'lambda))
(define lambda-arg cadr)
(define lambda-body cddr)
(define (lambda-eval exp env)
  (list 'lambda-code env (lambda-arg exp) (lambda-body exp)))

(define lambda-code? (tag-name? 'lambda-code))
(define lambda-code-env cadr)
(define lambda-code-arg caddr)
(define lambda-code-body cadddr)
;; aviod the circular display
(define (lambda-code-display lc)
  (list 'lambda
        (lambda-code-arg lc)
        (lambda-code-body lc)))
;;--------------------
;; set special form
(define set? (tag-name? 'set!))
(define set-name cadr)
(define set-value caddr)

;; will reture the evaluation of the value
(define (set-eval exp env)
  (let ((name (set-name exp))
        (value (set-value exp)))
    ;; if it is a lambda code or a application, evaluate it.
    (if (or (lambda-code? value)
            (application? value))
        (set! value (eval value env)))

    (if (not (environment-change env name value))
        (environment-add env name value))
    (eval value env)))

;;--------------------
;; if special form
(define if? (tag-name? 'if))
(define if-premise cadr)
(define if-true caddr)
(define if-false cadddr)

(define (if-eval exp env)
  (let ((premise (if-premise exp))
        (true (if-true exp))
        (false (if-false exp)))
    (if (eval premise env)
        (eval true env)
        (eval false env))))

;;--------------------
;; application
(define (application? exp)
  (and (list? exp)
       (not (null? exp))
       (variable? (car exp))))

(define application-procedure car)
(define application-arg cdr)

(define (application-eval exp env)
  (let ((pro (eval (application-procedure exp) env))
        ;; evaluating the procedure first
        (arg (application-arg exp)))
    (cond ((procedure? pro)
           (apply pro
                  (map (lambda (a) (eval a env)) arg)))
          ((lambda-code? pro)
           (let ((lambda-env (lambda-code-env pro))
                 (def-arg (lambda-code-arg pro))
                 (body (lambda-code-body pro)))
             ;; check the input list is equals to defined input
             (cond ((not (= (length arg)
                            (length def-arg)))
                    (error (list 'procedure 'need (length def-arg) 'inputs (length arg) 'inputted 'instead) arg))
                   ;; do the evaluation
                   (else
                    (let ((eval-arg (map (lambda (a) (eval a env))
                                         arg))
                          ;; using the dynamic scoping
                          ;; (new-env (environment-init env))
                          ;; using the lexical scoping instead of the dynamic scoping
                          (new-env (environment-init lambda-env))
                          )

                      ;; add the names and values into the new environment
                      (do ((ea eval-arg (cdr ea))
                           (da def-arg (cdr da)))
                          ((null? ea))
                        (environment-add new-env (car da) (car ea)))

                      ;; evaluate according to the body
                      (do ((remain (cdr body) (cdr remain))
                           (this (car body) (car remain)))
                          ((null? remain)
                           (eval this new-env))
                        (eval this new-env)))))))
          (else
           (error "Undefined application -- " pro)))))

;;----------------------------------------
;; evaluator
;;----------------------------------------

(define (eval exp env)
  (cond ((self-evaluated? exp) exp)
        ((define? exp) (define-eval exp env))
        ((variable? exp) (variable-eval exp env))
        ((lambda? exp) (lambda-eval exp env))
        ((set? exp) (set-eval exp env))
        ((if? exp) (if-eval exp env))
        ((application? exp) (application-eval exp env))
        (else (error "Undefined expression -- " exp))))


;; define the global environment
(define genv (make-global-environmet))
;; add some premitives
(environment-add genv '- -)
(environment-add genv '+ +)
(environment-add genv '* *)
(environment-add genv '/ /)
(environment-add genv '= =)
(environment-add genv '> >)
(environment-add genv '< <)

;; wrap the eval
(define (env-check-print exp)
  (define (write-prefix distance)
    (do ((start distance (- start 1)))
        ((= start 0) (write ""))
      (write "")))
  (let outsideiter ((start exp)
                    (distance 3))
    (cond ((env? start)
           (write '(env)))
          ((list? start)
           (write "(")
           (let insideiter ((l start)
                            (thefirst #t))
             (cond ((null? l)
                    ;;(write "")
                    )
                   ((env? (car l))
                    (write '(env))
                    (insideiter (cdr l) #f))
                   ((list? (car l))
                    (cond ((not thefirst)
                           (write "")
                           (newline)
                           (write-prefix distance)))
                    (outsideiter (car l) (+ 1 distance))
                    (cond ((not (null? (cdr l)))
                           (newline)
                           (write-prefix (+ 1 distance))
                           (insideiter (cdr l) #f))))
                   (else
                    (write (car l))
                    (write "")
                    (insideiter (cdr l) #f))))
           (write ")"))
          (else
           (write start)
           (write ""))))
  (newline))

(define gevalnum 1)
(define (geval exp)
  (let ((r (eval exp genv)))
    (write gevalnum)
    (write ">>>")
    (set! gevalnum (+ 1 gevalnum))
    (env-check-print r)))

;; ;; Test
;; ;; self evaluation
;; (geval '1)
;; (geval '+)
;; (geval #f)

;; ;; define and variable
;; (geval '(define a 1))
;; (geval 'a)

;; ;; define and lambda and application
;; (geval '(define twice (lambda (x) (* 2 x))))
;; (geval 'twice)
;; (geval '(twice 2))

;; ;; set and define
;; (geval '(define 'a 1))
;; (geval '(set! a 2))
;; (geval 'a)

;; ;; recursion evaluation
;; (geval '(define a 1))
;; (geval '(set! a (+ a 1)))

;; ;; if
;; (geval '(if (> 2 1) 2 1))

;; ;; lexical scoping
;; (geval '(define a 1))
;; (geval '(define t1 (lambda () a)))
;; (geval '(define t2 (lambda (a) (t1))))
;; (geval '(t2 2)) ;; 1


;; ;; CBV
;; (geval '(define r (lambda () (r))))
;; (geval '(define t (lambda (x y) (if (= x 0) 0 y))))
;; (geval '(t 0 (r))) ;; run forever
;; (exit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some Syntactic Sugar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; In this part, I am going to implement some of the following syntactic sugar:
;; 1. define abbreviate: in the define
;; 2. cond: in the eval
;; 3. begin: in the eval
;; 4. let: in the application


;;--------------------
;; define abbreviate way
(define (define-abbre? exp)
  (list? (define-name exp)))

(define (define-abbre->normal exp)
  (let ((name (cadr exp))
        (body (cddr exp)))
    (let ((function-name (car name))
          (function-arg (cdr name)))
      (list 'define function-name
            (append (list 'lambda function-arg)
                    body)))))
;; Test
;; (print-out (define-abbre->normal '(define (me x y) (+ 1 x) (+ 3 y))))

;; rewrite the define-eval
(define (define-eval exp env)
  (if (define-abbre? exp)
      (set! exp (define-abbre->normal exp)))
  (let ((name (define-name exp))
        (value (define-value exp)))
    ;; add the value to the environment
    (environment-add env name (eval value env))
    ;; return the value evaluation to the upper function
    (eval value env)))

;; Test
;; (geval '(define (t x) (+ x 1)))

;;--------------------
;; cond
(define cond? (tag-name? 'cond))

(define cond-body cdr)
(define cond-body-first-pre caar)
(define cond-body-first-bod cdar)
(define cond-body-left cdr)

(define (cond-eval exp env)
  (let ((body (cond-body exp)))
    ;; find the right command
    (do ((left-body (cond-body-left body) (cond-body-left left-body))
         (pre (cond-body-first-pre body) (cond-body-first-pre left-body))
         (body-command (cond-body-first-bod body) (cond-body-first-bod left-body)))
        ((or (eq? 'else pre)
             (eval pre env)
             (null? left-body))
         (cond ((or (eq? 'else pre)
                    (eval pre env))
                ;; carry out the evaluation
                (do ((left (cdr body-command) (cdr left))
                     (this (car body-command) (car left)))
                    ((null? left) (eval this env))
                  (eval this env)))
               ((null? left-body) 'value-not-defined)
               (else (error "unknown error -- " pre left-body))))
      )))

;; rewrite the eval
(define (eval exp env)
  (cond ((self-evaluated? exp) exp)
        ((define? exp) (define-eval exp env))
        ((variable? exp) (variable-eval exp env))
        ((lambda? exp) (lambda-eval exp env))
        ((set? exp) (set-eval exp env))
        ((if? exp) (if-eval exp env))
        ((cond? exp) (cond-eval exp env))
        ((application? exp) (application-eval exp env))
        (else (error "Undefined expression -- " exp))))

;; ;; test
;; (geval '(= 2 1))
;; (geval '(cond ((= 2 1) 1)
;;               ((< 2 1) 2)
;;               ((> 1 2) 3)))
;; (exit)
;;--------------------
;; begin
(define begin? (tag-name? 'begin))

(define begin-body cdr)
(define begin-body-first car)
(define begin-body-left cdr)

(define (begin-eval exp env)
  (let ((body (begin-body exp)))
    (do ((remain (begin-body-left body) (begin-body-left remain))
         (this (begin-body-first body) (begin-body-first remain)))
        ((null? remain) (eval this env))
      (eval this env))))

;; rewrite the eval
(define (eval exp env)
  (cond ((self-evaluated? exp) exp)
        ((define? exp) (define-eval exp env))
        ((variable? exp) (variable-eval exp env))
        ((lambda? exp) (lambda-eval exp env))
        ((set? exp) (set-eval exp env))
        ((if? exp) (if-eval exp env))
        ((cond? exp) (cond-eval exp env))
        ((begin? exp) (begin-eval exp env))
        ((application? exp) (application-eval exp env))
        (else (error "Undefined expression -- " exp))))


;; ;; test
;; (geval '(begin (define a 1)
;;                (set! a 2)
;;                a))
;; (exit)

;;--------------------
;; let
(define let? (tag-name? 'let))

(define let-arg cadr)
(define let-body cddr)
(define let-arg-first-name caar)
(define let-arg-first-value cadar)
(define let-arg-left cdr)
(define let-body-first car)
(define let-body-left cdr)

(define (let-eval exp env)
  (let ((args (let-arg exp))
        (body (let-body exp))
        (new-env (environment-init env)))

    ;; add the args into the new environment
    (do ((left-args args (let-arg-left left-args)))
        ((null? left-args))
      (environment-add new-env
                       (let-arg-first-name left-args)
                       (eval (let-arg-first-value left-args) env))
      (print-out left-args))

    ;; begin the evaluation
    (do ((left-body (let-body-left body) (let-body-left left-body))
         (this (let-body-first body) (let-body-first left-body)))
        ((null? left-body) (eval this new-env))
      (eval this new-env)
      )))

;; rewrite the eval
(define (eval exp env)
  (cond ((self-evaluated? exp) exp)
        ((define? exp) (define-eval exp env))
        ((variable? exp) (variable-eval exp env))
        ((lambda? exp) (lambda-eval exp env))
        ((set? exp) (set-eval exp env))
        ((if? exp) (if-eval exp env))
        ((cond? exp) (cond-eval exp env))
        ((begin? exp) (begin-eval exp env))
        ((let? exp) (let-eval exp env))
        ((application? exp) (application-eval exp env))
        (else (error "Undefined expression -- " exp))))

;; ;; test
;; (geval '(let ((a 1)
;;               (b 2))
;;           (set! a (+ 1 a))
;;           (+ a b)))
;; (exit)

;; ;; dynamic scoping
;; (geval '(define (a x) (+ x y)))
;; (geval '(define (b y) (a y)))
;; (geval '(b 3))
;; (exit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Try another way of the application call
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use the grammer like: (call fun arg)

;;--------------------
;; rewrite the application
;; (define application? (tag-name? 'call))

;; (define application-procedure cadr)
;; (define application-arg cddr)

;; test
;; (geval '(call + 1 1))
