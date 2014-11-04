(load "../BookExercise/basic")
(define (loadimp41) (load "../BookImplementation/sec4.1"))
;; search key word
;; args-eval-order: adjust the order of evaluation of the arguments in a application evaluation, default left to right
;; CBN-or-CBV: adjust the call value protocol, default CBV
;; lexical-or-dynamic-coping: adjust the scoping rules, default lexical
;; internal-represent-register: to register the internal representation

(define debug #f)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; to rewrite the evaluator again
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I will imlpement a interpretor using the following qualities:(sorry,not enough time for R6RS, but I will do it someday!)
;; this evalator is an environment based and CBV evaluator and has the following features:
;; 1. self-evaluate atoms: numbers, booleans, procedures, lambda code(as the list of input(could be non-fixed length input) and defined environment and expressions to execute) (return its value directlf)
;; 2. string atoms: the string data as a list of symbols start with the tag 'string-tag (return the symbols list)
;; 3. define clause: get the bindings of the name and the value into the environment
;; 4. variable atoms: the defined names in the environment
;; 5. set clause: to reset the name with a different value
;; 6. if clause: select premise and results accordingly
;; 7. begin clause: to evaluate the proceding expressions and return the last one as value
;; 8. lamdba clause: to define a function (inside expression could use the begins)
;; 9. cond clause: to use a separate if clauses (syntax sugar)
;; 10. let clause: use the lambda

;;------------------------------
;; tag data
;;------------------------------
;; tag data to recognize the object
(define (tag? t)
  (and (list? t)
       (not (null? t))
       (symbol? (car t))))

;; generate a lambda function for recognizing the tagged-data with name n
(define (tag-named n)
  (if (symbol? n)
      (lambda (d) (and (tag? d)
                       (eq? n (car d))))
      (error "input not a tag name" n)))

(define tag-data cdr)

(define (tag-gen name . data)
  (if (and (symbol? name)
           (list? data))
      (cons name data)
      (error "input not name and data" name data)))

(define (tag-add name)
  (lambda (d)
    (cons name d)))
;;------------------------------
;; the environment object
;;------------------------------
;; for every level of evaluation, the environment object could be look up using (env-look-up env)

;; the data structure of the env is ('env (parent-env) (bindings))

;; env?
(define env? (tag-named 'env))

;; operator for env
(define env-parent-null? null?)

;; the empty environment
(define env-empty-env (list))
(define env-empty-names (list))
(define env-empty-values (list))

;; generate an environment
(define (env-gen parent-env names values)
  (tag-gen 'env parent-env (doublemap (lambda (n v) (bindings-pair n v)) names values)))

;; generate an empty global environment
(define (env-gen-empty-global-env)
  (env-gen env-empty-env env-empty-names env-empty-values))

;; generate a global environment
(define (env-gen-global-env names values)
  (env-gen env-empty-env names values))

;; generate an empty environment
(define (env-gen-empty-env parent-env)
  (env-gen parent-env env-empty-names env-empty-values))

;; selector for the env
(define env-parent-env cadr)
(define env-frame caddr)

;; constructor for the bindings-pair
(define bindings-pair cons)
(define bindings-add cons)
(define bindings-append append)

;; env-new-bindings
(define (env-set-new-bindings env new-bindings)
  (set-car! (cddr env) new-bindings))

;; add a binding into the environment
(define (env-add env name data)
  (if (symbol? name)
      (env-set-new-bindings env (bindings-add (bindings-pair name data)
                                              (env-frame env)))
      (error "name is not a symbol" name)))

(define (env-adds env names values)
  (if (= (length names) (length values))
      (env-set-new-bindings env (bindings-append (doublemap (lambda (n v) (bindings-pair n v))
                                                            names
                                                            values)
                                                 (env-frame env)))
      (error "Not equal length of names and values!" names values)))

;; bindings selector
(define bindings-next cdr)
(define bindings-first-pair car)
;; binding-pair selector
(define bindings-pair-name car)
(define bindings-pair-value cdr)

;; internal search for a value, will return the desired data pair or a symbol of 'name-not-found
(define (env-internal-search env name)
  (if (symbol? name)
      (let ((parent-env (env-parent-env env))
            (bindings (env-frame env)))
        (let search ((rest (bindings-next bindings))
                     (first-pair (bindings-first-pair bindings)))
          (cond ((eq? name (bindings-pair-name first-pair))
                 first-pair)
                ((not (null? rest))
                 (search (bindings-next rest)
                         (bindings-first-pair rest)))
                ((env-parent-null? parent-env)
                 'name-not-found)
                (else
                 (env-internal-search parent-env name)))))
      (error "name is not a symbol" name)))

;; search the name for value
(define (env-search env name)
  (let ((result (env-internal-search env name)))
    (cond ((eq? result 'name-not-found)
           (error "name not exist -- " name))
          ((pair? result)
           (let ((value (bindings-pair-value result)))
             (if (eq? value '*unassigned*)
                 (error "name's value unassigned -- " name)
                 value)))
          (else
           (error "return value error -- " name result)))))

;; search the name for existence or not
(define (env-exist? env name)
  (not (eq? 'name-not-found (env-internal-search env name))))

;; change the name
(define (env-change env name value)
  (set-cdr! (env-internal-search env name) value))

;; ;; test
;; (define e1 (env-gen-empty-global-env))
;; (print e1)
;; (define e1 (env-gen-global-env '(t) '(1)))
;; (print e1)
;; (print (env-frame e1))
;; (env-add e1 'a 1)
;; (print e1)
;; (env-add e1 'b 2)
;; (print e1)
;; (print (env-search e1 'a))
;; (print (env-search e1 'b))
;; (env-change e1 'a 3)
;; (print (env-search e1 'a))
;; (env-change e1 'b 4)
;; (print (env-search e1 'b))
;; ;; (print (env-search e1 'x))
;; (define e2 (env-gen-empty-env e1))
;; (print e2)
;; (print (env-frame e2))
;; (env-add e2 'a 2)
;; (print e2)
;; (env-add e2 'b 1)
;; (print e2)
;; (print (env-search e2 'a))
;; (print (env-search e2 'b))
;; ;;(print (env-search e2 'x))
;; (env-change e1 'a 4)
;; (print (env-search e1 'a))
;; (env-change e1 'b 3)
;; (print (env-search e1 'b))
;; (env-adds e2 '(c d) '(*unassigned* *unassigned*))
;; (env-add e1 'e '*unassigned*)
;; (print (env-exist? e2 'c))
;; (print (env-exist? e2 'e))
;; (print (env-exist? e2 'x))
;; ;; (print e2)
;; (env-search e2 'e)
;; (exit)

;;------------------------------
;; internal-represent-register
(define (internal-represent? exp)
  (or (eq? exp '*unassigned*)))

;;------------------------------
;; self evaluator
(define (self-evaluator? v)
  (or (number? v)
      (boolean? v)
      (procedure? v)
      (lambda-code? v)
      ))

(define (self-eval exp env) exp)
;;------------------------------
;; string
(define string? (tag-named 'string))
(define (string-eval exp env) (tag-data exp))

;;------------------------------
;; define claus
;; selector
(define define-name cadr)
(define define-value caddr)

(define define-lambda? list?)
(define define-lambda-name caadr)
(define define-lambda-arg cdadr)
(define define-lambda-body cddr)

(define define? (tag-named 'define))
(define define-gen (tag-add 'define))

(define (define-eval exp env)
  (let ((name (define-name exp))
        (value (define-value exp)))
    ;; add the syntax sugar for the define
    (cond ((lambda? value)
           (set! value (eval value env)))
          ((define-lambda? name)
           (set! name (define-lambda-name exp))
           (set! value (lambda-code-gen env
                                        (define-lambda-arg exp)
                                        (define-lambda-body exp)))))
    (env-add env name value)
    (eval-value value env)))

;;------------------------------
;; variable
(define variable? symbol?)
(define (variable-eval exp env)
  (env-search env exp))

;;------------------------------
;; set
(define set? (tag-named 'set))
;; selector
(define set-name cadr)
(define set-value caddr)
(define set-gen (tag-add 'set))

(define (set-eval exp env)
  (let ((name (set-name exp))
        (value (set-value exp)))
    (if (env-exist? env name)
        (env-change env name value)
        (env-add env name value))
    (eval value env)))
;;------------------------------
;; if clause
(define if? (tag-named 'if))
;; selector
(define if-pre cadr)
(define if-true caddr)
(define if-false cadddr)

(define (if-eval exp env)
  (let ((pre (if-pre exp))
        (true (if-true exp))
        (false (if-false exp)))
    ;; (print exp)
    ;; (print pre)
    ;; (print true)
    ;; (print false)
    (if (eval pre env)
        (eval true env)
        (eval false env))))

(define if-gen (tag-add 'if))
;;------------------------------
;; begin clause
(define begin? (tag-named 'begin))
;; selector
(define begin-body cdr)
(define begin-body-first car)
(define begin-body-remove-first cdr)
(define begin-body-empty? null?)

;; begin-eval
(define (begin-eval exp env)
  (let ((body (begin-body exp)))
    (do ((rest (begin-body-remove-first body) (begin-body-remove-first rest))
         (first (begin-body-first body) (begin-body-first rest)))
        ((null? rest) (eval first env))
      (eval first env))))

(define begin-gen (tag-add 'begin))
;;------------------------------
;; lambda clause
;; lambda code object store in the env
;; the data structure of the lambda code: ('lambda-code env arg body)
;; generator
(define (lambda-code-gen env arg body)
  (tag-gen 'lambda-code env arg body))
;; selector
(define lambda-code-arg caddr)
(define lambda-code-body cadddr)
(define lambda-code-env cadr)
;; operator
(define lambda-code? (tag-named 'lambda-code))

;; lambda code print
(define (lambda-code-print r)
  (print (list (lambda-code-arg r)
               (lambda-code-body r))))

;; put the old env in the list just for adjustment
(define (lambda-code-apply lambda-code arg old-env)
  (let ((lambda-arg (lambda-code-arg lambda-code))
        (lambda-body (lambda-code-body lambda-code))
        (lambda-env (lambda-code-env lambda-code)))
    ;;(print "enter lambda-code-apply")
    ;;(print arg)
    (let ( ;;lexical-or-dynamic-coping
          (env (env-gen-empty-env lambda-env)))
      ;; add the name value bindings into the env
      (env-adds env lambda-arg arg)
      ;; carried out the body
      ;;(print lambda-body)
      (eval-value (begin-gen lambda-body) env))))

;; the lambda
(define lambda? (tag-named 'lambda))
(define lambda-gen (tag-add 'lambda))

;; selector
(define lambda-arg cadr)
(define lambda-body cddr)

;; lambda eval
(define (lambda-eval exp env)
  (lambda-code-gen env
                   (lambda-arg exp)
                   (lambda-body exp)))

;; application-eval
(define (application? exp)
  (and (list? exp)
       (not (null? exp))
       (or (symbol? (car exp))
           (lambda? (car exp)))))

(define application-name car)
(define application-arg cdr)

(define (list-of-values arg env)
  (map (lambda (e) (eval e env)) arg))

(define (application-eval exp env)
  (let ((name (eval (application-name exp) env))
        ;; adjust the args-eval-order and cbn-or-cbv
        (arg (list-of-values (application-arg exp) env)))
    (cond ((lambda-code? name)
           (lambda-code-apply name arg env))
          (else
           (apply name (map (lambda (e) (eval-value e env)) arg))))))

;;------------------------------
;; cond clause
(define cond? (tag-named 'cond))
(define cond-body cdr)
(define cond-body-first-pre caar)
(define cond-body-first-true cdar)
(define cond-body-remove-first cdr)

(define cond-gen (tag-add 'cond))

(define (cond-eval exp env)
  (let ((body (cond-body exp)))
    (if (null? body)
        'system-no-return-value
        (let ((rest (cond-body-remove-first body))
              (pre (cond-body-first-pre body))
              (true (cond-body-first-true body)))
          (if (eq? 'else pre)
              (eval (begin-gen true) env)
              (eval (if-gen
                     (list pre (begin-gen true) (cond-gen rest))) env))))))

;;------------------------------
;; let clause
(define let? (tag-named 'let))

;; selector
(define let-param cadr)
(define let-body cddr)
(define let-param-first-pair car)
(define let-param-remove-first-pair cdr)
(define let-param-pair-name car)
(define let-param-pair-value cadr)

;; let generater
(define let-gen (tag-add 'let))

;; change let exp into a lambda exp
(define (let->application exp)
  (let ((param (let-param exp))
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
      (cons lambda-fun input-param))))

;;------------------------------
;; evaluator
(define (eval exp env)
  (cond (debug
         (if (lambda-code? exp)
             (lambda-code-print exp)
             (print exp))
         (newline)))

  (cond ((internal-represent? exp) exp)
        ((self-evaluator? exp) (self-eval exp env))
        ((string? exp) (string-eval exp env))
        ((define? exp) (define-eval exp env))
        ((variable? exp) (variable-eval exp env))
        ((set? exp) (set-eval exp env))
        ((if? exp) (if-eval exp env))
        ((begin? exp) (begin-eval exp env))
        ((lambda? exp) (lambda-eval exp env))
        ((cond? exp) (cond-eval exp env))
        ((let? exp) (eval (let->application exp) env))
        ((application? exp) (application-eval exp env))
        (else
         (error "evaluation prefix not found" exp))))

;; get eval value
(define (eval-value exp env)
  (let ((result (eval exp env)))
    (if (or (self-evaluator? exp)
            (string? exp)
            (lambda-code? exp))
        result
        (eval-value result env))))

(define genv (env-gen-global-env '(+ - * / < > = <= >)
                                 (list + - * / < > = <= >)))

(define (geval exp)
  (let ((r (eval-value exp genv)))
    (if (lambda-code? r)
        (lambda-code-print r)
        (print r))))

;; ;; test
;; ;; self evlautor
;; (geval 1)
;; (geval #f)
;; (geval +)
;; ;; string
;; (geval '(string haha i am chenlin))
;; ;; define
;; (geval '(define a 1))
;; (geval 'a)
;; (geval '(define b a))
;; (geval 'b)
;; (geval '(set a 2))
;; (geval 'a)
;; (geval 'b)
;; (geval '(set b 3))
;; (geval 'b)
;; ;; if
;; (geval '(if #t 2 3))
;; ;; begin
;; (geval '(begin (define a 1)
;;                (set a 2)
;;                a))
;; ;; lambda and lambda code
;; (geval '(define tmp (lambda (x) (* 2 x) (+ 2 x))))
;; (geval '(define (tmp x) (* 2 x) (+ 2 x)))
;; (geval '(tmp 4))

;; ;; cond
;; (geval '(cond ((= 1 2) 2)
;;               ((< 1 1) 3)
;;               (else 4)))
;; ;; let
;; (geval '(let ((a 1)
;;               (b 2))
;;           (+ a 1)
;;           (+ b 1)))
;; ;; recursionally define
;; (geval '(define a 1))
;; (geval '(define b a))
;; (geval '(define c b))
;; (geval '(define d c))
;; (exit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; section 4.1.3 rewrite the environment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the frame design in the old implementation is very bad, i simply use the bindings as the name-and-value pair.this is not good consider that in many cases, we need to input a lot of the name-and-value pairs from lists of names and values. but it could be added to the apis of the environments.

;; the following environment objects are of the form ('env frame-object enclosing-env)

;; and for frame objects it's a pair of names and value

(define frame-gen cons)

(define frame-name car)
(define frame-value cdr)

(define (frame-add f name value)
  (set-car! f (cons name (frame-name f)))
  (set-cdr! f (cons value (frame-value f))))

(define (frame-adds f name-list value-list)
  (if (= (length name-list)
         (length value-list))
      (let ((names (frame-name f))
            (values (frame-value f)))
        (set-car! f (append name-list names))
        (set-cdr! f (append value-list values)))
      (error "the length of name-list is not equal to the value-list" name-list value-list)))

;; the value returned will be contained in a list
(define (frame-find f name)
  (let iter ((names (frame-name f))
             (values (frame-value f)))
    (cond ((null? names) 'name-not-found)
          ((eq? name (car names)) (list (car values)))
          (else (iter (cdr names) (cdr values))))))

(define (frame-change f name value)
  (let iter ((names (frame-name f))
             (values (frame-value f)))
    (cond ((null? names) 'name-not-found)
          ((eq? name (car names)) (set-car! values value) #t)
          (else (iter (cdr names) (cdr values))))))

;; ;; test
;; (define f (frame-gen '(a b) '(1 2)))
;; (print f)
;; (frame-adds f '(x y) '(3 4))
;; (print f)
;; (print (frame-find f 't))
;; (print (frame-find f 'x))
;; (frame-change f 'x 10)
;; (print f)
;; (exit)

(define env-empty-env (list))
(define env-empty-names (list))
(define env-empty-values (list))

;; generate the environment
(define (env-gen parent-env names values)
  (tag-gen 'env (frame-gen names values) parent-env))

;; generate an empty global environment
(define (env-gen-empty-global-env)
  (env-gen env-empty-env env-empty-names env-empty-values))

;; generate a global environment
(define (env-gen-global-env names values)
  (env-gen env-empty-env names values))

;; generate an empty environment
(define (env-gen-empty-env parent-env)
  (env-gen parent-env env-empty-names env-empty-values))

(define env-frame cadr)
(define env-parent-env caddr)

(define (env-add env name data)
  (let ((f (env-frame env)))
    (frame-add f name data)))

(define (env-adds env names values)
  (frame-adds (env-frame env) names values))

(define (env-parent-null? env)
  (null? (env-parent-env env)))

(define (env-exist? env name)
  (let ((result (frame-find (env-frame env) name)))
    (cond ((eq? result 'name-not-found)
           (if (env-parent-null? env)
               #f
               (env-exist? (env-parent-env env) name)))
          (else #t))))

(define (env-search env name)
  (let ((result (frame-find (env-frame env) name)))
    (cond ((eq? result 'name-not-found)
           (if (env-parent-null? env)
               (error "name not found -- " name)
               (env-search (env-parent-env env) name)))
          ((eq? (car result) '*unassigned*)
           (error "value unassigned -- " value))
          (else (car result)))))

(define (env-change env name value)
  (if (eq? (frame-change (env-frame env) name value)
           'name-not-found)
      (if (env-parent-null? env)
          (error "name not found -- " name)
          (env-change (env-parent-env env) name value))))

;; ;; test
;; (define e1 (env-gen-empty-global-env))
;; (print e1)
;; (define e1 (env-gen-global-env '(t) '(1)))
;; (print e1)
;; (print (env-frame e1))
;; (env-add e1 'a 1)
;; (print e1)
;; (env-add e1 'b 2)
;; (print e1)
;; (print (env-search e1 'a))
;; (print (env-search e1 'b))
;; (env-change e1 'a 3)
;; (print (env-search e1 'a))
;; (env-change e1 'b 4)
;; (print (env-search e1 'b))
;; ;; (print (env-search e1 'x))
;; (define e2 (env-gen-empty-env e1))
;; (print e2)
;; (print (env-frame e2))
;; (env-add e2 'a 2)
;; (print e2)
;; (env-add e2 'b 1)
;; (print e2)
;; (print (env-search e2 'a))
;; (print (env-search e2 'b))
;; ;;(print (env-search e2 'x))
;; (env-change e1 'a 4)
;; (print (env-search e1 'a))
;; (env-change e1 'b 3)
;; (print (env-search e1 'b))
;; (env-adds e2 '(c d) '(*unassigned* *unassigned*))
;; (env-add e1 'e '*unassigned*)
;; (print (env-exist? e2 'c))
;; (print (env-exist? e2 'e))
;; (print (env-exist? e2 'x))
;; ;; (print e2)
;; (env-search e2 'e)
;; (exit)


(define genv (env-gen-global-env '(+ - * / < > = <= >= list car cdr display newline)
                                 (list + - * / < > = <= >= list car cdr display newline)))

(define (geval exp)
  (let ((r (eval-value exp genv)))
    (if (lambda-code? r)
        (lambda-code-print r)
        (print r))))

;; ;; test
;; ;; self evlautor
;; (geval 1)
;; (geval #f)
;; (geval +)
;; ;; string
;; (geval '(string haha i am chenlin))
;; ;; define
;; (geval '(define a 1))
;; (geval 'a)
;; (geval '(define b a))
;; (geval 'b)
;; (geval '(set a 2))
;; (geval 'a)
;; (geval 'b)
;; (geval '(set b 3))
;; (geval 'b)
;; ;; if
;; (geval '(if #t 2 3))
;; ;; begin
;; (geval '(begin (define a 1)
;;                (set a 2)
;;                a))
;; ;; lambda and lambda code
;; (geval '(define tmp (lambda (x) (* 2 x) (+ 2 x))))
;; (geval '(define (tmp x) (* 2 x) (+ 2 x)))
;; (geval '(tmp 4))

;; ;; cond
;; (geval '(cond ((= 1 2) 2)
;;               ((< 1 1) 3)
;;               (else 4)))
;; ;; let
;; (geval '(let ((a 1)
;;               (b 2))
;;           (+ a 1)
;;           (+ b 1)))
;; ;; the recursion definition
;; (geval '(define a 1))
;; (geval '(define b a))
;; (geval '(define c b))
;; (geval '(define d c))
;; (exit)

;; the list
;; (geval '(car (list 1 2)))
;; the writeline
;; (geval '(display 1))
;; (geval '(define (double x) (+ x x)))
;; (geval '(double (begin (display 12)
;;                        (newline)
;;                        12)))
;; (exit)
;; test for the reciprocal function
;; (geval '(define (f x)
;;           (define (odd? x)
;;             (if (= x 0) 1 (+ 2 (even? (- x 1)))))
;;           (define (even? x)
;;             (if (= x 0) 0 (+ 1 (odd? (- x 1)))))
;;           (even? x)))

;; (geval '(f 5))
;; (exit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; to define the internal define
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is not nececssarily needed for my interpreter. However, if you want you interpreter to have the syntax checking ability, it should be coded like this.

;; the main goal of this is to get the define inside a define to become a let primitive.

;; scan the input of a lambda code block, and pick up the define inside it and use a let instead.
(define (lambda-code->let-form l)
  (let ((names (list))      ;; for initialize the let
        (change-body (list));; the renewed body with set
        (lambda-body (lambda-code-body l)))
    (do ((rest lambda-body (cdr rest)))
        ((null? rest) (cond ((null? names) l)
                            (else (lambda-code-gen (lambda-code-env l)
                                                   (lambda-code-arg l)
                                                   (list (let-gen (cons (map (lambda (n) (list n '*unassigned*)) names)
                                                                        change-body)))))))
      (let ((first (car rest)))
        (cond ((define? first) (let ((name (define-name first))
                                     (value (define-value first)))
                                 (cond ((define-lambda? name)
                                        (set! name (define-lambda-name first))
                                        (set! value (lambda-gen (append (list (define-lambda-arg first))
                                                                        (define-lambda-body first))))))
                                 (set! names (append names (list name)))
                                 (set! change-body (append change-body (list (set-gen (list name value)))))))
              (else (set! change-body (append change-body (list first)))))))))

;; test for the lambda-code->let-from
;; without any define
;; (print (lambda-code->let-from '(lambda-code (env) (a b c)
;;                                             ((set a 1)
;;                                              (set b 23)
;;                                              (+ c 44)))))
;; (print (lambda-code->let-from '(lambda-code (env) (a b c)
;;                                             ((define a 1)
;;                                              (+ 1 3)
;;                                              (define (b c) (+ c 1))))))
;; (exit)

(define (define-eval exp env)
  (let ((name (define-name exp))
        (value (define-value exp)))
    ;; add the syntax sugar for the define
    (cond ((lambda? value)
           (set! value (eval value env)))
          ((define-lambda? name)
           (set! name (define-lambda-name exp))
           (set! value (lambda-code-gen env
                                        (define-lambda-arg exp)
                                        (define-lambda-body exp)))))
    (if (lambda-code? value)
        (set! value (lambda-code->let-form value)))
    (env-add env name value)
    (eval-value value env)))

;; ;; test for the *unassigned*
;; (geval '(define t *unassigned*))
;; (geval 't)
;; (exit)

;; ;; test the result
;; (geval '(define (even? x)
;;           (define (even x) (if (= x 0) #t (odd (- x 1))))
;;           (define (odd x) (if (= x 0) #f (even (- x 1))))
;;           (even x)))
;; (geval '(even? 4))
;; (exit)

;; test for performance
;; (define starttime (current-milliseconds))
;; (geval '(define r (lambda (n)
;;                     (if (< n 0)
;;                         0
;;                         (r (- n 1))))))
;; (geval '(r 4000))
;; (define endtime (current-milliseconds))
;; (print (- endtime starttime))
;; (define starttime (current-milliseconds))
;; (geval '(define (fib n)
;;                   (if (= n 0)
;;                       1
;;                       (if (= n 1)
;;                           1
;;                           (+ (fib (- n 1))
;;                              (fib (- n 2)))))))
;; (geval '(fib 20))
;; (define endtime (current-milliseconds))
;; (print (- endtime starttime))
;; (define starttime (current-milliseconds))
;; (geval '(define (fib n)
;;                   (if (= n 0)
;;                       1
;;                       (if (= n 1)
;;                           1
;;                           (+ (fib (- n 1))
;;                              (fib (- n 2)))))))

;; (define add-fib (lambda (n) (if (= 0 n)
;;                                 (list)
;;                                 (cons '(fib 2) (add-fib (- n 1))))))
;; (geval (cons 'begin (add-fib 1000)))
;; (define endtime (current-milliseconds))
;; (print (- endtime starttime))
;; (exit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; analyzer instead of evaluator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rewrite the eval
(define (eval exp env)
  (cond (debug
          (if (lambda-code? exp)
              (lambda-code-print exp)
              (print exp))))

  ((analyze exp) env))

(define (geval exp)
  (let ((result (eval exp genv)))
    (if (lambda-code? result)
        (lambda-code-print result)
        (print result))))

;;--------------------
;; self-analyze
(define (self-analyze exp)
  (lambda (env) exp))

;;--------------------
;; string-analyze
(define (string-analyze exp)
  (lambda (env) (tag-data exp)))

;;--------------------
;; define-analyze
(define (define-analyze exp)
  (let ((name (define-name exp))
        (value-analyze (define-value exp)))
    ;; add the syntax sugar for teh define
    (cond ((lambda? value-analyze)
           (set! value-analyze (analyze value-analyze)))
          ((define-lambda? name)
           (set! name (define-lambda-name exp))
           (set! value-analyze (analyze (lambda-gen (cons (define-lambda-arg exp)
                                                          (define-lambda-body exp))))))
          (else (set! value-analyze (analyze value-analyze))))

    (lambda (env)
      (let ((value (value-analyze env)))
        (env-add env name value)
        value))))

;;--------------------
;; variable-analyze
(define (variable-analyze exp)
  (lambda (env)
    ((analyze (env-search env exp)) env)))

;;--------------------
;; set-analyze
(define (set-analyze exp)
  (let ((name (set-name exp))
        (value-analyze (analyze (set-value exp))))
    (lambda (env)
      (let ((value (value-analyze env)))
        (if (env-exist? env name)
          (env-change env name value)
          (env-add env name value))
        value))))

;;--------------------
;; if-analyze
(define (if-analyze exp)
  (let ((pre (analyze (if-pre exp)))
        (true (analyze (if-true exp)))
        (false (analyze (if-false exp))))
    (lambda (env)
      (if (pre env)
          (true env)
          (false env)))))

;;--------------------
;; begin-analyze
(define (begin-analyze exp)
  (define (sequential a1 a2)
    (lambda (env) (a1 env) (a2 env)))

  (define (loop a r)
    (cond ((null? r) a)
          (else (loop (sequential a (car r)) (cdr r)))))

  (let ((analyzers (map (lambda (exp) (analyze exp))
                        (begin-body exp))))
    (if (null? analyzers)
        (error "empty begin clause -- " exp))
    (loop (car analyzers) (cdr analyzers))
    ;; (lambda (env)
    ;;   (let iter ((rest analyzers))
    ;;     (cond ((= (length rest) 1)
    ;;            ((car rest) env))
    ;;           (else
    ;;            ((car rest) env)
    ;;            (iter (cdr rest))))))
    ))

;;--------------------
;; lambda-analyze
(define (lambda-analyze exp)
  (lambda (env)
    (lambda-code-gen env
                     (lambda-arg exp)
                     (lambda-body exp))))

;;--------------------
;; application-analyze
(define (list-of-analyzers arg)
  (map (lambda (exp) (analyze exp)) arg))

(define (application-analyze exp)
  (let ((name-analyzer (analyze (application-name exp)))
        ;; adjust eth args-eval-order and cbn-or-cbv
        (args-analyzers (list-of-analyzers (application-arg exp))))
    ;; (print "finish the analyze in app")
    (lambda (env)
      (let ((name (name-analyzer env))
            (args (map (lambda (a) (a env)) args-analyzers)))
        (cond ((lambda-code? name)
               (lambda-code-apply name args env))
              (else
               (apply name args)))))))

(define (analyze exp)
  (cond (debug
         (if (lambda-code? exp)
             (lambda-code-print exp)
             (print exp))))

  (cond ((internal-represent? exp) (self-analyze exp))
        ((self-evaluator? exp) (self-analyze exp))
        ((string? exp) (string-analyze exp))
        ((define? exp) (define-analyze exp))
        ((variable? exp) (variable-analyze exp))
        ((set? exp) (set-analyze exp))
        ((if? exp) (if-analyze exp))
        ((begin? exp) (begin-analyze exp))
        ((lambda? exp) (lambda-analyze exp))
        ((let? exp) (analyze (let->application exp)))
        ((application? exp) (application-analyze exp))
        (else
         (error "undefined prefix for the analyzer -- " exp))))

;; ;; test
;; ;; self evlautor
;; (geval 1)
;; (geval #f)
;; (geval +)
;; ;; string
;; (geval '(string haha i am chenlin))
;; ;; define
;; (geval '(define a 1))
;; (geval 'a)
;; (geval '(define b a))
;; (geval 'b)
;; (geval '(set a 2))
;; (geval 'a)
;; (geval 'b)
;; (geval '(set b 3))
;; (geval 'b)
;; ;; if
;; (geval '(if #t 2 3))
;; ;; begin
;; (geval '(begin (define a 1)
;;                (set a 2)
;;                a))
;; lambda and lambda code
;; (geval '(define tmp (lambda (x) (* 2 x) (+ 2 x))))
;; (geval '(define (tmp x) (* 2 x) (+ 2 x)))
;; (geval '(tmp 4))

;; ;; cond
;; (geval '(cond ((= 1 2) 2)
;;               ((< 1 1) 3)
;;               (else 4)))
;; let
;; (geval '(let ((a 1)
;;               (b 2))
;;           (+ a 1)
;;           (+ b 1)))
;; (exit)
;; test for performance
;; (define starttime (current-milliseconds))
;; (geval '(define r (lambda (n)
;;                     (if (< n 0)
;;                         0
;;                         (r (- n 1))))))
;; (geval '(r 4000))
;; (define endtime (current-milliseconds))
;; (print (- endtime starttime))
;; (define starttime (current-milliseconds))
;; (geval '(define (fib n)
;;                   (if (= n 0)
;;                       1
;;                       (if (= n 1)
;;                           1
;;                           (+ (fib (- n 1))
;;                              (fib (- n 2)))))))
;; (geval '(fib 20))
;; (define endtime (current-milliseconds))
;; (print (- endtime starttime))
;; (define starttime (current-milliseconds))
;; (geval '(define (fib n)
;;                   (if (= n 0)
;;                       1
;;                       (if (= n 1)
;;                           1
;;                           (+ (fib (- n 1))
;;                              (fib (- n 2)))))))

;; (define add-fib (lambda (n) (if (= 0 n)
;;                                 (list)
;;                                 (cons '(fib 2) (add-fib (- n 1))))))
;; (geval (cons 'begin (add-fib 2000)))
;; (define endtime (current-milliseconds))
;; (print (- endtime starttime))
;; (exit)
