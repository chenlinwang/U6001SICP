(load "../BookExercise/basic")
(define (loadimp42) (load "../BookImplementation/sec4.2"))
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
  (and (pair? t)
       (not (null? t))
       (symbol? (car t))))

;; generate a lambda function for recognizing the tagged-data with name n
(define (tag-name-check-func-gen n)
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

(define (tag-add-name-func-gen name)
  (lambda (d)
    (cons name d)))
;;------------------------------
;; the environment object
;;------------------------------
;; for every level of evaluation, the environment object could be look up using (env-look-up env)

;; the data structure of the env is ('env (parent-env) (bindings))

;; env?
(define env? (tag-name-check-func-gen 'env))

;; operator for env
(define env-parent-null? null?)

;; the empty environment
(define env-empty-env (list))
(define env-empty-names (list))
(define env-empty-values (list))

;; generate an environment
(define (env-gen parent-env names values)
  (tag-gen 'env parent-env (map (lambda (n v) (bindings-pair n v)) names values)))

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

;; add pairs of name and value into the environment all together
(define (env-adds env names values)
  (if (= (length names) (length values))
      (env-set-new-bindings env (bindings-append (map (lambda (n v) (bindings-pair n v))
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
      ;; just in case we evaluate the thunk, actually there is no such chance
      (thunk? v)
      (thunk-memo? v)
      ))

(define (self-eval exp env) exp)
;;------------------------------
;; string
(define string? (tag-name-check-func-gen 'string))
(define (string-eval exp env) (tag-data exp))


;;------------------------------
;; the thunk object
(define thunk? (tag-name-check-func-gen 'thunk))
(define thunk-gen (tag-add-name-func-gen 'thunk))

(define thunk-var cadr)
(define thunk-env caddr)

;; the functions to use the thunk
(define (delay-var var env)
  (thunk-gen (list var env)))

(define (list-of-delayed-var vars env)
  (map (lambda (v) (delay-var v env))
       vars))

;; ;; 1: only with the thunk, without memorized
;; (define (force-it t)
;;   (cond ((thunk? t)
;;          (force-it (eval (thunk-var t)
;;                          (thunk-env t))))
;;         (else
;;          t)))

(define thunk-memo? (tag-name-check-func-gen 'thunk-memo))
;; 2: with the memorized thunk
(define thunk-memo-value cdr)

(define (thunk-set-to-memo t value)
  (set-car! t 'thunk-memo)
  (set-cdr! t value))

;; (define (force-it t)
;;   (cond ((thunk-memo? t) (thunk-memo-value t))
;;         ((thunk? t)
;;          (let ((value (force-it (eval (thunk-var t)
;;                                       (thunk-env t)))))
;;            (thunk-set-to-memo t value)
;;            value))
;;         (else t)))

;; 3: with the extension to the original scheme
(define thunk-lazy? (tag-name-check-func-gen 'thunk-lazy))
(define thunk-lazy-gen (tag-add-name-func-gen 'thunk-lazy))

(define thunk-lazy-var cadr)
(define thunk-lazy-env caddr)

;; statement of a type
(define statement? (lambda (s) (and (list? s)
                                    (symbol? (cadr s)))))
(define statement-var car)
(define statement-type cadr)
(define statement-thunk-lazy 'lazy)
(define statement-thunk-memo 'lazy-memo)

(define (list-of-right-var vars lambda-vars env)
  (map (lambda (v lv) (cond ((symbol? lv) (actual-value v env))
                         ((statement? lv)
                          (cond ((eq? statement-thunk-lazy
                                      (statement-type lv))
                                 (thunk-lazy-gen (list v
                                                       env)))
                                ((eq? statement-thunk-memo
                                      (statement-type lv))
                                 (thunk-gen (list v
                                                  env)))
                                (else
                                 (error "undefined type for evaluation!" v))))
                         (else
                          (error "undefined syntax for the argument declaration!" v))))
       vars lambda-vars))

(define (lambda-list-of-args arg)
  (map (lambda (a) (cond ((symbol? a) a)
                         ((statement? a) (statement-var a))
                         (else
                          (error "undefined syntax for the argument declaration!" a))))
       arg))

(define (force-it t)
  (cond ((thunk-memo? t) (thunk-memo-value t))
        ((thunk? t)
         (let ((value (force-it (eval (thunk-var t)
                                      (thunk-env t)))))
           (thunk-set-to-memo t value)
           value))
        ((thunk-lazy? t)
         (force-it (eval (thunk-lazy-var t)
                         (thunk-lazy-env t))))
        (else t)))


(define (actual-value var env)
  (force-it (eval var env)))

(define (list-of-actual-value vars env)
  (map (lambda (v) (force-it (eval v env)))
       vars))

;;------------------------------
;; define clause
;; selector
(define define-name cadr)
(define define-value caddr)

(define define-lambda? list?)
(define define-lambda-name caadr)
(define define-lambda-arg cdadr)
(define define-lambda-body cddr)

(define define? (tag-name-check-func-gen 'define))
(define define-gen (tag-add-name-func-gen 'define))

(define (define-eval exp env)
  (let ((name (define-name exp))
        (value (define-value exp)))
    ;; add the syntax sugar for the define
    (cond ((and (lambda? value)
                (symbol? name))
           (set! value (eval value env)))
          ((define-lambda? name)
           (set! name (define-lambda-name exp))
           (set! value (lambda-code-gen env
                                        (define-lambda-arg exp)
                                        (define-lambda-body exp)))))
    (let ((r (eval value env)))
      (cond (debug
             (print "At define eval:")
             (print "name:")
             (env-check-print name)
             (print "value:")
             (env-check-print value)
             (newline)))
      (env-add env name '*unassigned*)
      (env-change env name r)
      r)))

;;------------------------------
;; variable
(define variable? symbol?)
(define (variable-eval exp env)
  (env-search env exp))

;;------------------------------
;; set
(define set? (tag-name-check-func-gen 'set))
;; selector
(define set-name cadr)
(define set-value caddr)
(define set-gen (tag-add-name-func-gen 'set))

(define (set-eval exp env)
  (let ((name (set-name exp))
        (value (set-value exp)))
    (let ((r (eval value env)))
      (if (env-exist? env name)
          (env-change env name r)
          (env-add env name r))
      r)))
;;------------------------------
;; if clause
(define if? (tag-name-check-func-gen 'if))
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
    (if (actual-value pre env)
        (eval true env)
        (eval false env))))

(define if-gen (tag-add-name-func-gen 'if))
;;------------------------------
;; begin clause
(define begin? (tag-name-check-func-gen 'begin))
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

(define begin-gen (tag-add-name-func-gen 'begin))
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
(define lambda-code? (tag-name-check-func-gen 'lambda-code))

;; lambda code print
(define (lambda-code-print r)
  (print (list (lambda-code-arg r)
               (lambda-code-body r))))

;; put the old env in the list just for adjustment
(define (lambda-code-apply lambda-code arg old-env)
  (let ((lambda-arg (lambda-code-arg lambda-code))
        (lambda-body (lambda-code-body lambda-code))
        (lambda-env (lambda-code-env lambda-code)))
    ;; 1 and 2: with all delayed or memo-delayed
    (set! arg (list-of-delayed-var arg old-env))
    ;; 3: change the right type of declaration to the varible
    ;; (set! arg (list-of-right-var arg lambda-arg old-env))
    (let ( ;;lexical-or-dynamic-coping
          (env (env-gen-empty-env lambda-env)))
      ;; add the name value bindings into the env
      ;; 1 and 2:
      (env-adds env lambda-arg arg)
      ;; 3:
      ;; (env-adds env (lambda-list-of-args lambda-arg) arg)
      (cond (debug
           (print "enter lambda code apply:")
           (write "arg:")
           (env-check-print arg)
           (newline)
           (write "new env:")
           (env-check-print (cdr env))
           (newline)))
      ;; carried out the body
      (eval (begin-gen lambda-body) env))))

;; the lambda
(define lambda? (tag-name-check-func-gen 'lambda))
(define lambda-gen (tag-add-name-func-gen 'lambda))

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
  (let ((name (actual-value (application-name exp) env))
        ;; adjust the args-eval-order and cbn-or-cbv
        (arg (application-arg exp)))
    (cond (debug
           (print "enter application eval:")
           (write "name:")
           (env-check-print name)
           (write "arg:")
           (env-check-print arg)
           (newline)))
    (cond ((lambda-code? name)
           (lambda-code-apply name arg env))
          (else
           (apply name (list-of-actual-value arg env))))))

;;------------------------------
;; cond clause
(define cond? (tag-name-check-func-gen 'cond))
(define cond-body cdr)
(define cond-body-first-pre caar)
(define cond-body-first-true cdar)
(define cond-body-remove-first cdr)

(define cond-gen (tag-add-name-func-gen 'cond))

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
(define let? (tag-name-check-func-gen 'let))

;; selector
(define let-param cadr)
(define let-body cddr)
(define let-param-first-pair car)
(define let-param-remove-first-pair cdr)
(define let-param-pair-name car)
(define let-param-pair-value cadr)

;; let generater
(define let-gen (tag-add-name-func-gen 'let))

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
         (print "enter eval")
         (env-check-print exp)
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


(define genv (env-gen-global-env '(+ - * / < > = <= > print list eq?)
                                 (list + - * / < > = <= > print list eq?)))

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
  (let ((r (actual-value exp genv)))
    (cond ((lambda-code? r)
           (write gevalnum)
           (write ">>>")
           (set! gevalnum (+ 1 gevalnum))
           (lambda-code-print r))
          (else
           (write gevalnum)
           (write ">>>")
           (set! gevalnum (+ 1 gevalnum))
           (env-check-print r)))))


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
;; ;; test for the normal order
;; (geval '(define (foo x)
;;           (print 1)
;;           (+ x x)))
;; (geval '(foo (begin (print 2) 2)))
;; (exit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the recursion input module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define input-prompt "-in  >>>")

(define (driver-loop)
  (write gevalnum)
  (write input-prompt)
  (let ((input (read)))
    (cond ((and (list? input)
                (eq? (car input) 'exit))
           (print "end of recursion ;P\n"))
          ((geval input)
           (newline)
           (driver-loop)))))

;; (driver-loop)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; inplement the cons and car cdr as the procedure instread of the primitives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(geval '(define (cons x y)
          (lambda (m) (m x y))))
(geval '(define (car z)
          (z (lambda (x y) x))))
(geval '(define (cdr z)
          (z (lambda (x y) y))))
(geval '(define (list-ref l n)
          (cond ((= n 0) (car l))
                (else (list-ref (cdr l)
                                (- n 1))))))
(geval '(define nil (list)))
(geval '(define (null? l)
          (eq? l nil)))

(geval '(define (map proc l)
          (cond ((null? l) (list))
                (else
                 (cons (proc (car l))
                       (map proc (cdr l)))))))


(geval '(define x (cons 1 (cons 2 (cons 3 (cons 4 nil))))))
;; (geval '(car x))
;; (geval '(list-ref x 0))
;; (geval '(list-ref x 1))
;; (geval '(list-ref x 2))
;; (geval '(list-ref x 3))
;; (geval '(null? (list)))
(geval '(define double-x (map (lambda (e) (* 2 e))
                              x)))
;; (geval '(list-ref double-x 0))
;; (geval '(list-ref double-x 1))
;; (geval '(list-ref double-x 2))
;; (geval '(list-ref double-x 3))
(geval '(define (scale-list l n)
          (map (lambda (e) (* e n))
               l)))
(geval '(define (add-list l1 l2)
          (cons (+ (car l1)
                   (car l2))
                (add-list (cdr l1)
                          (cdr l2)))))
(geval '(define ones (cons 1 ones)))
(geval '(define ints (cons 1 (add-list ones
                                       ints))))
(geval '(define (integral list init dt)
          (define result (cons init
                               (add-list result
                                         (scale-list list dt))))
          result))
(geval '(define integral-int (integral ints 0 2)))
(geval '(define (solve f y0 dt)
          (define y (integral dy y0 dt))
          (define dy (map f y))
          y))
;; (geval '(list-ref (solve (lambda (x) x) 1 0.001) 1000))
;; (driver-loop)
;; (exit)
