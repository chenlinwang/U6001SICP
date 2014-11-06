(define (loadlec21) (load "../LectureImplementation/lec21"))
(load "../BookExercise/basic")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Silly test about the CBN and CBV
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (foo x)
  (print "entering foo function!")
  (+ x x))

;; ;; test
;; (print (foo (begin (print "entering the begin clause!")
;;                    222)))
;; (exit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the lazy evaluation with 1 (only delay) 2 (memoried delay)
;; 3 (type define for the used in lambda: delay and memoried delay
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; search key word
;; args-eval-order: adjust the order of evaluation of the arguments in a application evaluation, default left to right
;; CBN-or-CBV: adjust the call value protocol, default CBV
;; lexical-or-dynamic-coping: adjust the scoping rules, default lexical
;; internal-represent-register: to register the internal representation

(define debug #f)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rewrite the evaluator again
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
(define (tag-name-check-function n)
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

;; generate a lambda function for adding a tag to a list of data
(define (tag-add-name-function name)
  (lambda (d)
    (cons name d)))
;;------------------------------
;; the environment object
;;------------------------------
;; for every level of evaluation, the environment object could be look up using (env-look-up env)

;; the data structure of the env is ('env (parent-env) (bindings))

;; env?
(define env? (tag-name-check-function 'env))

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
;; ;; unsigned test
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
      (lambda-code? v)))

(define (self-eval exp env) exp)
;;------------------------------
;; string
(define string? (tag-name-check-function 'string))
(define (string-eval exp env) (tag-data exp))

;;------------------------------
;; define clause
;; selector
(define define-name cadr)
(define define-value caddr)

(define define-lambda? list?)
(define define-lambda-name caadr)
(define define-lambda-arg cdadr)
(define define-lambda-body cddr)

(define define? (tag-name-check-function 'define))
(define define-gen (tag-add-name-function 'define))

(define (define-eval exp env)
  ;;(print "enter define")
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
    ;; (print "end")
    (env-add env name '*unassigned*)
    (env-change env name (eval value env))
    (env-search env name)))

;;------------------------------
;; variable
(define variable? symbol?)
(define (variable-eval exp env)
  (env-search env exp))

;;------------------------------
;; set
(define set? (tag-name-check-function 'set))
;; selector
(define set-name cadr)
(define set-value caddr)
(define set-gen (tag-add-name-function 'set))

(define (set-eval exp env)
  (let ((name (set-name exp))
        (value (set-value exp)))
    (if (env-exist? env name)
        (env-change env name value)
        (env-add env name value))
    (eval value env)))
;;------------------------------
;; if clause
(define if? (tag-name-check-function 'if))
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

(define if-gen (tag-add-name-function 'if))
;;------------------------------
;; begin clause
(define begin? (tag-name-check-function 'begin))
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

(define begin-gen (tag-add-name-function 'begin))
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
(define lambda-code? (tag-name-check-function 'lambda-code))

;; lambda code print
(define (lambda-code-print r)
  (print (list (lambda-code-arg r)
               (lambda-code-body r))))
;; arg
(define lambda-arg-name car)
(define lambda-arg-type cadr)

;; ;; 1 2 just use the delay thunk or solved thunk
;; (define (lambda-code-apply lambda-code arg old-env)
;;   (let ((lambda-arg (lambda-code-arg lambda-code))
;;         (lambda-body (lambda-code-body lambda-code))
;;         (lambda-env (lambda-code-env lambda-code))
;;         (arg (list-of-delay-exp arg old-env)))
;;     ;;lexical-or-dynamic-coping
;;     (let ((env (env-gen-empty-env lambda-env)))
;;       ;; add the name value bindings into the env
;;       (env-adds env lambda-arg arg)
;;       ;; carried out the body
;;       ;;(print lambda-body)
;;       (eval (begin-gen lambda-body) env))))
;; ;; 1 2 end

;; 3 put the old env in the list just for adjustment
(define (lambda-code-apply lambda-code arg old-env)
  (let ((lambda-arg (lambda-code-arg lambda-code))
        (lambda-body (lambda-code-body lambda-code))
        (lambda-env (lambda-code-env lambda-code)))
    (let ((arg (doublemap (lambda (la a)
                            (cond ((list? la)
                                   (let ((arg-type (lambda-arg-type la)))
                                     (cond ((eq? arg-type 'thunk)
                                            (delay-gen (list a old-env)))
                                           ((eq? arg-type 'thunk-memoried)
                                            (memoried-gen (list a old-env)))
                                           (else
                                            (error "lambda code type define unknown! -- " arg-type)))))
                                  (else
                                    (actual-value a old-env))))
                          lambda-arg arg))
          (lambda-arg (map (lambda (la) (if (list? la) (lambda-arg-name la) la)) lambda-arg)))

      ;; (print "enter lambda-code-apply")
      ;; (env-check-print arg)
      ;; (print "current old env")
      ;; (env-check-print (cdr old-env))
      ;; (print "current lambda env")
      ;; (env-check-print (cdr lambda-env))
      ;;lexical-or-dynamic-coping
      (let ((env (env-gen-empty-env lambda-env)))
        ;; add the name value bindings into the env
        (env-adds env lambda-arg arg)
        ;; (print "current new env")
        ;; (env-check-print (cdr env))
        ;; carried out the body
        ;;(print lambda-body)
        (eval (begin-gen lambda-body) env)))))
;; 3 end

;; the lambda
(define lambda? (tag-name-check-function 'lambda))
(define lambda-gen (tag-add-name-function 'lambda))

;; selector
(define lambda-arg cadr)
(define lambda-body cddr)

;; lambda eval
(define (lambda-eval exp env)
  (lambda-code-gen env
                   (lambda-arg exp)
                   (lambda-body exp)))

;; application-eval
;; the delay tags
(define delay-tag? (tag-name-check-function 'thunk))
(define delay-gen (tag-add-name-function 'thunk))
(define delay-exp cadr)
(define delay-env caddr)
(define (list-of-delay-exp exp env)
  (map (lambda (e) (delay-gen (list e env))) exp))

;; ;; 1 only use delay tag
;; (define (force-it exp)
;;   ;; (if (not (lambda-code? exp))
;;   ;;     (cond ((delay-tag? exp)
;;   ;;            (write-line "force-it-delay:")
;;   ;;            (print (delay-exp exp)))
;;   ;;           (else
;;   ;;            (write-line "force-it-direct:")
;;   ;;            (print exp)))
;;   ;;     (begin (write-line "force-it-direct:")
;;   ;;            (lambda-code-print exp)))
;;   (cond ((delay-tag? exp)
;;          (actual-value (delay-exp exp) (delay-env exp)))
;;         (else exp)))
;; ;; 1 end

;; ;; 2 new force it, with memorized feature
;; (define solved-tag? (tag-name-check-function 'solved))
;; (define solved-value cadr)
;; (define (solve-delay-thunk d v)
;;   (set-car! d 'solved)
;;   (set-car! (cdr d) v)
;;   (set-cdr! (cdr d) (list)))
;; (define (force-it exp)
;;   (cond ((delay-tag? exp)
;;          (let ((result (actual-value (delay-exp exp) (delay-env exp))))
;;            (solve-delay-thunk exp result)
;;            result))
;;         ((solved-tag? exp) (solved-value exp))
;;         (else exp)))
;; ;; 2 end

;; 3 the memoried thunk
(define memoried-tag? (tag-name-check-function 'thunk-memoried))
(define memoried-gen (tag-add-name-function 'thunk-memoried))
(define memoried-value cadr)
(define (solve-thunk-memoried d v)
  (set-car! d 'thunk-solved)
  (set-car! (cdr d) v)
  (set-cdr! (cdr d) (list)))

;; the solved thunk
(define solved-tag? (tag-name-check-function 'thunk-solved))
(define solved-value cadr)

;; new force-it
(define (force-it exp)
  ;; (write-line "force-it:")
    ;; (if (lambda-code? exp)
    ;;   (lambda-code-print exp)
    ;;   exp)
  (cond ((delay-tag? exp)
         ;; (print "delay-tag")
         (actual-value (delay-exp exp) (delay-env exp)))
        ((memoried-tag? exp)
         ;; (print "memoried-tag")
         (let ((result (actual-value (delay-exp exp) (delay-env exp))))
           (solve-thunk-memoried exp result)
           ;; (print result)
           result))
        ((solved-tag? exp)
         ;; (print "solved-tag")
         (solved-value exp))
        (else
         ;; (print "base value")
         exp)))
;; 3 end

(define (actual-value exp env)
  ;; (write-line "actual value:")
  ;; (print exp)
  (force-it (eval exp env)))
(define (list-of-actual-exp exp env)
  ;; (write-line "list-of-actual-exp:")
  ;; (print exp)
  (map (lambda (e) (actual-value e env)) exp))

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
  ;;(print "enter application eval")
  (let ((name (actual-value (application-name exp) env))
        ;; adjust the args-eval-order and cbn-or-cbv
        (arg (application-arg exp)))
    (cond ((lambda-code? name)
           (lambda-code-apply name arg env))
          (else
           ;; (print name)
           ;; (print (list-of-actual-exp arg env))
           ;; (print "---")
           (if (equal? name cons)
               (apply name (list-of-values arg env))
               (apply name (list-of-actual-exp arg env)))))))

;;------------------------------
;; cond clause
(define cond? (tag-name-check-function 'cond))
(define cond-body cdr)
(define cond-body-first-pre caar)
(define cond-body-first-true cdar)
(define cond-body-remove-first cdr)

(define cond-gen (tag-add-name-function 'cond))

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
(define let? (tag-name-check-function 'let))

;; selector
(define let-param cadr)
(define let-body cddr)
(define let-param-first-pair car)
(define let-param-remove-first-pair cdr)
(define let-param-pair-name car)
(define let-param-pair-value cadr)

;; let generater
(define let-gen (tag-add-name-function 'let))

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
         (write-line "eval input exp:")
         (cond ((lambda-code? exp) (lambda-code-print exp))
               (else (env-check-print exp)))))

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
        ;; ((cons? exp) (cons-eval exp env))
        ;; ((car? exp) (car-eval exp env))
        ;; ((cdr? exp) (cdr-eval exp env))
        ((application? exp) (application-eval exp env))
        (else
         (error "evaluation prefix not found" exp))))


(define genv (env-gen-global-env
              '(+ - * / < > = <= > >= print null? cons car cdr list remainder not)
              (list + - * / < > = <= > >= print null? cons car cdr list remainder not)))

(define (env-check-print exp)
  (define (write-prefix distance)
    (do ((start distance (- start 1)))
        ((= start 0) (write ""))
      (write "")))
  (let outsideiter ((start exp)
                    (distance 5))
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
(define output-promt "-out >>>")
(define (geval exp)
  (let ((r (eval exp genv)))
    (write gevalnum)
    (write output-promt)
    (set! gevalnum (+ 1 gevalnum))
    (env-check-print r))
  (newline))

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
;; ;; test for the code
;; (geval '(define (foo x) (print 1) (+ x x)))
;; (geval '(foo (begin (print 2) 2)))
;; ;; test special for the 3rd situation
;; (geval '(define (foo x (y thunk) (z thunk-memoried)) (print 1)
;;           (+ x x y y z z)))
;; (geval '(foo (begin (print 2) 2) (begin (print 3) 3) (begin (print 4) 4)))
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
           (print "end of recursion ;P"))
          ((geval input)
           (driver-loop)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the stream control
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; check for the cons and car and cdr
;; (geval '(define a (cons 1 2)))
;; (geval 'a)
;; (geval '(car a))
;; (geval '(cdr a))
;; (geval '(define b (cons 3 a)))
;; (geval '(car (cdr b)))
;; (geval 'nil)
;; (geval '(null? nil))
;; (exit)

;; ;; construct the stream object
(geval '(define (stream-cons x (y thunk-memoried))
          (cons x y)))
;; ;; (print "end first")
(geval '(define ones (stream-cons 1 ones)))
;; (geval '(cdr ones))
;; (geval '(car (cdr ones)))
;; ;; (geval 'ones)
;; (exit)
;; ;; construct the int interval
(geval '(define (stream-int start end)
          (if (>= start end)
              (list)
              (stream-cons start (stream-int (+ start 1) end)))))
(geval '(define (stream-ref s n)
          (cond ((null? s)
                 (print (list 'end 'of 'the 'stream)))
                ((= n 0)
                 (car s))
                (else
                 (stream-ref (cdr s) (- n 1))))))
;; (geval '(define intval (stream-int 1 10000000)))
;; (geval '(car (cdr intval)))
;; (geval '(cdr (cdr intval)))
;; (geval '(null? (cdr (cdr intval))))
(geval '(define (stream-filter pre s)
          (cond ((null? s)
                 (list))
                (else
                 (let ((first (car s))
                       (second (cdr s)))
                   (cond ((pre first)
                          (stream-cons first (stream-filter pre second)))
                         (else
                          (stream-filter pre second))))))))
(geval '(define (add-stream s1 s2)
          (cond ((null? s1) (list))
                ((null? s2) (list))
                (else
                 (stream-cons (+ (car s1)
                                 (car s2))
                              (add-stream (cdr s1)
                                          (cdr s2)))))))

;; ;; the seise test
(geval '(define ints (stream-cons 2 (add-stream ones ints))))
;; (geval '(define (devide? d1 d2) (= (remainder d1 d2) 0)))
;; (geval '(define (seive str)
;;           (stream-cons (car str)
;;                        (seive (stream-filter (lambda (x)
;;                                                (not (devide? x (car str))))
;;                                              (cdr str))))))
;; (geval '(define prime (seive ints)))
;; ;; (driver-loop)
;; ;; (exit)

(geval '(define (stream-scale dt integrand)
          (define (inner-scale accumulate-dt in)
            (cond ((null? in) 0)
                  ((= accumulate-dt 0)
                   (stream-cons 0 (stream-scale dt in)))
                  (else
                   (let ((correct-stream (inner-scale (- accumulate-dt 1)
                                                      (cdr in))))
                     (stream-cons (+ (car in)
                                     (car correct-stream))
                                  (cdr correct-stream))))))
          (inner-scale dt integrand)))

;; (geval '(car (stream-scale 3 ones)))
;; (geval '(car (stream-scale 4 ones)))
(geval '(define (integral integrand init dt)
          (define int
            (stream-cons init
                         (add-stream (stream-scale dt integrand)
                                     int)))
          int))
(geval '(define int-ones (integral ones 0 2)))
;; (geval '(define scale-ints (stream-scale 2 ints)))
;; (geval '(define scale-ones (stream-scale 2 ones)))
;; (exit)
;; (driver-loop)
