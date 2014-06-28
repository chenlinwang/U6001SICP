(load "basic.scm")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.54
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Two lists are said to be equal? if they contain equal elements arranged in the same order.
;; For example,

;; (equal? '(this is a list) '(this is a list))

;; is true, but

;; (equal? '(this is a list) '(this (is a) list))

;; is false. To be more precise, we can define equal? recursively in terms of the basic eq? equality of symbols by saying that a and b are equal? if they are both symbols and the symbols are eq?, or if they are both lists such that (car a) is equal? to (car b) and (cdr a) is equal? to (cdr b). Using this idea, implement equal? as a procedure.

(define (own-list-equal? l1 l2)
    ;; Decide two lists to be equal. l1,l2:input list.
    ;; (list, list) -> (boolean)
    (define (ieq? l1 l2)
        ;; iteraly decide two lists to be equal. l1,l2:input list.
        ;; (list, list) -> (boolean)
        (if (null? l1)
            (if (null? l2) #t #f)
            (let ((e1 (car l1))
                  (r1 (cdr l1))
                  (e2 (car l2))
                  (r2 (cdr l2)))
              (cond ((and (symbol? e1) (symbol? e2)) (if (eq? e1 e2) (ieq? r1 r2) #f))
                    ((and (number? e1) (number? e2)) (if (= e1 e2) (ieq? r1 r2) #f))
                    ((and (list? e1) (list? e2)) (if (own-list-equal? e1 e2) (ieq? r1 r2) #f))
                    ((and (string? e1) (string? e2)) (if (equal? e1 e2) (ieq? r1 r2) #f))
                    (else #f)))))
  (if (= (length l1) (length l2))
      (ieq? l1 l2)
      #f))

;; ;; testing
;; (display (own-list-equal? '(a b c) '(a b c)))
;; (newline)
;; (display (own-list-equal? '(a b d) '(a b c)))
;; (newline)
;; (display (own-list-equal? (list 'a 'b 'c 1 2 3 "a" "b") (list 'a 'b 'c 1 2 3 "a" "b")))
;; (newline)
;; (display (own-list-equal? (list 'a '(a b) 'c) (list 'a '(a b) 'c)))
;; (newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.56
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Show how to extend the basic differentiator to handle more kinds of expressions. For instance, implement the differentiation rule on exponent.By adding a new clause to the deriv program and defining appropriate procedures exponentiation?, base,exponent, andmake-exponentiation. (You may use the symbol =**= to denote exponentiation.) Build in the rules that anything raised to the power 0 is 1 and anything raised to the power 1 is the thing itself.
;;loading what we already have.
(load "../BookImplementation/sec2.3.scm")

;; Primitives for exponent.
;; constructor
(define (make-exponentiation base exp)
    ;; make exponent out of base and exp.
    ;; (A,A) -> (list/number)
    (cond ((and (number? base) (number? exp)) (exponentiation base exp))
          ((and (number? exp) (= 0 exp)) 1)
          ((and (number? exp) (= 1 exp)) base)
          ((and (number? base) (= 0 base)) 0)
          ((and (number? base) (= 1 base)) 1)
          (else (list '** base exp))))

;;selector
(define base cadr)
(define exponent caddr)

;;operator
(define (exponentiation? expr)
    (and (expr? expr) (eq? '** (car expr))))


(define (d256 expr var)
  ;; To differentiate an expr within var. expr:expression to be differentiate; var:respect variable.
  ;; (list,symbol) -> (list)
  (cond ((number? expr) 0)
        ((symbol? expr) (if (same-symbol? expr var) 1 0))
        ((sum? expr) (make-sum (d256 (addend expr) var)
                               (d256 (augend expr) var)))
        ((product? expr) (make-sum (make-product (d256 (multiplier expr) var)
                                                 (multiplicant expr))
                                   (make-product (multiplier expr)
                                                 (d256 (multiplicant expr) var))))
        ((exponentiation? expr) (make-product (exponent expr) (make-product (make-exponentiation (base expr)
                                                                                                 (- (exponent expr) 1))
                                                                            (d256 (base expr) var))))
        (else (errormsg "Unrecongnized Symbols" expr))))

;; (display (d256 (list '** 'x 1) 'x))
;; (newline)
;; (display (d256 (list '** 'x 2) 'x))
;; (newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.57
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extend the differentiation program to handle sums and products of arbitrary numbers of (two or more) terms. Then the last example above could be expressed as

;; (deriv '(* x y (+ x 3)) 'x)

;; Try to do this by changing only the representation for sums and products, without changing the deriv procedure at all. For example, the addend of a sum would be the first term, and the augend would be the sum of the rest of the terms.

;; redo the constructor for sum and product.

;; Expr
(define (expr? expr) (and (list? expr) (>= (length expr) 3)))
(define (allsymbols? vl)
    ;; check whether expression is all symbols but the first one.
    ;; (list)->(boolean)
    (if (null? vl) #t (if (number? (car vl)) #f (allsymbols? (cdr vl)))))

;; Add up
(define (calculate-through vl init op sl op?)
    ;; calculate through a list. vlist: variable list; result:result of calculation; op: operator; slist: symbol list.
    ;; (list,number,(number,number)->(number),list) -> (list)
    (define (cc vlist result slist)
        (cond ((null? vlist) (cons result slist))
              ((number? (car vlist)) (cc (cdr vlist) (op result (car vlist)) slist))
              ((symbol? (car vlist)) (cc (cdr vlist) result (cons (car vlist) slist)))
              ((list? (car vlist)) (if (op? (car vlist))
                                              (cc (append (cdr (car vlist)) (cdr vlist)) result slist)
                                              (cc (cdr vlist) result
                                                  (cons (cond
                                                          ((sum? (car vlist)) (ms257 0 (car vlist)))
                                                          ((product? (car vlist)) (mp257 1 (car vlist))))
                                                        slist))))
              (else (errormsg "Variable list unrecognized!" vlist))))
  (cc vl init sl))

;; (display (calculate-through '(1 2 3 (+ 3 4 5) 4 5) 0 + (list) sum?))
;; (newline)

(define (ms257 e1 . e2)
    ;; make sum out of e1 and list e2.
    ;; (A,list) -> (list)
    (if (null? e2)
        e1
        (let ((resultlist (calculate-through (cons e1 e2) 0 + (list) sum?)))
          (let ((num (car resultlist))
                (vlist (cdr resultlist))
                (vlistlen (length (cdr resultlist))))
            (cond ((= num 0) (cond ((= vlistlen 0) 0)
                                   ((= vlistlen 1) (car vlist))
                                   (else (cons '+ vlist))))
                  ((= vlistlen 0) num)
                  (else (cons '+ resultlist)))))))

;; (display (ms257 1 (+ 1 2 3 4) 1))
;; (newline)

(define add257 cadr)
(define (aug257 sum)
    ;; Take the augend of a sum.
    ;; (list) -> (number/list)
    (cond ((= (length sum) 3) (caddr sum))
          ((allsymbols? (cddr sum)) (cons '+ (cddr sum))) ;;check if is all symbols, avoid infinite circling.
          (else (ms257 0 (cons '+ (cddr sum))))))

(define (mp257 e1 . e2)
    ;; make product out of e1 and list e2.
    ;; (A,list) -> (list/A)
    (if (null? e2)
        e1
        (let ((resultlist (calculate-through (cons e1 e2) 1 * (list) product?)))
          (let ((num (car resultlist))
                (vlist (cdr resultlist))
                (vlistlen (length (cdr resultlist))))
            (cond ((= num 0) 0)
                  ((= num 1) (cond ((= vlistlen 0) 1)
                                   ((= vlistlen 1) (car vlist))
                                   (else (cons '* vlist))))
                  ((= vlistlen 0) num)
                  (else (cons '* resultlist)))))))

(define mer257 add257)
(define (mnd257 sum)
    ;; Take the augend of a sum.
    ;; (list) -> (number/list)
    (cond ((= (length sum) 3) (caddr sum))
          ((allsymbols? (cddr sum)) (cons '* (cddr sum))) ;;check if is all symbols, avoid infinite circling.
          (else (mp257 1 (cons '* (cddr sum))))))

(define (d257 expr var)
  ;; To differentiate an expr within var. expr:expression to be differentiate; var:respect variable.
  ;; (list,symbol) -> (list)
  (cond ((number? expr) 0)
        ((symbol? expr) (if (same-symbol? expr var) 1 0))
        ((sum? expr) (ms257 (d257 (add257 expr) var)
                            (d257 (aug257 expr) var)))
        ((product? expr) (ms257 (mp257 (d257 (mer257 expr) var)
                                       (mnd257 expr))
                                (mp257 (mer257 expr)
                                       (d257 (mnd257 expr) var))))
        ((exponentiation? expr) (mp257 (exponent expr) (mp257 (make-exponentiation (base expr)
                                                                                   (- (exponent expr) 1))
                                                              (d257 (base expr) var))))
        (else (errormsg "Unrecongnized Symbols" expr))))

;; (display (d257 '(* x (+ 1 2 3 4)) 'x))
;; (newline)
;; (display (d257 '(* x y (+ x 3)) 'x))
;; (newline)
;; (display (d257 '(* x y (+ x 3 (+ 1 2 3)) (* x 2 3)) 'x))
;; (newline)
;; (display (d257 '(* x 2 (+ 1 2 3)) 'x))
;; (newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.58
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Suppose we want to modify the differentiation program so that it works with ordinary mathematical notation, in which + and * are infix rather than prefix operators. Since the differentiation program is defined in terms of abstract data, we can modify it to work with different representations of expressions solely by changing the predicates, selectors, and constructors that define the representation of the algebraic expressions on which the differentiator is to operate.

;; 1. Show how to do this in order to differentiate algebraic expressions presented in infix form, such as =(x + (3 * (x + (y + 2))))=. To simplify the task, assume that + and * always take two arguments and that expressions are fully parenthesized.
;; 2. The problem becomes substantially harder if we allow standard algebraic notation, such as =(x + 3 * (x + y + 2))=, which drops unnecessary parentheses and assumes that multiplication is done before addition. Can you design appropriate predicates, selectors, and constructors for this notation such that our derivative program still works?

;; 1. Change the start
;; Constructors
(define (make-sum258 e1 e2)
  ;; make a sum out of e1 and e2.e1:addend; e2:augend.
  ;; (A,A) -> (list/number)
  (cond ((and (number? e1) (number? e2)) (+ e1 e2))
        ((and (number? e1) (= e1 0)) e2)
        ((and (number? e2) (= e2 0)) e1)
        (else (list e1 '+ e2) )))

(define (make-product258 e1 e2)
  ;; make a product out of e1 and e2.e1:multiplier;e2:multiplicand.
  ;; (A,A) -> (list/number)
  (cond ((and (number? e1) (number? e2)) (* e1 e2))
        ((and (number? e1) (= e1 1)) e2)
        ((and (number? e1) (= e1 0)) 0)
        ((and (number? e2) (= e2 1)) e1)
        ((and (number? e2) (= e2 0)) 0)
        (else (list '* e1 e2) )))

;; Selectors
(define addend258 car)
(define augend258 caddr)
(define multiplier258 car)
(define multiplicant258 caddr)
;; The Operators
(define (sum?258 expr) (and (expr? expr) (eq? '+ (cadr expr))))
(define (product?258 expr) (and (expr? expr) (eq? '* (cadr expr))))

(define (differentiate258 expr var)
  ;; To differentiate an expr within var. expr:expression to be differentiate; var:respect variable.
  ;; (list,symbol) -> (list)
  (cond ((number? expr) 0)
        ((symbol? expr) (if (same-symbol? expr var) 1 0))
        ((sum?258 expr) (make-sum258 (differentiate258
                                      (addend258 expr) var)
                                     (differentiate258
                                      (augend258 expr) var)))
        ((product?258 expr) (make-sum258 (make-product258
                                          (differentiate258
                                           (multiplier258 expr)
                                           var)
                                          (multiplicant258 expr))
                                         (make-product258
                                          (multiplier258 expr)
                                          (differentiate258
                                           (multiplicant258 expr)
                                           var))))
        (else (begin (display "Error: unrecongnized symbol:\n")
                     (display "\t")
                     (display expr)
                     (newline)))))

;; Testing
;; (display (differentiate258 '(x + (3 * (x + (y + 2)))) 'x))
;; (newline)

;; 2 without unnecessary bracket
;; Constructor
(define (make-sum2582 e1 e2)
  ;; make a sum out of e1 and e2.e1:addend; e2:augend.
  ;; (A,A) -> (list/number)
  (cond ((and (number? e1) (number? e2)) (+ e1 e2))
        ((and (number? e1) (= e1 0)) e2)
        ((and (number? e2) (= e2 0)) e1)
        (else (list e1 '+ e2) )))

(define (make-product2582 e1 e2)
  ;; make a product out of e1 and e2.e1:multiplier;e2:multiplicand.
  ;; (A,A) -> (list/number)
  (cond ((and (number? e1) (number? e2)) (* e1 e2))
        ((and (number? e1) (= e1 1)) e2)
        ((and (number? e1) (= e1 0)) 0)
        ((and (number? e2) (= e2 1)) e1)
        ((and (number? e2) (= e2 0)) 0)
        (else (list e1 '* e2) )))

;; Selectors
(define addend2582 car)
(define (augend2582 expr)
    (if (= (length expr) 3)
        (caddr expr)
        (cddr expr)))
(define multiplier2582 car)
(define (multiplicant2582 expr)
  ;; To find the multiplicant until an addition appears
  ;; (list) -> (symbol/list)
  (if (or (= (length expr) 3)
          (sum?258 (cddr expr)))
      (multiplicant258 expr)
      (list (multiplicant258 expr)
            '*
            (multiplicant2582 (cddr expr)))))

;; ;; Test for multiplicant2582
;; (display (multiplicant2582 '(1 * 2 + 2)))
;; (newline)
;; (display (multiplicant2582 '(1 * 2)))
;; (newline)
;; (display (multiplicant2582 '(1 * 2 * 2 + 2)))
;; (newline)
;; (display (multiplicant2582 '(1 * 2 * 2)))
;; (newline)
;; (display (multiplicant2582 '(1 * 2 * 2 * 3 * 4)))
;; (newline)

(define (after-add expr)
  ;; To find the first appeared addition to the end.
  ;; (list) -> (number/symbol/list)
  (cond ((= (length expr) 3)
         (if (product?258 expr) 0 (caddr expr)))
        ((sum?258 (cddr expr)) (if (= (length (cddddr expr)) 1)
                                   (car (cddddr expr))
                                   (cddddr expr)))
        ((product?258 (cddr expr)) (after-add (cddr expr)))
        (else (errormsg "after-add error" expr))))
;; Testing for after-add
;; (display (after-add '(1 * 2 + 2)))
;; (newline)
;; (display (after-add '(1 * 2)))
;; (newline)
;; (display (after-add '(1 * 2 * 2 + 2 + 3)))
;; (newline)
;; (display (after-add '(1 * 2 * 2)))
;; (newline)
;; (display (after-add '(1 * 2 * 2 * 3 * 4 + 3 + 4 + 5)))
;; (newline)

;; Main function
(define (diff-norm expr var)
  ;; To differentiate normal order expressions
  ;; (list,symbol) -> (list)
  (cond ((number? expr) 0)
        ((symbol? expr) (if (same-symbol? expr var) 1 0))
        ((sum?258 expr) (make-sum2582 (diff-norm
                                       (addend2582 expr) var)
                                      (diff-norm
                                       (augend2582 expr) var)))
        ((product?258 expr) (make-sum2582
                             (make-product2582
                              (diff-norm
                               (multiplier2582 expr)
                               var)
                              (multiplicant2582 expr))
                             (make-sum2582
                              (make-product2582
                               (multiplier2582 expr)
                               (diff-norm
                                (multiplicant2582 expr)
                                var))
                              (diff-norm (after-add expr)
                                         var))))
        (else (errormsg "diff-norm" expr))))

;; ;; Test for diff-norm
;; (display (diff-norm '(x + 3 * (x + y + 2)) 'x))
;; (newline)
;; (display (diff-norm '(x * x + 3 * (x + y + 2) + 3 * y + 3 + 4) 'x))
;; (newline)


;; Unfinished ;; Bonus, To get rid of the brackets,
(define (get-two expr)
    ;; Get the first two element of the expr. expr: expression.
    ;; (list) -> (list)
    (list-get expr 0 2))

(define (get-unit expr)
    ;; Get a single element, or multiplied elements. expr: expression.
    ;; (list) -> (list/number/symbol)
    (define (it expr)
        ;; recursion process. expr: expression.
        ;; (list) -> (list/number/symbol)
        (cond ((= 1 (length expr)) expr)
              ((sum?258 expr) (list (addend258 expr)))
              (else (cons (addend258 expr) (cons '* (it (cddr expr)))))))
  (cond ((null? expr) expr)
        ((= 1 (length expr)) (car expr))
        ((= 3 (length expr)) (if (sum?258 expr)
                                 (addend258 expr)
                                 expr))
        ((sum?258 expr) (addend258 expr))
        (else (it expr))))

;; ;; Testing
;; (display (get-unit (list)))
;; (newline)
;; (display (get-unit '(1)))
;; (newline)
;; (display (get-unit '(1 * 1)))
;; (newline)
;; (display (get-unit '(1 + 1)))
;; (newline)
;; (display (get-unit '(1 * 1 + 1)))
;; (newline)
;; (display (get-unit '(1 * 1 * 1)))
;; (newline)

(define (get-rest expr)
    ;; Get the rest of the elements after the unit.expr: expression.
    ;; (list) -> (list/number/symbol)
    (let ((unit (get-unit expr))
          (total (length expr)))
      (if (number? unit)
          (if (= total 1) (list) (cddr expr))
          (let ((unitlen (length unit)))
            (if (= total unitlen)
                (list)
                (list-get expr (+ 2 unitlen) -1))))))

;; ;; Testing
;; (display (get-rest (list)))
;; (newline)
;; (display (get-rest '(1)))
;; (newline)
;; (display (get-rest '(1 * 1)))
;; (newline)
;; (display (get-rest '(1 + 1)))
;; (newline)
;; (display (get-rest '(1 * 1 + 1)))
;; (newline)
;; (display (get-rest '(1 * 1 * 1)))
;; (newline)


;; UNFINISHED
;; (define (del-bra expr)
;;     ;; Get rid of the brackets in expression. expr: the expressions input.
;;     ;; (list) -> (list)
;;     (cond ((product?258 expr)
;;            (let ((mer (multiplier2582 expr))
;;                  (mnt (multiplicant2582 expr))
;;                  (add (after-add expr)))
;;              (if (list? mer)
;;                  ;; get rid of the brackets of mer
;;                  (let ((mernb (del-bra mer)))
;;                    ;; put every unit of mernb into form with latter
;;                    (del-bra (append
;;                              (do ((left (get-rest mernb)
;;                                         (get-rest left))
;;                                   (unit (get-unit mernb)
;;                                         (get-unit left))
;;                                   (result (list)
;;                                           (append result
;;                                                   (list '+)
;;                                                   mnt
;;                                                   (list '*)
;;                                                   unit)))
;;                                  ((null? left) (cdr
;;                                                 (append result
;;                                                         (list '+)
;;                                                         mnt
;;                                                         (list '*)
;;                                                         unit)))))))
;;                  ;; if it is not a list. Only solve the first case, tired, do not want to continue.... have idea: test whether there are still brackets, if there are  change the order!
;;                  (do if the first is an element))))
;;           ((list? (car expr)) (append (del-bra (car expr))
;;                                       (del-bra (cdr expr))))
;;           ((sum?258 expr) (append (get-two expr)
;;                                   (del-bra (cddr expr))))
;;           ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sec 2.60
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; We specified that a set would be represented as a list with no duplicates. Now suppose we allow duplicates. For instance, the set {1,2,3} could be represented as the list (2 3 2 1 3 2 2). Design procedures =element-of-set?=, =adjoin-set=, =union-set=, and =intersection-set= that operate on this representation. How does the efficiency of each compare with the corresponding procedure for the non-duplicate representation? Are there applications for which you would use this representation in preference to the non-duplicate one?

;; Constructor
(define make-set list)

;; Operation
(define (element-of-set? e set)
       ;; Decide whether a element is in set.e:element;set:set
       ;; (number/symbol,list) -> (boolean)
       (cond ((null? set) #f)
             ((equal? e (car set)) #t)
             (else (element-of-set? e (cdr set)))))

(define adjoin-set cons)
(define union-set append)
(define (intersection-set s1 s2)
  ;; To interset two sets.s1:set 1;s2:set 2.
  ;; (list,list) -> (list)
  (if (null? s1)
      (list)
      (let ((e1 (car s1))
            (r1 (cdr s1)))
        (if (element-of-set? e1 s2)
            (cons e1 (intersection-set r1 s2))
            (intersection-set r1 s2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.6
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Give an implementation of adjoin-set using the ordered representation. By analogy with element-of-set? show how to take advantage of the ordering to produce a procedure that requires on the average about half as many steps as with the unordered representation.

;; Load the ordered settings from Book
(load "../BookImplementation/sec2.3")

(define (adjoin-order-set e s)
    ;; To insert an element into set. e:element;s:set.
    (let ((slen (length s))
          (half (inexact->exact (floor (/ (length s) 2)))))
      ;; See how many elements are left
      (cond ((= slen 0) (list e)) ;; None left
            ((= half 0) (let ((halfele (car s))) ;; One left
                          (cond ((= halfele e) s)
                                ((< halfele e) (list halfele e))
                                ((> halfele e) (cons e s)))))
            ;; More than two left
            (else
             (let ((halfele (list-ref s half)) ;; Get the middle element
                   (former (list-get s 0 half)) ;; Get the former part
                   (latter (list-get s (+ half 1) -1))) ;; Get the latter part
               (cond ((= halfele e) s)
                     ((> halfele e) (append
                                     (adjoin-order-set e former)
                                     (cons halfele latter)))
                     ((< halfele e) (append
                                     former
                                     (list halfele)
                                     (adjoin-order-set e latter)))))))))

;; ;; Testing
;; (define os1 (make-order-set 1 2 3 4 5))
;; (display (adjoin-order-set 1 os1))
;; (newline)
;; (display (adjoin-order-set 2 os1))
;; (newline)
;; (display (adjoin-order-set 3 os1))
;; (newline)
;; (display (adjoin-order-set 4 os1))
;; (newline)
;; (display (adjoin-order-set 5 os1))
;; (newline)
;; (define os2 (make-order-set 1 2 3 4))
;; (display (adjoin-order-set 1 os2))
;; (newline)
;; (display (adjoin-order-set 2 os2))
;; (newline)
;; (display (adjoin-order-set 3 os2))
;; (newline)
;; (display (adjoin-order-set 4 os2))
;; (newline)

;; (define os1 (make-order-set 2 3 4))
;; (display (adjoin-order-set 1 os1))
;; (newline)
;; (define os1 (make-order-set 1 3 4))
;; (display (adjoin-order-set 2 os1))
;; (newline)
;; (define os1 (make-order-set 1 2 4))
;; (display (adjoin-order-set 3 os1))
;; (newline)
;; (define os1 (make-order-set 1 2 3))
;; (display (adjoin-order-set 4 os1))
;; (newline)
;; (define os2 (make-order-set 2 3 ))
;; (display (adjoin-order-set 1 os2))
;; (newline)
;; (define os2 (make-order-set 1 3 ))
;; (display (adjoin-order-set 2 os2))
;; (newline)
;; (define os2 (make-order-set 1 2 ))
;; (display (adjoin-order-set 3 os2))
;; (newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.61
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Give a \Theta (n) implementation of union-set for sets represented as ordered lists.
(define (union-order-set s1 s2)
    ;; To union order set.s1: order set 1; s2: order set 2.
    ;; (list,list) -> (list)
    ;; See if any of sets are empty
    (cond ((null? s1) s2)
          ((null? s2) s1)
          ;; Both not empty, compare their first elements
          (else (let ((e1 (car s1))
                      (e2 (car s2)))
                  (cond ((< e1 e2)
                         (cons e1
                               (union-order-set (cdr s1) s2)))
                        ((> e1 e2)
                         (cons e2
                               (union-order-set s1 (cdr s2))))
                        ;; Equal, then just get one.
                        (else
                         (cons e1
                               (union-order-set (cdr s1)
                                                (cdr s2)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.63
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert tree to list
(define (tree->list-1 t)
    ;; version 1. t:tree.
    ;; (list) -> (list)
    (if (null? t)
        t
        (append (tree->list-1 (left-branch t))
                (list (entry t))
                (tree->list-1 (right-branch t))))
  ;; (begin (display "Enter function with tree:")
  ;;        (display t)
  ;;        (newline))
  )

;; Testing
;; (display (tree->list-1 tree1))
;; (newline)
;; (display (tree->list-1 t1))
;; (newline)
;; (display (tree->list-1 t2))
;; (newline)

(define (tree->list-2 t)
    ;; version 2. t: tree.
    ;; (list) -> (list)
    (define (copy-to-list t rl)
        ;; Copy the tree to the list.t:tree; rl:resultlist.
        ;; (list,list) -> (list)
        (if (empty-tree? t)
            rl
            (copy-to-list (left-branch t)
                          (cons (entry t)
                                (copy-to-list (right-branch t)
                                              rl)))))
  (copy-to-list t (list)))

;; ;; Testing
;; (display (tree->list-2 tree1))
;; (newline)


;; a. They do have the same result.
;; b. They are also the same order or growth.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.64
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following procedure =list->tree= converts an ordered list to a balanced binary tree. The helper procedure =partial-tree= takes as arguments an integer n and list of at least n elements and constructs a balanced tree containing the first n elements of the list. The result returned by =partial-tree= is a pair (formed with cons) whose car is the constructed tree and whose cdr is the list of elements not included in the tree.

(define (list->tree l)
    ;; Convert a list to a tree.l:list.
    ;; (list) -> (list)
    (car (partial-tree l (length l))))

(define (partial-tree l n)
    ;; Convert the first n members of l to a balanced tree, and concatenate it with a list of the rest members. l:list; n: number between 0 and length of l.
    ;; (list,number) -> (list)
    (if (= n 0)
        (cons (list) l)
        (let ((left-length (quotient n 2)))
          (let ((left-parts (partial-tree l left-length)))
            (let ((left-tree (car left-parts))
                  (rest-member (cdr left-parts))
                  (right-length (- n (+ left-length 1))))
              (let ((node (car rest-member))
                    (right-parts (partial-tree
                                  (cdr rest-member)
                                  right-length)))
                (cons (make-tree node
                                 left-tree
                                 (car right-parts))
                      (cdr right-parts))))))))

;; ;; Test
;; (display (list->tree (list 1 2 3 4 5 6 7 8 9)))
;; (newline)

;;1. The partial-tree works in the following way: Given a list and a number, it first see whether the number is 0. If it is 0, then just return a pair of an empty list and the list. If not, it then figures out the left subtree size, and then use recursion to get the pair of the left subtree and the remaining elements. After that, it calculates the right subtree size and then use the recursion to get the responding pair. Make a tree out of subtrees and return a pair of them and rest members.

;; 2. \Theta (\log n)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.63
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use the results of exercises 2.63 and 2.64 to give \Theta (n) implementations of =union-set= and =intersection-set= for sets implemented as (balanced) binary trees.

(define (union-tree t1 t2)
    ;; To union two trees. t1: tree 1; t2:tree 2.
    ;; (list,list) -> (list)
    (let ((tl1 (tree->list-1 t1))) ;; Convert tree 1 to a list
      (let iter ((left tl1) ;; Initialize left to be tree list
                 (currenttree t2)) ;; Initialize result tree to be tree 2
           (if (null? left)
               currenttree
               (iter (cdr left)  ;; Get rest of left member
                     (adjoin-tree (car left) ;; Join element to reuslt tree
                                  currenttree))))))

(define (intersect-tree t1 t2)
    ;; To intersect two trees. t1: tree 1; t2:tree 2.
    ;; (list,list) -> (list)
    (let ((tl1 (tree->list-1 t1))) ;; Convert tree 1 and to a list
      (let iter ((left tl1) ;; Initialize left1 to be tree list 1
                 (currenttree empty-tree)) ;; Initialize result tree to be empty tree.
           (if (null? left)
               currenttree
               (let ((first (car left)) ;; Get first element of left.
                     (restleft (cdr left)))
                 (if (element-of-tree? first t2) ;; See whether tree 2 contains it.
                     (iter restleft
                           (adjoin-tree first currenttree))
                     (iter restleft currenttree)))))))

;; ;; Test
;; (define t1 (list->tree (list 1 2 3 4 5)))
;; (define t2 (list->tree (list 3 4 5 6 7)))
;; (display (tree->list-1 (union-tree t1 t2)))
;; (newline)
;; (display (tree->list-1 (intersect-tree t1 t2)))
;; (newline)


(define (union-tree2 t1 t2)
    ;; n verion
    (let ((l1 (tree->list-1 t1))
          (l2 (tree->list-1 t2)))
      (list->tree (union-order-set l1 l2))))

(define (intersect-tree2 t1 t2)
    ;; n version
    (let ((l1 (tree->list-1 t1))
          (l2 (tree->list-1 t2)))
      (list->tree (intersect-order-set l1 l2))))

;; ;; Test
;; (define t1 (list->tree (list 1 2 3 4 5)))
;; (define t2 (list->tree (list 3 4 5 6 7)))
;; (display (tree->list-1 (union-tree2 t1 t2)))
;; (newline)
;; (display (tree->list-1 (intersect-tree2 t1 t2)))
;; (newline)
