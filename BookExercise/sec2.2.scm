;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.17
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define a procedure last-pair that returns the list that contains only the last element of a given (nonempty) list.

(define (last-ele l)
    ;; Return the last element of a list. l: the list
    ;; (list) -> (A)
    (if (null? (cdr l))
        (car l)
        (last-ele (cdr l))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.18
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define a procedure reverse that takes a list as argument and returns a list of the same elements in reverse order.
(define (list-reverse l)
    ;;Return the reversed list of input.l:input list.
    ;; (list) -> (list)
    (if (null? l)
        (list)
        ;; append the rest of list infront of the first element.
        (append (list-reverse (cdr l)) (list (car l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.19
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Consider the =change-counting= program of section 1.2.2. It would be nice to be able to easily change the currency used by the program, so that we could compute the number of ways to change a British pound, for example. As the program is written, the knowledge of the currency is distributed partly into the procedure =first-denomination= and partly into the procedure =count-change= (which knows that there are five kinds of U.S. coins). It would be nicer to be able to supply a list of coins to be used for making change.
;; We want to rewrite the procedure =cc= so that its second argument is a list of the values of the coins to use rather than an integer specifying which coins to use. We could then have lists that defined each kind of currency:

;; (define us-coins (list 50 25 10 5 1))
;; (define uk-coins (list 100 50 20 10 5 2 1 0.5))

;; We could then call cc as follows:

;; (cc 100 us-coins)
;; 292

;; To do this will require changing the program cc somewhat. It will still have the same form, but it will access its second argument differently, as follows:

;; (define (cc amount coin-values)
;;   (cond ((= amount 0) 1)
;;         ((or (< amount 0) (no-more? coin-values)) 0)
;;         (else
;;          (+ (cc amount
;;                 (except-first-denomination coin-values))
;;             (cc (- amount
;;                    (first-denomination coin-values))
;;                 coin-values)))))

;; Define the procedures first-denomination, except-first-denomination, and no-more?
;; in terms of primitive operations on list structures. Does the order of the list coin-values affect the answer produced by cc? Why or why not?
(define (no-more? l)
    ;; To see the money list is empty. l: money list.
    ;; (list) -> (boolean)
    (null? l))

(define (except-first-denomination l)
    ;; Delete the first number of money. l: money list.
    ;; (list) -> (list)
    (cdr l))

(define (first-denomination l)
    ;; Give the first money number. l: money list.
    ;; (list) -> (number)
    (car l))

(define (cc amount coin-values)
    ;; To see count the ways to change certain amount of money. amount: the money amount; coin-values: money list.
    ;;(number,list)-> (number)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

;; (define us-coins (list 50 25 10 5 1))
;; (define us-coins-r (list-reverse us-coins))
;; (display (cc 100 us-coins))
;; (newline)
;; (display (cc 100 us-coins-r))
;; (newline)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.20
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The procedures +, *, and =list= take arbitrary numbers of arguments. One way to define such procedures is to use =define= with =dotted-tail notation=. In a procedure definition, a parameter list that has a dot before the last parameter name indicates that, when the procedure is called, the initial parameters (if any) will have as values the initial arguments, as usual, but the final parameter's value will be a list of any remaining arguments. For instance, given the definition

;; (define (f x y . z) <body>)

;; the procedure =f= can be called with two or more arguments. If we evaluate

;; (f 1 2 3 4 5 6)

;; then in the body off, x will be 1, y will be 2, and z will be the list (3 4 5 6). Given the definition

;; (define (g . w) <body>)

;; the procedure g can be called with zero or more arguments. If we evaluate

;; (g 1 2 3 4 5 6)

;; then in the body of g,w will be the list (1 2 3 4 5 6).
;; Use this notation to write a procedure =same-parity= that takes one or more integers and returns a list of all the arguments that have the same even-odd parity as the first argument. For example,

;; (same-parity 1 2 3 4 5 6 7)
;; (1 3 5 7)
;; (same-parity 2 3 4 5 6 7)
;; (2 4 6)

(define (same-parity x . y)
  ;; Return a list of numbers that have the same parity of the x. x: first input number;y:the rest of number
  ;; (number . number) -> (list)
  (define x-remainder (remainder x 2))
  (define (test-parity z)
    ;; Test the parity of z is the same to x. z: input number
    ;; (number) -> (boolean)
    (= (remainder z 2) x-remainder))
  (do ((candidlist y (cdr candidlist))
       (test-element (car y) (car candidlist))
       (paritylist (list x) (if (test-parity test-element)
                                (append paritylist (list test-element))
                                paritylist)))
      ((null? candidlist) paritylist)))

;; (display (same-parity 1 3 5 4 6 8))
;; (newline)
;; (display (same-parity 2 4 3 5 6 9))
;; (newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.21
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The procedure =square-list= takes a list of numbers as argument and returns a list of the squares of those numbers.

;; (square-list (list 1 2 3 4))
;; (1 4 9 16)

;; Here are two different definitions of =square-list=. Complete both of them by filling in the missing expressions:

;; (define (square-list items)
;;   (if (null? items)
;;       nil
;;       (cons <??> <??>)))
;; (define (square-list items)
;;   (map <??> <??>))

(define (square x) (* x x))

(define (square-list1 items)
  ;; Return a list of number that is square of the input. items: the input list.
  ;; (list) -> (list)
  (if (null? items)
      (list)
      (cons (square (car items)) (square-list1 (cdr items)))))

(define (square-list2 items)
    ;; Return a list of number that is square of the input. items: the input list.
  ;; (list) -> (list)
  (map square items))

;; (display (square-list1 (list 1 2 3 4)))
;; (newline)
;; (display (square-list2 (list 1 2 3 4)))
;; (newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.23
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The procedure =for-each= is similar to =map=. It takes as arguments a procedure and a list of elements. However, rather than forming a list of the results, =for-each= just applies the procedure to each of the elements in turn, from left to right. The values returned by applying the procedure to the elements are not used at all -- =for-each= is used with procedures that perform an action, such as printing. For example,

;; (for-each (lambda (x) (newline) (display x))
;;           (list 57 321 88))
;; 57 321 88

;; The value returned by the call to for-each (not illustrated above) can be something arbitrary, such as true. Give an implementation of =for-each=.

(define (own-foreach func l)
  ;; Self defined foreach.Apply func to  each element of l. func: the procedure;l: list of input element.
  ;; ((A->B),list) -> nil
  (do ((inputlist l (cdr inputlist))
       (output #f (func (car inputlist))))
      ((null? inputlist))))

;; (own-foreach (lambda (x) (display x) (newline)) (list 1 2 3 4 5))

(define (own-foreach2 func l)
  ;; Self defined foreach.Apply func to  each element of l. func: the procedure;l: list of input element.
  ;; ((A->B),list) -> nil
  (do ((x l (cdr x))
       (output #f (func (car x))))
      ((null? x)))
)

(own-foreach2 (lambda (x) (display x) (newline)) (list 1 2 3))
