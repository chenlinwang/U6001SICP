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

(define us-coins (list 50 25 10 5 1))
(define us-coins-r (list-reverse us-coins))
(display (cc 100 us-coins))
(newline)
(display (cc 100 us-coins-r))
(newline)
