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

;; (own-foreach2 (lambda (x) (display x) (newline)) (list 1 2 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.25
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Give combinations of cars and cdrs that will pick 7 from each of the following lists:
(define t2251 (list 1 3 (list 5 7) 9))
(define t2252 (list (list 7)))
(define t2253 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

;; (display (cadr (car (cdr (cdr t2251)))))
;; (newline)
;; (display (car (car t2252)))
;; (newline)
;; (display (cadr (cadr (cadr (cadr (cadr (cadr t2253)))))))
;; (newline)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.26
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Suppose we define x and y to be two lists:

;; (define x (list 1 2 3))
;; (define y (list 4 5 6))

;; What result is printed by the interpreter in response to evaluating each of the following expressions:

;; (append x y)
;; (cons x y)
;; (list x y)
(define e226x (list 1 2 3))
(define e226y (list 4 5 6))

;; (display (append e226x e226y))
;; (newline)
;; (display (cons e226x e226y))
;; (newline)
;; (display (list e226x e226y))
;; (newline)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.27
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modify your =reverse= procedure of exercise 2.18 to produce a =deep-reverse= procedure that takes a list as argument and returns as its value the list with its elements reversed and with all sublists deep-reversed as well. For example,

;; (define x (list (list 1 2) (list 3 4)))
;; x
;; ((1 2) (3 4))
;; (reverse x)
;; ((3 4) (1 2))
;; (deep-reverse x)
;; ((4 3) (2 1))

(define (deep-reverse l)
  ;; Reverse all lists inside l. l: tree structured list.
  ;; (list) -> (list)
  (list-reverse (map (lambda (l)
                       (if (list? l) ;; test whether input is a list
                           (deep-reverse l);; yes, then deep reverse it
                           l;; no, then simple return it
                           ))
                     l)))

;; (define t227 (list (list 1 2) (list 3 4)))
;; (display (deep-reverse t227))
;; (newline)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.28
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write a procedure =fringe= that takes as argument a tree (represented as a list) and returns a list whose elements are all the leaves of the tree arranged in left-to-right order. For example,

;; (define x (list (list 1 2) (list 3 4)))
;; (fringe x)
;; (1 2 3 4)
;; (fringe (list x x))
;; (1 2 3 4 1 2 3 4)

(define (fringe l)
  ;; Return a list of lieves of the input. l: tree structures list.
  ;; (list) -> (list)
  (cond ((null? l) l)
        ((not (pair? l)) (list l))
        (else (append (fringe (car l)) (fringe (cdr l))))))

;; (define t228 (list (list 1 2) (list 3 4)))
;; (display (fringe t228))
;; (newline)
;; (display (fringe (list t228 t228)))
;; (newline)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.29
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A binary mobile consists of two branches, a left branch and a right branch. Each branch is a rod of a certain length, from which hangs either a weight or another binary mobile. We can represent a binary mobile using compound data by constructing it from two branches (for example, using list):

;; (define (make-mobile left right)
;;   (list left right))

;; A branch is constructed from a length (which must be a number) together with a structure, which may be either a number (representing a simple weight) or another mobile:

;; (define (make-branch length structure)
;;   (list length structure))

;; 1. Write the corresponding selectors left-branch and right-branch, which return the branches of a mobile, and branch-length and branch-structure, which return the components of a branch.
;; 2. Using your selectors, define a procedure total-weight that returns the total weight of a mobile.
;; 3. A mobile is said to be balanced if the torque applied by its top-left branch is equal to that applied by its top-right branch (that is, if the length of the left rod multiplied by the weight hanging from that rod is equal to the corresponding product for the right side) and if each of the submobiles hanging off its branches is balanced. Design a predicate that tests whether a binary mobile is balanced.
;; 4. Suppose we change the representation of mobiles so that the constructors are as following.How much do you need to change your programs to convert to the new representation?

;; (define (make-mobile left right)
;;   (cons left right))
;; (define (make-branch length structure)
;;   (cons length structure))

;; 0
(define (make-mobile left right)
  ;; Constructor of a mobile. left: left branch; right: right branch.
  ;; (list,list) -> (list)
  (list left right))

(define (make-branch length structure)
  ;; Constructor of a branch. length: length of the branch; structure: structure of the branch, can be a weight(number) or a mobile.
  ;; (number,number or list) -> (list)
  (list length structure))

;; 1
(define (left-branch m)
  ;; Selector of left branch of a mobile. m: a mobile.
  ;; (list) -> (list)
  (car m))

(define (right-branch m)
  ;; Selector of right branch of a mobile. m: a mobile.
  ;; (list) -> (list)
  (cadr m))

(define (branch-length b)
  ;; Selector of length of a branch. b: a branch.
  ;; (list) -> (number)
  (car b))

(define (branch-structure b)
  ;; Selector of structure of a branch. b: a branch.
  ;; (list) -> (number or list)
  (cadr b))

;; 1 testing
(define t229-1-b1 (make-branch 1 2))
(define t229-1-b2 (make-branch 3 4))

(define t229-1-m1 (make-mobile t229-1-b1 t229-1-b2))

(define t229-1-b3 (make-branch 5 t229-1-m1))
(define t229-1-m2 (make-mobile t229-1-b1 t229-1-b3))

;; (display (branch-length t229-1-b1))
;; (newline)
;; (display (branch-structure t229-1-b3))
;; (newline)
;; (display (left-branch t229-1-m1))
;; (newline)
;; (display (right-branch t229-1-m2))
;; (newline)

;; 2
(define (total-weight m)
  ;; Calculate the total weight of a mobile.m: the mobile.
  ;; (list) -> (number)
  (let ((lb (left-branch m))
        (rb (right-branch m)))
    (let ((ls (branch-structure lb))
          (rs (branch-structure rb)))
      (+ (if (list? ls)
             (total-weight ls)
             ls)
         (if (list? rs)
             (total-weight rs)
             rs))
      )))

;; 2 testing
;; (display (total-weight t229-1-m1))
;; (newline)
;; (display (total-weight t229-1-m2))
;; (newline)

;; 3
(define (torque b left?)
  ;; Calculate the torque of the branch. b: input branch; left?: whether this branch is left side of the mobile
  ;; (list) -> (number)
  (define (titer b len)
    ;; iteratively calculae the torq of the branch. b: input branch; len: the accumulative length at the branch.
    ;; (list, number, boolean) -> (number)
    (let ((st (branch-structure b)))
      (if (list? st)
          (let ((lb (left-branch st))
                (rb (right-branch st)))
            (+ (titer lb (- len (branch-length lb)))
               (titer rb (+ len (branch-length rb)))))
          (* len st))))
  (titer b ((if left? - +) (branch-length b))))

(define (balanced? m)
  ;; To see whether the mobile is balanced. m: input mobile.
  ;; (list) -> (boolean)
  (= 0 (+ (torque (left-branch m) #t) (torque (right-branch m) #f))))

;; 3 testing
;; (display (torque t229-1-b1 #f))
;; (newline)
;; (display (torque t229-1-b2 #f))
;; (newline)
;; (display (torque t229-1-b3 #f))
;; (newline)
;; (display (torque t229-1-b1 #t))
;; (newline)
;; (display (torque t229-1-b2 #t))
;; (newline)
;; (display (torque t229-1-b3 #t))
;; (newline)
;; (display t229-1-m1)
;; (newline)
;; (display (balanced? t229-1-m1))
;; (newline)
;; (display t229-1-m2)
;; (newline)
;; (display (balanced? t229-1-m2))
;; (newline)

;; 4
(define (make-mobile2 left right)
  (cons left right))
(define (make-branch2 length structure)
  (cons length structure))

;; 1
(define (left-branch2 m)
  ;; Selector of left branch of a mobile. m: a mobile.
  ;; (list) -> (list)
  (car m))

(define (right-branch2 m)
  ;; Selector of right branch of a mobile. m: a mobile.
  ;; (list) -> (list)
  (cdr m))

(define (branch-length2 b)
  ;; Selector of length of a branch. b: a branch.
  ;; (list) -> (number)
  (car b))

(define (branch-structure2 b)
  ;; Selector of structure of a branch. b: a branch.
  ;; (list) -> (number or list)
  (cdr b))

;; 1 testing
(define t229-1-b1 (make-branch2 1 2))
(define t229-1-b2 (make-branch2 3 4))

(define t229-1-m1 (make-mobile2 t229-1-b1 t229-1-b2))

(define t229-1-b3 (make-branch2 5 t229-1-m1))
(define t229-1-m2 (make-mobile2 t229-1-b1 t229-1-b3))

;; (display (branch-length2 t229-1-b1))
;; (newline)
;; (display (branch-structure2 t229-1-b3))
;; (newline)
;; (display (left-branch2 t229-1-m1))
;; (newline)
;; (display (right-branch2 t229-1-m2))
;; (newline)

;; 2
(define (total-weight2 m)
  ;; Calculate the total weight of a mobile.m: the mobile.
  ;; (list) -> (number)
  (let ((lb (left-branch2 m))
        (rb (right-branch2 m)))
    (let ((ls (branch-structure2 lb))
          (rs (branch-structure2 rb)))
      (+ (if (pair? ls)
             (total-weight2 ls)
             ls)
         (if (pair? rs)
             (total-weight2 rs)
             rs))
      )))

;; 2 testing
;; (display (total-weight2 t229-1-m1))
;; (newline)
;; (display (total-weight2 t229-1-m2))
;; (newline)

;; 3
(define (torque2 b left?)
  ;; Calculate the torque of the branch. b: input branch; left?: whether this branch is left side of the mobile
  ;; (list) -> (number)
  (define (titer b len)
    ;; iteratively calculae the torq of the branch. b: input branch; len: the accumulative length at the branch.
    ;; (list, number, boolean) -> (number)
    (let ((st (branch-structure2 b)))
      (if (pair? st)
          (let ((lb (left-branch2 st))
                (rb (right-branch2 st)))
            (+ (titer lb (- len (branch-length2 lb)))
               (titer rb (+ len (branch-length2 rb)))))
          (* len st))))
  (titer b ((if left? - +) (branch-length2 b))))

(define (balanced?2 m)
  ;; To see whether the mobile is balanced. m: input mobile.
  ;; (list) -> (boolean)
  (= 0 (+ (torque2 (left-branch2 m) #t) (torque2 (right-branch2 m) #f))))
;; 3 testing
;; (display (torque2 t229-1-b1 #f))
;; (newline)
;; (display (torque2 t229-1-b2 #f))
;; (newline)
;; (display (torque2 t229-1-b3 #f))
;; (newline)
;; (display (torque2 t229-1-b1 #t))
;; (newline)
;; (display (torque2 t229-1-b2 #t))
;; (newline)
;; (display (torque2 t229-1-b3 #t))
;; (newline)
;; (display t229-1-m1)
;; (newline)
;; (display (balanced?2 t229-1-m1))
;; (newline)
;; (display t229-1-m2)
;; (newline)
;; (display (balanced?2 t229-1-m2))
;; (newline)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.30
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define a procedure =square-tree= analogous to the =square-list= procedure of exercise 2.21. That is, =square-list= should behave as follows:

;; (square-tree
;;   (list 1
;;        (list 2 (list 3 4) 5)
;;        (list 6 7)))
;; (1 (4 (9 16) 25) (36 49))

;; Define square-tree both directly (i.e., without using any higher-order procedures) and also by using map and recursion.

;;Without map
(define (square-tree1 t)
  ;; Square the element of tree. t: a tree-structured list.
  ;; (list) -> (list)
  (cond ((null? t) (list))
        ((not (pair? t)) (square t))
        (else (cons (square-tree1 (car t)) (square-tree1 (cdr t))))))

(define t230 (list 1
                   (list 2 (list 3 4) 5)
                   (list 6 7)))

;; (display (square-tree1 t230))
;; (newline)

(define (square-tree2 t)
  ;; Square the element of tree. t: a tree-structured list.
  ;; (list) -> (list)
  (map (lambda (subt)
         (if (pair? subt)
             (square-tree2 subt)
             (square subt)))
       t))

;; (display (square-tree2 t230))
;; (newline)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.31
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Abstract your answer to exercise 2.30 to produce a procedure tree-map with the property that square-tree could be defined as

;; (define (square-tree tree) (tree-map square tree))

(define (tree-map func t)
  ;; A map-like tree map to deal with tree. func: the applied function; t: tree-like list.
  ;; (list) -> (list)
  (define (miter t)
    ;; Iterative function to carry on. t: tree-like list.
    ;; (list) -> (list)
    (map (lambda (subt)
           (if (pair? subt)
               (miter subt)
               (func subt)))
         t))
  (miter t))

(define square-tree3 (lambda (x) (tree-map square x)))

;; (display (square-tree3 t230))
;; (newline)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.32
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; We can represent a set as a list of distinct elements, and we can represent the set of all subsets of the set as a list of lists. For example, if the set is (1 2 3), then the set of all subsets is (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)). Complete the following definition of a procedure that generates the set of subsets of a set and give a clear explanation of why it works:

;; (define (subsets s)
;;   (if (null? s)
;;       (list nil)
;;       (let ((rest (subsets (cdr s))))
;; (append rest (map <??> rest)))))

;; (define (subsets s)
;;   (if (null? s)
;;       (begin (display "\nEmpty Set!\n")
;;              (list (list)))
;;       (let ((rest (subsets (cdr s))))
;;         (begin (display "\nHead: ")
;;                (display (car s))
;;                (display " - ")
;;                (display "With Set: ")
;;                (display rest)
;;                (newline)
;;                (append rest (map (lambda (news)
;;                                    (cons (car s) news))
;;                                  rest))))))

;; (display (subsets (list 1)))
;; (newline)
;; (display (subsets (list 1 2)))
;; (newline)

(define (subsets s)
  (if (null? s)
      (list (list))
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (news)
                            (cons (car s) news))
                          rest)))))

;; (define t232 (list 1 2 3))
;; (display (subsets t232))
;; (newline)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.33
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fill in the missing expressions to complete the following definitions of some basic list- manipulation operations as accumulations:

;; (define (map p sequence)
;;   (accumulate (lambda (x y) <??>) nil sequence))
;; (define (append seq1 seq2)
;;   (accumulate cons <??> <??>))
;; (define (length sequence)
;;   (accumulate <??> 0 sequence))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (own-map p sequence)
  (accumulate cons (list) sequence))

;; (display (own-map square (list 1 2 3)))
;; (newline)

(define (own-append seq1 seq2)
  (accumulate cons seq2 seq1))

;; (display (own-append (list 1 2 3) (list 4 5 6)))
;; (newline)

(define (own-length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

;; (display (own-length (list 1 2 3 4)))
;; (newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.34
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evaluating a polynomial in x at a given value of x can be formulated as an accumulation. We evaluate the polynomial, using a well-known algorithm called Horner's rule, which structures the computation as
;; : ( \dots (a_{n} x + a_{n-1})x \dots )x + a_0
;; In other words, we start with a_{n}, multiply by x, add a_{n-1}, multiply by x, and so on, until we reach a_0. Fill in the following template to produce a procedure that evaluates a polynomial using Horner's rule. Assume that the coefficients of the polynomial are arranged in a sequence, from a0 through an.

;; (define (horner-eval x coefficient-sequence)
;;   (accumulate (lambda (this-coeff higher-terms) <??>) 0
;;               coefficient-sequence))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms))) 0
              coefficient-sequence))

;; (display (horner-eval 2 (list 1 3 0 5 0 1)))
;; (newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.35
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Redefine count-leaves from section 2.2.2 as an accumulation:
(load "../BookImplementation/sec1.2")
(load "../BookImplementation/sec2.2")

(define (count-leaves235 t)
  (accumulate (lambda (x y) (+ 1 y)) 0 (enumerate-tree t)))

;; (define t235 (list 1 2 3 (list 4 5 6) (list 7 8 (list 9))))
;; (display (count-leaves235 t235))
;; (newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.36
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The procedure =accumulate-n= is similar to =accumulate= except that it takes as its third argument a sequence of sequences, which are all assumed to have the same number of elements. It applies the designated accumulation procedure to combine all the first elements of the sequences, all the second elements of the sequences, and so on, and returns a sequence of the results. For instance, if s is a sequence containing four sequences, ((1 2 3) (4 5 6) (7 8 9) (10 11 12)), then the value of (accumulate-n + 0 s) should be the sequence (22 26 30). Fill in the missing expressions in the following definition of accumulate-n:

;; (define (accumulate-n op init seqs)
;;   (if (null? (car seqs))
;; ￼￼￼￼
;; nil
;; (cons (accumulate op init <??>)
;; (accumulate-n op init <??>))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      (list)
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;; (define t236 (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
;; (display (accumulate-n + 0 t236))
;; (newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.37
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Suppose we represent vectors v = (vi) as sequences of numbers, and matrices m = (mij) as sequences of vectors (the rows of the matrix). For example, the matrix is represented as the sequence ((1 2 3 4) (4 5 6 6) (6 7 8 9)). With this representation, we can use sequence operations to concisely express the basic matrix and vector operations. These operations (which are described in any book on matrix algebra) are the following.
;; We can define the dot product as:

;; (define (dot-product v w)
;;   (accumulate + 0 (map * v w)))

;; Fill in the missing expressions in the following procedures for computing the othermatrix operations. (The procedure accumulate-n is defined in exercise 2.36.)

;; (define (matrix-*-vector m v)
;;   (map <??> m))

;; (define (transpose mat)
;;   (accumulate-n <??> <??> mat))

;; (define (matrix-*-matrix m n) (let ((cols (transpose n)))
;;   (map <??> m)))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(define (transpose mat)
  (accumulate-n  cons (list) mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector n x)) m)))

;; (define t237v (list 1 2 3 4))
;; (define t237w (list 1 1 1 1))
;; (define t237m (list (list 1 1 1 1)
;;                     (list 0 1 0 0)
;;                     (list 0 0 0 0)
;;                     (list 0 0 1 0)
;;                     (list 1 1 1 1)))

;; (define t237n (list t237w t237w t237w))

;; (display (dot-product t237v t237w))
;; (newline)
;; (display (matrix-*-vector t237m t237v))
;; (newline)
;; (display (matrix-*-matrix t237m t237n))
;; (newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.38
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The accumulate procedure is also known as fold-right, because it combines the
;; first element of the sequence with the result of combining all the elements to the right. There is also a fold- left, which is similar to fold-right, except that it combines elements working in the opposite direction:

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;; What are the values of
(define fold-right accumulate)

;; (display (fold-right / 1 (list 1 2 3)))
;; (newline)
;; (display (fold-left / 1 (list 1 2 3)))
;; (newline)
;; (display (fold-right list (list) (list 1 2 3)))
;; (newline)
;; (display (fold-left list (list) (list 1 2 3)))
;; (newline)

;; Give a property that op should satisfy to guarantee that fold-right and fold-left will produce the same values for any sequence.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.39
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Complete the following definitions of reverse (exercise 2.18) in terms of fold-right and fold-left from exercise 2.38:

(define (reverse2391 sequence)
    (fold-right (lambda (x y) (append y (list x))) (list) sequence))

(define (reverse2392 sequence)
    (fold-left (lambda (x y) (cons y x)) (list) sequence))

;; (define l239 (list 1 2 3 4))
;; (display (reverse2391 l239))
;; (newline)
;; (display (reverse2392 l239))
;; (newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nesting Map
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (flatmap proc seq)
      (accumulate append (list) (map proc seq)))

(define (ordered-pair n)
    ;; Find all ordered distinctive pair that less or equal to n. n: number > 1.
    ;; (number) -> (list)
    (flatmap (lambda (i)
               (map (lambda (j) (list j i))
                    (enumerate-range (- i 1))))
             (enumerate-interval 2 n)))

;; (display (ordered-pair 5))
;; (newline)
(define (iter-pair i) (map (lambda (j) (list j i))
                (enumerate-range (- i 1))))
;; (display (map iter-pair (enumerate-interval 2 3)))
;; (newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.41
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write a procedure to find all ordered triplets of distinct positive integers i, j, and k less than or equal to a given integer n that sum to a given integer s.
(define (ordered-triplet n)
    ;; Return ordered distinctice triplet that are lest or equal to n. n: number > 2
    ;;(number) -> (list)
    (flatmap (lambda (i)
               (let ((lp (ordered-pair (- i 1))))
                 (map (lambda (p) (append p (list i)))
                      lp)))
             (enumerate-interval 3 n)))

(define (iter-triplet i)
    (let ((lp (ordered-pair (- i 1))))
      (map (lambda (p) (append p (list i)))
           lp)))

;; (display (iter-triplet 3))
;; (newline)

;; (display (ordered-triplet 5))
;; (newline)
(define (triplet-sum-of n s)
    ;; Find all ordered distinctive triplets that sums up to s.n: number; s: number.
    ;; (number,number) -> (list)
    ((filterout (lambda (t) (= s (+ (car t) (cadr t) (caddr t)))))
     (ordered-triplet n)))

;; (display (triplet-sum-of 5 10))
;; (newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.42
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The ``eight-queens puzzle'' asks how to place eight queens on a chessboard so that no queen is in check from any other (i.e., no two queens are in the same row, column, or diagonal). One possible solution is shown in figure 2.8. One way to solve the puzzle is to work across the board, placing a queen in eachcolumn. Once we have placed k - 1 queens, we must place the kth queen in a position where it does not check any of the queens already on the board. We can formulate this approach recursively: Assume that we have already generated the sequence of all possible ways to place k - 1 queens in the first k - 1 columns of the board. For each of these ways, generate an extended set of positions by placing a queen in each row of the kth column. Now filter these, keeping only the positions for which the queen in the kth column is safe with respect to the other queens. This produces the sequence of all ways to place k queens in the first k columns. By continuing this process, we will produce not only one solution, but all solutions to the puzzle.We implement this solution as a procedure queens, which returns a sequence of all solutions to the problem of placing n queens on an n× n chessboard. Queens has an internal procedure queen-cols that returns the sequence of all ways to place queens in the first k columns of the board.

;; In this procedure rest-of-queens is a way to place k - 1 queens in the first k - 1 columns, and new- row is a proposed row in which to place the queen for the kth column. Complete the program by implementing the representation for sets of board positions, including the procedure adjoin-position, which adjoins a new row-column position to a set of positions, and empty-board, which represents an empty set of positions. You must also write the proceduresafe?, which determines for a set of positions, whether the queen in the kth column is safe with respect to the others. (Note that we need only check whether the new queen is safe -- the other queens are already guaranteed safe with respect to each other.)
(define empty-board (list))

(define (filterbetweense s e)
    ;; Return a filter in interval [s,e). s: number; e: number.
    ;; (number,numer) -> (list->list)
    (filterout (lambda (x) (and (>= x s) (< x e)))))

(define (list-interval s e l)
    ;; Return the list interval l[s:e). s: start numer, e: end number, l: the list.
    ;; (number number list) -> (list)
    (let ((filter (filterout (lambda (x) (and (>= x s) (< x e))))))
      (map (lambda (x) (list-ref l x))
           (filter (enumerate-interval 0 (- (length l) 1))))))

(define (adjoin-position new-row rest-of-queens)
    ;; Return the position with k queens on it. new-row: the row number of the kth; rest-of-queens: the rows of the k-1 queens.
    ;; (number,number,list) -> (list)
    (cons new-row rest-of-queens))

(define (list-in ele l)
    ;; Tell whether an element is in a list. ele: element; l: the list.
    ;; (A,list) -> boolean
    (cond ((null? l) #f)
          ((= ele (car l)) #t)
          (else (list-in ele (cdr l)))))

(define (safe? k positions)
  ;; To check whether a board is safe or not. k: the number of queens it have; position: the board position.
  ;; (number,list) -> (boolean)
  (or (null? positions)
      (= (length positions) 1)
      (not (or (list-in (car positions) (cdr positions))
               (list-in (car positions) (map + (cdr positions) (enumerate-interval 1 (- k 1))))
               (list-in (car positions) (map - (cdr positions) (enumerate-interval 1 (- k 1))))))))


(define (filter f list)
  ;; Filetr a list. filter: filter function; list: the list to be filtered.
  ;; ((A->boolean),list) -> (list)
  ((filterout f) list))


(define (queens board-size)
  ;; Give all the solution to board of board-size. board-size: size of the board.
  ;; (number) -> (list)
  (define (queen-col k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap (lambda (positions)
                    (map (lambda (new-row)
                           (adjoin-position new-row positions))
                         (enumerate-interval 1 board-size)))
                  (queen-col (- k 1))))))
  (queen-col board-size))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.43
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Louis Reasoner is having a terrible time doing exercise 2.42. His queens procedure seems to work, but it runs extremely slowly. (Louis never does manage to wait long enough for it to solve even the 6× 6 case.) When Louis asks Eva Lu Ator for help, she points out that he has interchanged the order of the nested mappings in the flatmap, writing it as

;; (flatmap
;;  (lambda (new-row)
;;    (map (lambda (rest-of-queens)
;;           (adjoin-position new-row k rest-of-queens))
;;         (queen-cols (- k 1))))
;;  (enumerate-interval 1 board-size))

;; Explain why this interchange makes the program run slowly. Estimate how long it will take Louis's program to solve the eight-queens puzzle, assuming that the program in exercise 2.42 solves the puzzle in time T.
