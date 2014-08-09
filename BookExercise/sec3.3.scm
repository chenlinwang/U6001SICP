(load "../BookExercise/basic")
(define (loadexe33) (load "sec3.3"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3.14
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following procedure is quite useful, although obscure:
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))
;; Loop uses the "temporary" variable temp to hold the old value of the cdr of x, since the set-cdr! on the next line destroys the cdr. Explain what mystery does in general. Suppose v is defined by (define v (list 'a 'b 'c 'd)). Draw the box-and-pointer diagram that represents the list to which v is bound. Suppose that we now evaluate (define w (mystery v)). Draw box-and-pointer diagrams that show the structures v and w after evaluating this expression. What would be printed as the values of v and w ?

;; ;; Test
;; (define v '(a b c d))
;; (define w (mystery v))
;; (print-out v)
;; (print-out w)

;; The mystery will reverse the list. We will expect v and w to be:
;; : v : (a)
;; : w : (d c b a)

;; Because that the v is pointing to the start of the original list, which is at value =a=. But the process reverses the list and there is nothing following =a=. So that gives v =(a)=.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3.16
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ben Bitdiddle decides to write a procedure to count the number of pairs in any list structure. "It's easy," he reasons. "The number of pairs in any structure is the number in the car plus the number in the cdr plus one more to count the current pair." So Ben writes the following procedure:

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

;; Show that this procedure is not correct. In particular, draw box-and-pointer diagrams representing list structures made up of exactly three pairs for which Ben's procedure would return 3; return 4; return 7; never return at all.

;; ;; For 3
;; (define three (list 1 2 3))
;; (print-out (count-pairs three))

;; ;; For 4
;; (define last (list 3))
;; (define second (cons 2 last))
;; (define four (cons second last))
;; (print-out (count-pairs four))

;; ;; For 5
;; (define last (list 3))
;; (define second (cons 2 last))
;; (define five (cons second second))
;; (print-out (count-pairs five))

;; ;; For 5
;; (define last (list 3))
;; (define second (cons last last))
;; (define five-2 (cons second last))
;; (print-out (count-pairs five-2))

;; ;; For 7
;; (define last (list 3))
;; (define second (cons last last))
;; (define seven (cons second second))
;; (print-out (count-pairs seven))

;; ;; Forever
;; (define last (list 3))
;; (define forever (cons 1 (cons 2 last)))
;; (set-cdr! last forever)
;; (print-out (count-pairs forever))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3.17
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Devise a correct version of the count-pairs procedure of exercise 3.16 that returns the number of distinct pairs in any structure. (Hint: Traverse the structure, maintaining an auxiliary data structure that is used to keep track of which pairs have already been counted.)

(define (count-real-pairs x)
  ;; Define a visited list for visit
  (define visited-list (list))

  ;; Define a function for adding new pair
  (define (add-visit ele)
    (set! visited-list (cons ele visited-list)))

  ;; Define a function for looking up pair
  (define (is-not-visited? ele)
    (let iter ((rest visited-list))
      ;; Refuse a non pair object
        (cond ((not (pair? ele)) #f)
              ;; Add new pair to visited-list
              ((null? rest)
               (add-visit ele)
               #t)
              ;; Refuse a already visited pair
              ((eq? ele (car rest)) #f)
              (else (iter (cdr rest))))))

  ;; Test x
  (if (is-not-visited? x)
      ;; Iterating
      (let iter ((rest x))
        ;; Terminate if there is no more pairs
        (if (not (null? rest))
            (begin
              ;; Try iterate the car
              (if (is-not-visited? (car rest))
                  (iter (car rest)))
              ;; Try iterate the cdr
              (if (is-not-visited? (cdr rest))
                  (iter (cdr rest)))
              ))))

  (length visited-list))

;; ;; Test
;; ;; For 3
;; (define three (list 1 2 3))
;; (print-out (count-real-pairs three))

;; ;; For 4
;; (define last (list 3))
;; (define second (cons 2 last))
;; (define four (cons second last))
;; (print-out (count-real-pairs four))

;; ;; For 5
;; (define last (list 3))
;; (define second (cons 2 last))
;; (define five (cons second second))
;; (print-out (count-real-pairs five))

;; ;; For 5
;; (define last (list 3))
;; (define second (cons last last))
;; (define five-2 (cons second last))
;; (print-out (count-real-pairs five-2))

;; ;; For 7
;; (define last (list 3))
;; (define second (cons last last))
;; (define seven (cons second second))
;; (print-out (count-real-pairs seven))

;; ;; Forever
;; (define last (list 3))
;; (define forever (cons 1 (cons 2 last)))
;; (set-cdr! last forever)
;; (print-out (count-real-pairs forever))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3.18
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write a procedure that examines a list and determines whether it contains a cycle, that is, whether a program that tried to find the end of the list by taking successive cdrs would go into an infinite loop. Exercise 3.13 constructed such lists.

(define (circle? l)
  ;; Determind wheter a list has a circle. l: the input list.
  ;; (list) -> (boolean)

  ;; the visited list
  (define visited-list (list))

  ;; add the entry to visited list
  (define (add-entry ele)
    (set! visited-list (cons ele visited-list)))

  ;; find whether an entry has appeared in visited list
  (define (is-visited? ele)
    (let iter ((rest visited-list))
      (if (null? rest)
          #f
          (if (eq? (car rest) ele)
              #t
              (iter (cdr rest))))))

  ;; find whether a circle exits
  (let iter ((rest l))
    (if (null? rest)
        #f
        (if (is-visited? rest)
            #t
            (begin
              (add-entry rest)
              (iter (cdr rest))))))
  )

;; ;; Test
;; (define x (list 1 2))
;; (set-cdr! (cdr x) x)
;; (print-out (circle? x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3.19
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Redo exercise 3.18 using an algorithm that takes only a constant amount of space. (This requires a very clever idea.)

(define (smart-circle? l)
  ;; Using very only limitted space to test whether a list has a circle. l: the input list.
  (if (or (null? l) (null? (cdr l)))
      #f
      (let iter ((first l)
                 (second (cdr l)))
        (if (eq? first second)
            #t
            (if (or (null? second) (null? (cdr second)))
                #f
                (iter (cdr first) (cddr second)))))))

(define (make-circle l)
  ;; Make the inpute list a circle.l: the input list.
  ;; (list) -> (list)
  (set-cdr! (let iter ((rest l))
              (if (= (length rest) 1)
                  rest
                  (iter (cdr rest))))
            l)
  )

;; ;; Test
;; (define x (list 1 2 3))
;; (print-out (smart-circle? x))
;; (make-circle x)
;; (print-out (smart-circle? x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3.21
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ben Bitdiddle decides to test the queue implementation described above. He types in the procedures to the Lisp interpreter and proceeds to try them out:

;; (define q1 (make-queue))
;; (insert-queue! q1 'a)
;; ((a) a)
;; (insert-queue! q1 'b)
;; ((a b) b)
;; (delete-queue! q1)
;; ((b) b)
;; (delete-queue! q1)
;; (() b)

;; "It's all wrong!" he complains. "The interpreter's response shows that the last item is inserted into the queue twice. And when I delete both items, the second b is still there, so the queue isn't empty, even though it's supposed to be." Eva Lu Ator suggests that Ben has misunderstood what is happening. "It's not that the items are going into the queue twice," she explains. "It's just that the standard Lisp printer doesn't know how to make sense of the queue representation. If you want to see the queue printed correctly, you'll have to define your own print procedure for queues." Explain what Eva Lu is talking about. In particular, show why Ben's examples produce the printed results that they do. Define a procedure print-queue that takes a queue as input and prints the sequence of items in the queue.

;; (load "../BookImplementation/sec3.3.scm")
;; (define (queue-print q)
;;   (print-out (front-ptr q))
;;   (front-ptr q))

;; ;; Test
;; (define q1 (queue-make))
;; (queue-print (queue-insert q1 'a))
;; (queue-print (queue-insert q1 'b))
;; (queue-print (queue-delete q1))
;; (queue-print (queue-delete q1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3.22
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Instead of representing a queue as a pair of pointers, we can build a queue as a procedure with local state. The local state will consist of pointers to the beginning and the end of an ordinary list. Thus, the make-queue procedure will have the form
;; #+BEGIN_SRC scheme
;; (define (make-queue)
;;   (let ((front-ptr ...)
;;         (rear-ptr ...))
;;     <definitions of internal procedures>
;;     (define (dispatch m) ...)
;;     dispatch))
;; #+END_SRC
;; Complete the definition of make-queue and provide implementations of the queue operations using this representation.

(define (queue-make queue-name)
  ;; Procedure to represent a queue object
  (let ((front-ptr (list))
        (rear-ptr (list)))

    ;; Selector
    (define (empty?) (null? front-ptr))
    (define (front) (if (empty?)
                        (errormsg "Empty queue!" "Can't get front element!")
                        (car front-ptr)))

    (define (set-front-ptr! ele) (set! front-ptr ele))
    (define (set-rear-ptr! ele) (set! rear-ptr ele))
    (define (set-queue-rear! ele) (set! (cdr rear-ptr) ele))
    (define (number) (length front-ptr))

    ;; Mutator
    (define (insert ele)
      (let ((newele (list ele)))
        (cond ((empty?)
               (set-front-ptr! newele)
               (set-rear-ptr! newele))
              (else
               (set-queue-rear! newele)
               (set-rear-ptr! newele))))
      front-ptr)

    (define (delete)
      (cond ((empty?)
             (errormsg "Empty queue!" "Can't delete!"))
            (else
             (set-front-ptr! (cdr front-ptr))))
      front-ptr)

    (define (dispatch m)
      (cond ((eq? m 'name) queue-name)
            ((eq? m 'length) (number))
            ((eq? m 'empty?) (empty?))
            ((eq? m 'front) (front))
            ((eq? m 'front-ptr) front-ptr)
            ((eq? m 'rear-ptr) (if (empty?) (list) rear-ptr))
            ((eq? m 'insert) insert)
            ((eq? m 'delete) (delete))
            (else (error "Undefined operator - queue-make" m))))
    dispatch))

;; Wraper
(define (queue-empty? q) (q 'empty?))
(define (queue-name q) (q 'name))
(define (queue-length q) (q 'length))
(define (queue-front q) (q 'front))
(define (queue-front-ptr q) (q 'front-ptr))
(define (queue-rear-ptr q) (q 'rear-ptr))
(define (queue-insert q e) ((q 'insert) e))
(define (queue-delete q) (q 'delete))

;; ;; Test
;; (define q (queue-make "test"))
;; (print-out (queue-empty? q))
;; (print-out (queue-insert q 1))
;; (print-out (queue-insert q 2))
;; (print-out (queue-front q))
;; (print-out (queue-front-ptr q))
;; (print-out (queue-rear-ptr q))

;; (print-out (queue-delete q))
;; (print-out (queue-delete q))
;; (print-out (queue-front q))
;; (print-out (queue-front-ptr q))
;; (print-out (queue-rear-ptr q))

;; (print-out (queue-insert q 3))
;; (print-out (queue-front q))
;; (print-out (queue-front-ptr q))
;; (print-out (queue-rear-ptr q))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3.23
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A deque ("double-ended queue") is a sequence in which items can be inserted and deleted at either the front or the rear. Operations on deques are the constructor make-deque, the predicate empty-deque?, selectors front-deque and rear-deque, and mutators front-insert-deque!, rear-insert-deque!, front-delete-deque!, and rear-delete-deque!. Show how to represent deques using pairs, and give implementations of the operations.23 All operations should be accomplished in \Theta (1) steps.

;; Node element for the deque
;; Node Constructor
(define (node-make pre ele nex)
  (cons pre (cons ele nex)))
(define (empty-node) (node-make (list) (list) (list)))
;; Node Selector
(define node-pre car)
(define node-ele cadr)
(define node-nex cddr)
(define (node-empty? n) (null? (node-ele n)))
(define (node-pre? n) (null? (node-pre n)))
(define (node-nex? n) (null? (node-nex n)))
;; Node Mutator
(define (node-set-pre! n e) (set-car! n e) n)
(define (node-set-ele! n e) (set-car! (cdr n) e) n)
(define (node-set-nex! n e) (set-cdr! (cdr n) e) n)
(define (node-connect! n1 n2)
  (node-set-nex! n1 n2)
  (node-set-pre! n2 n1)
  n1)
;; Operator
(define (node-all n direct direct?)
  ;; Get the whole node list, from the one direct of the node list. n: node, node should not be an empty node; direct: how to get the next node.
  (let iter ((all (list))
             (rest n))
    (if (direct? rest)
        (cons (node-ele rest) all)
        (iter (cons (node-ele rest) all)
              (direct rest)))))
(define (node-iter n)
  (node-all n node-nex node-nex?))
(define (node-iter-rv n)
  (node-all n node-pre node-pre?))

;; ;; Test for node
;; (define n1 (node-make (list) 1 (list)))
;; (define n2 (node-make (list) 2 (list)))
;; (define n3 (node-make (list) 3 (list)))
;; (define n4 (node-make (list) 4 (list)))

;; (node-connect! n1 n2)
;; (node-connect! n2 n3)
;; (node-connect! n3 n4)
;; (print-out (node-iter n1))
;; (print-out (node-iter-rv n4))


;; One way of node structure implementation
(define (deque-make)
  ;; Note that the empty deque to be empty node for front and rear
  (let ((front (empty-node))
        (rear (list)))
    (set! rear front)
    ;; dispatch function
    (define (dispatch m)
      ;; Error
      (define (empty-error info)
        (errormsg "Empty deque!"
                  (string-append "Can't " info "!")))

      ;; Selector
      (define (empty?) (null? (node-ele front)))
      (define (one?) (and (not (empty?)) (null? (node-nex front))))

      (define (get loc name)
        (if (empty?)
            (empty-error (string-append "get " name))
            (node-ele loc)))
      (define (head) (get front "head"))
      (define (butt) (get rear "butt"))

      (define (delete-head)
        (cond ((empty?)
               ;; If empty, give error
               (empty-error "delete head"))
              ((one?)
               ;; If there is only one node, change the existing node element to nil
               (node-set-ele! front (list)))
              (else
               ;; If there are two or more, add one and set the location pointer to the other.
               (set! front (node-nex front))
               (node-set-pre! front (list)))))

      (define (delete-butt)
        (cond ((empty?)
               ;; If empty, give error
               (empty-error "delete butt"))
              ((one?)
               ;; If there is only one node, change the existing node element to nil
               (node-set-ele! front (list)))
              (else
               ;; If there are two or more, add one and set the location pointer to the other.
               (set! rear (node-pre rear))
               (node-set-nex! rear (list)))))

      (define (insert-head ele)
        (cond ((empty?)
               (node-set-ele! front ele))
              (else
               (let ((newn (node-make (list) ele (list))))
                 (node-connect! newn front)
                 (set! front newn)))))

      (define (insert-butt ele)
        (cond ((empty?)
               (node-set-ele! front ele))
              (else
               (let ((newn (node-make (list) ele (list))))
                 (node-connect! rear newn)
                 (set! rear newn)))))

      (define (deque-iter)
        if (empty?)
        (list)
        )

      (cond ((eq? m 'empty?) (empty?))
            ;; ((eq? m 'rear) rear)
            ;; ((eq? m 'front) front)
            ((eq? m 'head) (head))
            ((eq? m 'butt) (butt))
            ((eq? m 'delete-head) (delete-head))
            ((eq? m 'delete-butt) (delete-butt))
            ((eq? m 'insert-head) insert-head)
            ((eq? m 'insert-butt) insert-butt)
            ((eq? m 'deque-iter) (deque-iter))
            (else
             (error "Undefined operator -- deque" m))))
    dispatch)
  )

;; Outer wraper
(define (deque-empty? d) (d 'empty?))
;; (define (deque-front d) (d 'front))
;; (define (deque-rear d) (d 'rear))
(define (deque-head d) (d 'head))
(define (deque-butt d) (d 'butt))
(define (deque-delete-head d) (d 'delete-head))
(define (deque-delete-butt d) (d 'delete-butt))
(define (deque-insert-head d e) ((d 'insert-head) e))
(define (deque-insert-butt d e) ((d 'insert-butt) e))
(define (deque-iter d) (d 'deque-iter))

;; Test
(define d (deque-make))
;; (print-out (deque-iter d))
;; (deque-insert-head d 1)
;; (deque-insert-head d 2)
;; (print-out (deque-iter d))

;; (deque-delete-head d)
;; (deque-iter d)

;; (deque-delete-head d)
;; (print-out (deque-iter d))

;; (deque-insert-head d 3)
;; (print-out (deque-iter d))

;; (deque-insert-butt d 1)
;; (deque-insert-butt d 2)
;; (print-out (deque-iter d))

;; (deque-delete-butt d)
;; (print-out (deque-iter d))

;; (deque-delete-butt d)
;; (print-out (deque-iter d))

;; (deque-insert-butt d 3)
;; (print-out (deque-iter d))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3.24
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; In the table implementations above, the keys are tested for equality using equal? (called by assoc). This is not always the appropriate test. For instance, we might have a table with numeric keys in which we don't need an exact match to the number we're looking up, but only a number within some tolerance of it. Design a table constructor make-table that takes as an argument a same-key? procedure that will be used to test "equality" of keys. Make-table should return a dispatch procedure that can be used to access appropriate lookup and insert! procedures for a local table.
(define (blend-table blend)
  ;; blend function take in a blend function to help mix keys. If will use the blend function to search ,insert of delete
  (let ((table (list 'blend-table)))
    (define (dispatch m)

      ;; selector
      (define (empty?) (null? (cdr table)))
      (define content cdr)
      ;; error
      (define (no-key-error k)
        (errormsg "Can't find item with key:" k))
      (define (no-his-key-error k num)
        (errormsg "Can't find item with key in history:" (list k num)))
      (define (silence-error k) #f)

      ;; error generator
      (define (his-error-gen num)
        (lambda (k) (no-his-key-error k num)))

      ;; Simple search, return a list start with the item
      (define (simple-search k t error-headle)
        (let iter ((rest t))
          (cond ((null? rest) (error-headle k))
                ((equal? k (caar rest)) rest)
                (else (iter (cdr rest))))))

      ;; Search -- depend on simple-search
      (define (search k)
        (let ((bk (blend k)))
          (cdar (simple-search bk (content table) no-key-error))))

      ;; History Search Return -- depend on simple-search
      (define (his-search-return k num return)
        ;; Search for the history value of certain key. 1 means the most present search. Will return false if not found.
        (if (< num 1)
            (errormsg "Can't find history less than 1" num)
            (let ((bk (blend k)))
              (let iter ((rest (content table))
                         (restnum num))
                (let ((thissearch (simple-search bk rest (his-error-gen num))))
                  (cond ((not thissearch) #f)
                        ((= restnum 1) (return thissearch))
                        (else
                         (iter (cdr thissearch) (- restnum 1)))))))))
      ;; History Search -- depond on his-search-return
      (define his-search
        (lambda (k num)
                (his-search-return k num cdar)))

      ;; Insert
      (define (insert k e)
        (let ((bk (blend k)))
          (set-cdr! table (cons (cons bk e) (content table)))))

      ;; Delete Generator -- for delete and his-delete
      (define (delete-gen k t error-headle)
        ;; t is a table that always has target item not in the first location
        (let ((bk (blend k)))
          (cond ((null? t) (error-headle k))
                (else
                 (let iter ((first t)
                            (second (cdr t)))
                   (cond ((null? second) (error-headle k))
                         ((equal? bk (caar second))
                          (set-cdr! first (cddr first))
                          t)
                         (else (iter (cdr first) (cdr second)))))))))
      ;; Delete -- depend on delete-gen
      (define delete
        (lambda (k) (delete-gen k table no-key-error)))

      ;; History Delete -- depend on simple search and delete-gen
      (define (his-delete k num)
        (let ((before (his-search-return k (- num 1) cdr)))
          (if (not before)
              (no-his-key-error k num)
              (if (not (delete-gen k before silence-error))
                  (no-his-key-error k num)
                  table))))

      ;; Dispatch for apis
      (cond ((equal? m 'empty) (empty?))
            ((equal? m 'search) search)
            ((equal? m 'his-search) his-search)
            ((equal? m 'delete) delete)
            ((equal? m 'his-delete) his-delete)
            ((equal? m 'insert) insert)
            ((equal? m 'table) table)
            (else (error "Undefined Operator for blend-table!" m)))
      )
    dispatch)
  )

;; Outside dispatches
(define (blend-table-table d) (d 'table))
(define (blend-table-empty? d) (d 'empty))
(define (blend-table-search d k) ((d 'search) k))
(define (blend-table-delete d k) ((d 'delete) k))
(define (blend-table-his-search d k num) ((d 'his-search) k num))
(define (blend-table-his-delete d k num) ((d 'his-delete) k num))
(define (blend-table-insert d k e) ((d 'insert) k e))

;; ;; Test
;; (define (seven k) (remainder k 7))
;; (define bt (blend-table seven))
;; (print-out (blend-table-empty? bt))
;; (map (lambda (k) (blend-table-insert bt k k))
;;      (list 0 1 2 3 4 5 6 7 8 9 10 11))

;; (print-out (blend-table-table bt))
;; (print-out (map (lambda (k) (blend-table-search bt k))
;;                 (list 0 1 2 3 4 5 6 7 8 9 10 11)))
;; (print-out (map (lambda (k) (blend-table-his-search bt k 2))
;;                 (list 0 1 2 3 4 5 6)))
;; (map (lambda (k) (blend-table-delete bt k))
;;      (list 4 5 6 5))
;; (print-out (map (lambda (k) (blend-table-his-delete bt k 2))
;;                 (list 0 1 2 3 4 0)))
;; (print-out (blend-table-table bt))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3.25
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generalizing one- and two-dimensional tables, show how to implement a table in which values are stored under an arbitrary number of keys and different values may be stored under different numbers of keys. The lookup and insert! procedures should take as input a list of keys used to access the table.

;; The main concern is to use how to fold tables to inside each other. It could be a structure like this:

;; key := any object in scheme
;; value := any object in scheme
;; subtable := (key (value) subtable ... subtable)
;; table := ('multi-key-table (value) subtable ... subtale)

(define (multi-key-table)
  ;; A table that accepts multiple length of key to look up or insert.
  (let ((table (list 'multi-key-table (list))))
    (define (dispatch m)
      ;; Constructor
      (define (empty-subtable key)
        (list key (list)))
      (define (subtable-make key value subtable)
        (list key (list value) subtable))

      ;; Selector
      (define (empty?) (and (null? (cadr table))
                            (null? (cddr table))))
      (define get-itself (lambda (t) t))
      (define get-key car)
      (define get-value cadr)
      (define get-subtable cddr)

      ;; Mutator
      (define (set-value! t v)
        (set-car! (cdr t) (list v)))
      (define (add-subtable! t s)
        (set-cdr! (cdr t) (cons s (get-subtable t))))

      ;; Tag
      (define no-value-found-tag 'no-value-found)
      (define found-tag 'found)

      ;; Error
      (define (no-key-error k)
        (errormsg "Can't find item with key:" k))
      (define (silence-error k) #f)

      ;; Warning
      (define (rewrite-value-warning k old new)
        (warningmsg "Rewrite the value of key with new value:"
                    (list old k new)))

      ;; Simple Search: to search for the usage of search, insert, delete. Return the desired table with a found-tag if found, resturn the restkey with a no-value-found, if not found.
      (define (simple-search k t)
        (let iter1 ((resttable t)
                    (restkey k))
          ;; (print-out "--iter 1--")
          ;; (print-out resttable)
          ;; (print-out restkey)
          (cond ((null? restkey) (cons found-tag resttable))
                (else
                 (let ((firstkey (car restkey)))
                   (let iter2 ((subtables
                                (get-subtable resttable)))
                     ;; (print-out "--iter 2--")
                     ;; (print-out subtables)
                     (cond ((null? subtables)
                            (cons no-value-found-tag
                                  (cons restkey resttable)))
                           ((equal? firstkey
                                    (caar subtables))
                            (iter1 (car subtables) (cdr restkey)))
                           (else
                            (iter2 (cdr subtables))))))))))

      ;; Search for the item
      (define (search k)
        (cond ((null? k)
               (if (null? (get-value table))
                   (no-key-error k)
                   (car (get-value table))))
              (else
               (let ((result (simple-search k table)))
                 (let ((search-tag (car result))
                       (real-table (cdr result)))
                   (cond ((eq? found-tag search-tag)
                          (car (get-value real-table)))
                         ((eq? no-value-found-tag search-tag)
                          (no-key-error k))
                         (else
                          (error "Intern Error -- search" k))))))))

      ;; Insert for the item k with v
      (define (insert k v)
        (cond ((null? k)
               (if (not (null? (get-value table)))
                   (rewrite-value-warning
                    k
                    (car (get-value table))
                    v))
               (set-value! table v))
              (else
               (let ((result (simple-search k table)))
                 (let ((search-tag (car result))
                       (rest (cdr result)))
                   (cond ((eq? found-tag search-tag)
                          (if (not (null? (get-value rest)))
                              (rewrite-value-warning
                               k
                               (car (get-value rest))
                               v))
                          (set-value! rest v))
                         ((eq? no-value-found-tag search-tag)
                          (let iter ((restkey (car rest))
                                      (resttable (cdr rest)))
                             (cond ((null? restkey)
                                    ;; (print-out v)
                                    ;; (print-out resttable)
                                    (set-value! resttable v))
                                   (else
                                    (let ((newtable
                                           (empty-subtable
                                            (car restkey))))
                                      (add-subtable! resttable newtable)
                                      (iter (cdr restkey) newtable))))))))))))

      ;; Internal wraper
      (cond ((eq? m 'search) search)
            ((eq? m 'insert) insert)
            ((eq? m 'table) table)
            ((eq? m 'empty) (empty?))
            (else
             (error "Undefined Operations for multi-key-table!"
                    m)))
      )
    dispatch)
  )

(define (multi-key-table-empty? d) (d 'empty))
(define (multi-key-table-search d k) ((d 'search) k))
(define (Multi-key-table-insert d k v) ((d 'insert) k v))
(define (multi-key-table-table d) (d 'table))

;; Test
;; (define mt (multi-key-table))
;; (print-out (multi-key-table-table mt))
;; (multi-key-table-insert mt (list) 0)
;; (print-out (multi-key-table-search mt (list)))
;; (multi-key-table-insert mt (list 1) 1)
;; (multi-key-table-insert mt (list 2 1) 21)
;; (print-out (multi-key-table-table mt))
;; (multi-key-table-insert mt (list 2 1 2 1) 2121)
;; (print-out (multi-key-table-table mt))
;; (print-out (multi-key-table-search mt (list 2 1 2 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3.26
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;To search a table as implemented above, one needs to scan through the list of records. This is basically the unordered list representation of section 2.3.3. For large tables, it may be more efficient to structure the table in a different manner. Describe a table implementation where the (key, value) records are organized using a binary tree, assuming that keys can be ordered in some way (e.g., numerically or alphabetically). (Compare exercise 2.66 of chapter 2.)

(define (heap-table transfer)
  ;; heap-table function constructor, requires a transfer function that takes in a key object and then returns a number that could be compared using =, <, or >.
  ;; Tree node structure, there are two keys: one for notation on the far end of the node, one for indexing on the first.
  ;; Constructor
  (define (node-make key value left right)
    (list (transfer key) value left right key))
  (define (new-node key value)
    (node-make key value (list) (list)))
  (define (empty-node)
    (list (list) (list) (list) (list) (list)))

  ;; Selector
  (define caddddr (lambda (x) (cadr (cdddr x))))
  (define cddddr (lambda (x) (cdr (cdddr x))))
  (define node-key car)
  (define node-value cadr)
  (define node-left caddr)
  (define node-right cadddr)
  (define node-real-key caddddr)

  ;; Operator
  (define (node-request-gen sel)
    (lambda (n) (null? (sel n))))
  (define node-null-key? (node-request-gen node-key))
  (define node-null-value? (node-request-gen node-value))
  (define node-null-left? (node-request-gen node-left))
  (define node-null-right? (node-request-gen node-right))
  (define (node-empty? table) (and (node-null-value? table)
                                   (node-null-left? table)
                                   (node-null-right? table)))
  ;; Mutator
  (define (node-set-gen sel)
    (lambda (n e) (set-car! (sel n) e)))
  (define node-set-key! (node-set-gen (lambda (x) x)))
  (define node-set-value! (node-set-gen cdr))
  (define node-set-left! (node-set-gen cddr))
  (define node-set-right! (node-set-gen cdddr))
  (define node-set-real-key! (node-set-gen cddddr))
  (define (node-set-key-value-real! n k v r)
    (node-set-key! n k)
    (node-set-value! n v)
    (node-set-real-key! n r)
    n)
  (define (node-clear! n)
    (node-set-key-value-real! n (list) (list) (list))
    n)
  (define (node-set-node! n1 n2)
    (node-set-key-value-real! n1 (node-value n2)
                              (node-value n2)
                              (node-real-key n2))
    n1)

  ;; Swap check
  (define (node-swap-check n)
    ;; Check for newly inserted node. and left and right are in position that left < right if they both exist.

    (define (change-for location relate?)
      ;; help function change the current node for location location
      ;; (print-out "change-for")
      ;; (print-out location)
      ;; (print-out n)
      (cond ((null? (node-key location)) #t)
            ((relate? (node-key n) (node-key location)) #t)
            (else
             (let ((tmpnode (empty-node)))
               (node-set-node! tmpnode n)
               (node-set-node! n location)
               (node-set-node! location tmpnode)
               location))))

      (let ((left (node-left n))
            (right (node-right n)))
        (cond ((and (null? left) (null? right)) #t)
              ((null? left) (change-for right <))
              ((null? right) (change-for left >))
              (else
               (let ((leftreturn (change-for left >)))
                 (cond ((equal? leftreturn #t) (change-for right <))
                       (else leftreturn)))))))

  (let ((table (empty-node)))
    ;; We make this a big heap, meaning left < middle < right
    (define (dispatch m)

      ;; Error
      (define (no-key-error k)
        (errormsg "Can't find item with key:" k))
      (define (silence-error k) #f)

      ;; Warning
      (define (rewrite-value-warning k old new)
        (warningmsg "Rewrite the value of key with new value:"
                    (list old k new)))
      ;; Operator
      (define (empty?) (and (node-null-value? table)
                            (node-null-left? table)
                            (node-null-right? table)))

      ;; Tags for Simple Search Return
      (define empty-tag 'empty)
      (define found-tag 'found)
      (define left-empty 'left)
      (define right-empty 'right)

      ;; Simple search for search, delete, insert
      (define (simple-search k t)
        (let iter ((resttable t))
          (let ((thistablekey (node-key resttable)))
            ;; See wheter it is an empty node
            (cond ((null? thistablekey)
                   ;; Return empty
                   (cons empty-tag resttable))
                  ;; See whether it is desired
                  ((= k thistablekey)
                   ;; Return found
                   (cons found-tag resttable))
                  ;; Check the right node
                  ((> k thistablekey)
                   ;; See if the right exists
                   (if (node-null-right? resttable)
                       (cons right-empty resttable)
                       (iter (node-right resttable))))
                  ;; Check the left node
                  ((< k thistablekey)
                   ;; See if the left exists
                   (if (node-null-left? resttable)
                       (cons left-empty resttable)
                       (iter (node-left resttable))))
                  (else (error "heap-table intern error: can't compare key"
                               (list k thistablekey resttable)))))))
      ;; Search for the key -- depend on simple-search
      (define (search k)
        ;; Find the node using simple-search
        (let ((result (simple-search (transfer k) table)))
          (let ((tag (car result))
                (table (cdr result)))
            ;; Check tag type
            (cond ((eq? tag found-tag) (node-value table))
                  (else (no-key-error k))))))

      ;; Mutator
      (define (insert k v)
        ;; Transform the key
        (let ((tk (transfer k)))
          ;; Find the node using simple-search
          (let ((result (simple-search tk table)))
            (let ((tag (car result))
                  (table (cdr result)))
              ;; check the type
              (cond ((eq? tag found-tag)
                     ;; Rewrite the node if exists and give warning!
                     (rewrite-value-warning
                      k
                      (node-value table)
                      v)
                     (node-set-value! table v)
                     (node-set-real-key! table k))
                    ((eq? tag empty-tag)
                     ;; Create the node if there is an empty node
                     (node-set-key! table tk)
                     (node-set-value! table v)
                     (node-set-real-key! table k))
                    ((eq? tag left-empty)
                     ;; Create a left node if left is empty
                     (node-set-left! table (new-node k v)))
                    ((eq? tag right-empty)
                     ;; Create a right node if right is empty
                     (node-set-right! table (new-node k v)))
                    (else
                     (error "heap-table intern error: Can't compare tag!"
                            (list tag))))))))

      ;; Find the most right or left location of the input table, will delete the empty node if found!
      (define (location-search t node-location node-set-location!)
        (let iter ((resttable t))
          ;; Check out the location node
          (let ((location (node-location resttable)))
            (cond ((and (not (null? location))
                        (node-empty? location))
                   ;; If find empty, delete it and return the node
                   (node-set-location! resttable (list))
                   resttable)
                  ((null? location)
                   ;; If no child node return the node
                   resttable)
                  ;; Else go on search
                  (else (iter location))))))

      ;; Delete the node -- depend on simple-search and location-search
      (define (delete k)
        ;; Find the node using simple-search
        (let ((result (simple-search (transfer k) table)))
          (let ((tag (car result))
                (table (cdr result)))
            ;; (print-out tag)
            ;; (print-out table)
            ;; Get the replaced node that should be moved downwards using the heap relation.
            (let ((newtable
                   ;; Check out the tag
                   (cond ((eq? found-tag tag)
                          ;; if find the node, try to get a most right child
                          (let ((most-right (location-search table node-right node-set-right!)))
                            ;; Check if the most right child is itself
                            (cond ((eq? most-right table)
                                   ;; If it is, get the most left child
                                   (let ((most-left (location-search table node-left node-set-left!)))
                                     ;; See again whethe the most left child is itself
                                     (cond ((eq? most-left table)
                                            ;; if it is, clear the table and return it
                                            (node-clear! table))
                                           (else
                                            ;; if it isn't, set the table as the most left and clear the most left,return the table.
                                            (let ((tmp (empty-node)))
                                              (node-set-node! tmp most-left)
                                              (node-clear! most-left)
                                              (node-set-node! table tmp))))))
                                  (else
                                   ;; If the most right is not itself, set the table as the most right and clear the most right and retutn the table
                                   (let ((tmp (empty-node)))
                                     (node-set-node! tmp most-right)
                                     (node-clear! most-right)
                                     (node-set-node! table tmp))))))
                         (else (no-key-error k)))))
              ;; (print-out newtable)
              ;; Maintain the relationship
              ;; no need to change
              (cond ((not newtable) #f)
                    (else
                     ;; Swap check the present
                     (let iter ((resttable
                                 (node-swap-check newtable)))
                       ;; (print-out "-swap-")
                       ;; (print-out resttable)
                       ;; If it pass exit, else continue
                       (cond ((equal? #t resttable) #t)
                             (else (iter (node-swap-check newtable)))))))))))

      ;; Inner wraper
      (cond ((eq? m 'table) table)
            ((eq? m 'search) search)
            ((eq? m 'insert) insert)
            ((eq? m 'delete) delete)
            ((eq? m 'empty) (empty?))
            (else
             (error "Undefined Operations for heap-table!" m))))
    dispatch)
  )

;; Outier wraper
(define (heap-table-table ht) (ht 'table))
(define (heap-table-empt? ht) (ht 'empty))
(define (heap-table-search ht k) ((ht 'search) k))
(define (heap-table-insert ht k v) ((ht 'insert) k v))
(define (heap-table-delete ht k) ((ht 'delete) k))

;; ;; Test
;; (define ht (heap-table (lambda (x) x)))
;; (print-out (heap-table-empt? ht))
;; (print-out (heap-table-table ht))
;; (map (lambda (x) (heap-table-insert ht x x))
;;      (list 5 6 4 1 2 7 8))
;; ;; (map (lambda (x) (print-out (heap-table-search ht x)))
;; ;;      (list 1 2 3))
;; (print-out "--table--")
;; (print-out (heap-table-table ht))
;; (print-out "--table--")
;; (map (lambda (x) (heap-table-delete ht x))
;;      (list 1 2 3 4 5 6 7 8))
;; (print-out "--table--")
;; (print-out (heap-table-table ht))
;; (print-out "--table--")
(exit)
