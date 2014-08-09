(load "../BookExercise/basic")
(define (loadimp33) (load "sec3.3"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section 3.3.1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; New cons
(define (new-cons x y)
  (let ((newpair (list 1)))
    (set-car! newpair x)
    (set-cdr! newpair y)
    newpair))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section 3.3.2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The queue
;; Constructor
(define (queue-make)
  (let ((q (list (list))))
    (set-cdr! q (car q))
    q))

;; Selector
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (queue-empty? q)
  (null? (front-ptr q)))

(define (queue-front q)
  (if (queue-empty? q)
      (error "FRONT called with an empty queue" q)
      (car (front-ptr q))))

;; Mutator
(define (queue-insert q e)
  ;; Insert the element e into the queue q.Return the q.
  ;; (list) -> (list)
  (let ((newe (list e)))
    (cond ((queue-empty? q)
           (set-front-ptr! q newe)
           (set-rear-ptr! q newe))
          (else
           (set-cdr! (rear-ptr q) newe)
           (set-rear-ptr! q newe)))
    q))

(define (queue-delete q)
  ;; Delete the first element e of the queue q. Return the q.
  ;; (list) -> (list)
  (cond ((queue-empty? q)
         (errormsg "Empty queue!" "Can't delete!"))
        (else
         (set-front-ptr! q (cdr (front-ptr q)))))
  q)

;; ;; Test
;; (define q (queue-make))
;; (print-out (queue-delete q))
;; (print-out (queue-insert q 1))
;; (print-out (queue-insert q 2))
;; (print-out (queue-delete q))
;; (print-out (queue-delete q))
;; (print-out (queue-insert q 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section 3.3.3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; two dimension table
(define (td-table)
  ;; The table structure goes: table are lists of subtable items. Each subtable item is cons of a key and a subtable. Each subtable is a list of element items. Each element item is cons of a key and a value.

  (let ((table (list)))
    ;; dispatch function to provide apis
    (define (dispatch m)
      ;; Selector
      (define (empty?) (null? table))

      ;; Error
      (define (no-key-error k)
        (errormsg "Can't find item with the keys" k))

      ;; Operator
      (define (simple-search k t error-handle)
        ;; Search for once using key k in table t. Return the value of the key k if item found; false if item not found
        (let iter ((rest t))
          (cond ((null? rest) (error-handle k))
                ((equal? (caar rest) k) (cdar rest))
                (else (iter (cdr rest))))))

      (define (delete-search k t)
        ;; Search for once using key k in table t. Return the pointer before the item if item found; false if item not found. If there is only one node return the node if it is, otherwise false.
        ;; (print-out k)
        (cond ((null? t) #f)
              ((equal? k (caar t)) t)
              ((null? (cdr t)) (if (equal? k (caar t)) t #f))
              (else
               (let iter ((former t)
                          (latter (cdr t)))
                 ;; (print-out former)
                 ;; (print-out latter)
                 (cond ((null? latter) #f)
                       ((equal? (caar latter) k) former)
                       (else (iter (cdr former) (cdr latter))))))))

      (define (search k1 k2)
        ;; Search for the item in table with k1 and k2
        (let ((subtable (simple-search k1 table no-key-error)))
          (if subtable
              (simple-search k2 subtable no-key-error)
              subtable)))

      ;; Mutator
      (define (insert k1 k2 element)
        (let ((subtable (simple-search k1 table (lambda (k) #f))))
          ;; Read for change of table
          (let ((newitem (cons k2 element)))
            (if subtable
                (set-cdr! subtable (cons newitem (cdr subtable)))
                (set! table (cons (list k1 newitem) table))))))

      (define (delete k1 k2)
        ;; Delete the one item with key k1 and k2
        (let ((firsttable (delete-search k1 table)))
          ;; (display "--delete--")
          ;; (display k1)
          ;; (display "-")
          ;; (display k2)
          ;; (print-out "---")
          ;; (print-out firsttable)
          (cond ((not firsttable) #t)
                (else
                 (let ((secondtable
                        (delete-search
                         k2
                         (if (equal? k1 (caar firsttable))
                             (cdar firsttable)
                             (cdadr firsttable)))))
                   ;; (print-out secondtable)
                   ;; Decision table for delete
;; | secondt \ firstt   | first and only                                 | first and not only | not first                   |
;; | first and only     | =table->(cdr table)=                           | <-                 | =(cdr first)->(cddr first)= |
;; | first and not only | =(car first)->(cons (caar first) (cdr second)= | <-                 | <-+ =(first->(cdr first)=   |
;; | ont first          | =(cdr second)->(cddr second)=                  | <-                 | <-                          |
                   (cond ((not secondtable) #t)
                         ((not (equal? k2 (caar secondtable)))
                          (set-cdr! secondtable (cddr secondtable)))
                         ((not (null? (cdr secondtable)))
                          (if (not (equal? k1 (caar firsttable)))
                              (set! firsttable (cdr firsttable)))
                          (set-car! firsttable
                                    (cons (caar firsttable)
                                          (cdr secondtable))))
                         (else
                          (if (equal? k1 (caar firsttable))
                              (set! table (cdr table))
                              (set-cdr! firsttable
                                        (cddr firsttable)))))))))
        ;; (print-out "--delete-end--")
        )

      (cond ((eq? m 'empty) (empty?))
            ((eq? m 'table) table)
            ((eq? m 'search) search)
            ((eq? m 'insert) insert)
            ((eq? m 'delete) delete)
            (else
             (error "Undefined Operator for td-table!" m)))
      )
    dispatch)
  )

;; Wraper
(define (td-table-table d) (d 'table))
(define (td-table-empty? d) (d 'empty))
(define (td-table-search d k1 k2) ((d 'search) k1 k2))
(define (td-table-insert d k1 k2 element) ((d 'insert) k1 k2 element))
(define (td-table-delete d k1 k2) ((d 'delete) k1 k2))

;; Test
;; (define td (td-table))
;; (print-out (td-table-empty? td))
;; (td-table-insert td 1 1 'w)
;; (print-out (td-table-table td))
;; (td-table-insert td 1 2 'x)
;; (print-out (td-table-table td))
;; (td-table-insert td 2 1 'y)
;; (print-out (td-table-table td))
;; (td-table-insert td 2 2 'z)
;; (print-out (td-table-table td))

;; (print-out (td-table-search td 1 1))
;; (print-out (td-table-search td 1 2))
;; (print-out (td-table-search td 2 1))
;; (print-out (td-table-search td 2 2))
;; (td-table-delete td 2 2)
;; (print-out (td-table-table td))
;; (td-table-delete td 2 1)
;; (print-out (td-table-table td))
;; (td-table-delete td 1 1)
;; (print-out (td-table-table td))
;; (td-table-delete td 1 2)
;; (print-out (td-table-table td))

(exit)
