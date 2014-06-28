(load "../BookExercise/basic")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; section 2.3.1 quotations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (memq item itemlist)
    ;; To see whether a item is in itemlist. item: item; itemlist: list.
    ;; (EQtype, list) -> (boolean)
    (cond ((null? itemlist) #f)
          ((eq? item (car itemlist)) #t)
          (else (memq item (cdr itemlist)))))

;; (display (memq 'a '(b c d e f)))
;; (display (memq 'a '(a b c d)))
;; (display (memq 'a '(a)))
;; (newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sec1.1.scm 2.3.2 deriverative
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constructor
(define (make-sum e1 e2)
  ;; make a sum out of e1 and e2.e1:addend; e2:augend.
  ;; (A,A) -> (list/number)
  (cond ((and (number? e1) (number? e2)) (+ e1 e2))
        ((and (number? e1) (= e1 0)) e2)
        ((and (number? e2) (= e2 0)) e1)
        (else (list '+ e1 e2) )))

(define (make-product e1 e2)
  ;; make a product out of e1 and e2.e1:multiplier;e2:multiplicand.
  ;; (A,A) -> (list/number)
  (cond ((and (number? e1) (number? e2)) (* e1 e2))
        ((and (number? e1) (= e1 1)) e2)
        ((and (number? e1) (= e1 0)) 0)
        ((and (number? e2) (= e2 1)) e1)
        ((and (number? e2) (= e2 0)) 0)
        (else (list '* e1 e2) )))

;; Selectors
(define addend cadr)
(define augend caddr)
(define multiplier cadr)
(define multiplicant caddr)
;; The Operators
(define (expr? expr) (and (list? expr) (= (length expr) 3)))
(define (sum? expr) (and (expr? expr) (eq? '+ (car expr))))
(define (product? expr) (and (expr? expr) (eq? '* (car expr))))
(define (same-symbol? e1 e2) (eq? e1 e2))

(define (differentiate expr var)
  ;; To differentiate an expr within var. expr:expression to be differentiate; var:respect variable.
  ;; (list,symbol) -> (list)
  (cond ((number? expr) 0)
        ((symbol? expr) (if (same-symbol? expr var) 1 0))
        ((sum? expr) (make-sum (differentiate (addend expr) var)
                               (differentiate (augend expr) var)))
        ((product? expr) (make-sum (make-product (differentiate (multiplier expr) var)
                                                 (multiplicant expr))
                                   (make-product (multiplier expr)
                                                 (differentiate (multiplicant expr) var))))
        (else (begin (display "Error: unrecongnized symbol:\n")
                     (display "\t")
                     (display expr)
                     (newline)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sec2.3.3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sets objects

;; Operator
(define (element-of-set? e set)
       ;; Decide whether a element is in set.e:element;set:set
       ;; (number/symbol,list) -> (boolean)
       (cond ((null? set) #f)
             ((equal? e (car set)) #t)
             (else (element-of-set? e (cdr set)))))

;; Constructor
(define (make-set x . y)
  ;; Constructor for set
  (let iter ((result (list x))
             (rest y))
    (if (null? rest)
        result
        (if (element-of-set? (car rest) result)
            (iter result (cdr rest))
            (iter (cons (car rest) result) (cdr rest))))))
;; ;; Testing
;; (display (make-set 1 2 3 4 4 3 1 24 5))
;; (newline)
;; (display (make-set 1))
;; (newline)
;; (display (make-set 1 1))
;; (newline)
;; (display (make-set 1 2))
;; (newline)

(define empty-set (list))

;; Operator
(define (element-of-set? e set)
       ;; Decide whether a element is in set.e:element;set:set
       ;; (number/symbol,list) -> (boolean)
       (cond ((null? set) #f)
             ((equal? e (car set)) #t)
             (else (element-of-set? e (cdr set)))))

;; ;; Testting
;; (display (element-of-set? 1 (make-set 1)))
;; (newline)
;; (display (element-of-set? 'x (make-set 'x)))
;; (newline)
;; (display (element-of-set? 1 (make-set 2 1)))
;; (newline)
;; (display (element-of-set? 'x (make-set 'y 'x)))
;; (newline)
;; (display (element-of-set? 1 (make-set 2 2)))
;; (newline)
;; (display (element-of-set? 'x (make-set 'y 2)))
;; (newline)

(define (adjoin-set e set)
  ;; To join the element to set.e:element;set:set.
  ;; (number/symbol,list) -> (list)
  (if (element-of-set? e set)
      set
      (cons e set)))

(define (union-set s1 s2)
  ;; To union two sets.s1:set 1; s2:set2.
  ;; (list,list) -> (list)
  (if (null? s1)
      s2
      (let ((e1 (car s1))
            (r1 (cdr s1)))
        (adjoin-set e1 (union-set r1 s2)))))

(define (union-set2 s1 s2)
  ;; Union-set version 2
  (if (null? s1)
      s2
      (let ((e1 (car s1))
            (r1 (cdr s1)))
        (if (element-of-set? e1 s2)
            (union-set2 r1 s2)
            (union-set2 r1 (cons e1 s2))))))

(define (union-set3 s1 s2)
  ;; Union-set version 3
  (do ((left (cdr s1) (cdr left))
       (element (car s1) (car left))
       (result s2 (if (element-of-set? element result)
                      result
                      (cons element result))))
      ((null? left) (if (element-of-set? element result)
                        result
                        (cons element result)))))

;; ;; Testing
;; (define s1 (make-set 1 2 3 4 5 6))
;; (define s2 (make-set 1 2 3 7 8 9))
;; (define s3 (make-set 1 2 3))
;; (define s4 (make-set))

;; (display "Union Version 1:\n")
;; (display (union-set s1 s2))
;; (newline)
;; (display (union-set s2 s1))
;; (newline)
;; (display (union-set s1 s3))
;; (newline)
;; (display (union-set s3 s4))
;; (newline)

;; (display "Union Version 2:\n")
;; (display (union-set2 s1 s2))
;; (newline)
;; (display (union-set2 s2 s1))
;; (newline)
;; (display (union-set2 s1 s3))
;; (newline)
;; (display (union-set2 s3 s4))
;; (newline)

;; (display "Union Version 3:\n")
;; (display (union-set3 s1 s2))
;; (newline)
;; (display (union-set3 s2 s1))
;; (newline)
;; (display (union-set3 s1 s3))
;; (newline)
;; (display (union-set3 s3 s4))
;; (newline)

(define (intersect-set s1 s2)
  ;; To intersect two sets.s1:set 1;s2:set 2.
  ;; (list,list) -> (list)
  (if (null? s1)
      (list)
      (let ((e1 (car s1))
            (r1 (cdr s1)))
        (if (element-of-set? e1 s2)
            (cons e1 (intersect-set r1 s2))
            (intersect-set r1 s2)))))

(define (intersect-set2 s1 s2)
  ;; Intersect-set version 2
  (let iter ((e1 (car s1))
              (r1 (cdr s1)))
        (if (null? r1)
            (list)
            (if (element-of-set? e1 s2)
                (cons e1 (iter (car r1) (cdr r1)))
                (iter (car r1) (cdr r1))))))
;; Testing
;; (define s1 (make-set 1 2 3 4 5 6))
;; (define s2 (make-set 1 2 3 7 8 9))
;; (define s3 (make-set 1 2 3))
;; (define s4 (empty-set))

;; (display "Intersection Version 1:\n")
;; (display (intersect-set s1 s2))
;; (newline)
;; (display (intersect-set s2 s1))
;; (newline)
;; (display (intersect-set s1 s3))
;; (newline)
;; (display (intersect-set s3 s4))
;; (newline)

;; (display "Intersect Version 2:\n")
;; (display (intersect-set2 s1 s2))
;; (newline)
;; (display (intersect-set2 s2 s1))
;; (newline)
;; (display (intersect-set2 s1 s3))
;; (newline)
;; (display (intersect-set2 s3 s4))
;; (newline)

;; Ordered Sets with only numbers
(define (make-order-set x . y)
  ;; constructor for the ordered set.x,y: numbers.
  ;; (numbers*) -> (list)
  (merge-sort
   (let iter ((result (list x))
              (rest y))
     (if (null? rest)
         result
         (if (element-of-set? (car rest) result)
             (iter result (cdr rest))
             (iter (cons (car rest) result) (cdr rest)))))))

;; ;; Testing
;; (display (make-order-set 1 2 3 4 4 3 1 24 5))
;; (newline)
;; (display (make-order-set 1))
;; (newline)
;; (display (make-order-set 1 1))
;; (newline)
;; (display (make-order-set 1 2))
;; (newline)

(define (element-of-order-set? e s)
  ;; To see whethe element belongs to set.e:element;s:set.
  ;; (number,list) -> (boolean)
    (let ((slen (length s))
          (half (inexact->exact (floor (/ (length s) 2)))))
      ;; see how many elements are left
      (cond ((= slen 0) #f) ;; No element left
            ((= half 0) (if (= e (car s)) #t #f)) ;; Only one left
            (else ;; More than two left
             (let ((halfele (list-ref s half))) ;; get the middle element
               ;; Compare it to e
               (cond ((= halfele e) #t)
                     ((> halfele e)
                      (element-of-order-set? e
                                             (list-get s 0 half)))
                     ((< halfele e)
                      (element-of-order-set? e
                                             (list-get s (+ half 1) -1)))))))))

;; Test Version of element-of-order-set?
;; (define (element-of-order-set? e s)
;;   ;; To see whethe element belongs to set.e:element;s:set.
;;   ;; (number,list) -> (boolean)
;;   (let ((slen (length s))
;;         (half (inexact->exact (floor (/ (length s) 2)))))
;;     (begin (display "Enter function with:\tslen:")
;;            (display slen)
;;            (display " half:")
;;            (display half)
;;            (display " set:")
;;            (display s)
;;            (newline)
;;            (cond ((= slen 0) (begin (display "Empty Set!\n") #f))
;;                  ((= half 0) (begin (display "Element:")
;;                                     (display (car s))
;;                                     (newline)
;;                                     (if (= (car s) e) #t #f)))
;;                  (else (let ((halfele (list-ref s half)))
;;                          (begin (display "Decide:\thalfele:")
;;                                 (display halfele)
;;                                 (newline)
;;                                 (cond ((= halfele e) #t)
;;                                       ((> halfele e)
;;                                        (begin (display "Entering Former\n")
;;                                               (element-of-order-set?
;;                                         e
;;                                         (list-get s 0 half))))
;;                                       (else
;;                                        (begin (display "Entering Latter\n")
;;                                               (element-of-order-set?
;;                                         e
;;                                         (list-get s (+ half 1) -1))))))))))))

;; ;; Testing
;; (define os1 (make-order-set 1 2 3 4 5))
;; (define os2 (make-order-set 1 2 3 4))
;; (display (element-of-order-set? 1 os1))
;; (newline)
;; (display (element-of-order-set? 2 os1))
;; (newline)
;; (display (element-of-order-set? 3 os1))
;; (newline)
;; (display (element-of-order-set? 4 os1))
;; (newline)
;; (display (element-of-order-set? 5 os1))
;; (newline)
;; (display (element-of-order-set? 1 os2))
;; (newline)
;; (display (element-of-order-set? 2 os2))
;; (newline)
;; (display (element-of-order-set? 3 os2))
;; (newline)
;; (display (element-of-order-set? 4 os2))
;; (newline)

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

;; Testing
;; (define os1 (make-order-set 1 2 3 4 5))
;; (define os2 (make-order-set 1 2 3 4))
;; (define os3 (make-order-set 3 4 9 7 6))
;; (display (union-order-set os1 os2))
;; (newline)
;; (display (union-order-set os1 os3))
;; (newline)

(define (intersect-order-set s1 s2)
    ;; To intersect order set. s1: order set 1; s2: order set 2.
    ;; (list,list) -> (list)
    (if (or (null? s1) (null? s2))
        (list)
        (let ((e1 (car s1))
              (e2 (car s2)))
          (cond ((= e1 e2) (cons e1 (intersect-order-set
                                     (cdr s1)
                                     (cdr s2))))
                ((< e1 e2) (intersect-order-set
                            (cdr s1)
                            s2))
                (else (intersect-order-set
                       s1
                       (cdr s2)))))))

;; ;; Testing
;; (define os1 (make-order-set 1 2 3 4 5))
;; (define os2 (make-order-set 1 2 3 4))
;; (define os3 (make-order-set 3 4 9 7 6))
;; (display (intersect-order-set os1 os2))
;; (newline)
;; (display (intersect-order-set os1 os3))
;; (newline)

;; Tree Structure
;; Constructor
(define (make-tree entry left right)
    (list left entry right))

(define (make-leaf entry)
    (make-tree entry (list) (list)))

(define empty-tree (list))
;; Operator
(define empty-tree? null?)
(define entry cadr)
(define left-branch car)
(define right-branch caddr)

(define (element-of-tree? e t)
    ;; To test whether a element belongs to a tree. e: element; t:tree.
    ;; (number,list) -> (boolean)
    (if (empty-tree? t)
        #f
        (let ((node (entry t)))
          (cond ((= e node) #t)
                ((< e node) (element-of-tree? e (left-branch t)))
                ((> e node) (element-of-tree? e (right-branch t)))))))

(define (adjoin-tree e t)
    ;; To join an element to a tree. e:element; t:tree
    ;; (number,list) -> (list)
    (if (empty-tree? t)
        (make-leaf e)
        (let ((node (entry t)))
          (cond ((= e node) t)
                ((< e node)
                 (make-tree node
                            (adjoin-tree e (left-branch t))
                            (right-branch t)))
                ((> e node)
                 (make-tree node
                            (left-branch t)
                            (adjoin-tree
                             e
                             (right-branch t))))))))

;; ;; Testing
;; (define tree1 (make-leaf 10))
;; (define tree1 (adjoin-tree 4 tree1))
;; (define tree1 (adjoin-tree 39 tree1))
;; (define tree1 (adjoin-tree 9 tree1))
;; (define tree1 (adjoin-tree 3 tree1))
;; (define tree1 (adjoin-tree 0 tree1))
;; (define tree1 (adjoin-tree 21 tree1))

;; (display tree1)
;; (newline)
;; (display (element-of-tree? 10 tree1))
;; (newline)
;; (display (element-of-tree? 4 tree1))
;; (newline)
;; (display (element-of-tree? 39 tree1))
;; (newline)
;; (display (element-of-tree? 9 tree1))
;; (newline)
;; (display (element-of-tree? 3 tree1))
;; (newline)
;; (display (element-of-tree? 0 tree1))
;; (newline)
;; (display (element-of-tree? 21 tree1))
;; (newline)
