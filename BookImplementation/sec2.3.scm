(load "../BookExercise/basic")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section 2.3.1 quotations
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
;; section 2.3.2 deriverative
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
;; Section 2.3.3
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section 2.3.4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Implementation of the Huffman trees

;; order pair set to represent the symbols

;;Constructor
(define (make-symbol-set weight symbols)
    ;; Construct weight symbol set. weight; weight of the symbol set; symbols: symbole or symbol set.
    ;; (number,symbole/list) -> (list)
    (if (list? symbols)
        (list weight symbols)
        (list weight (list symbols))))

;; Test
(define aa (make-symbol-set 8 'a))
(define bb (make-symbol-set 3 'b))
(define cc (make-symbol-set 1 'c))
(define dd (make-symbol-set 1 'd))
(define ee (make-symbol-set 1 'e))
(define ff (make-symbol-set 1 'f))
(define gg (make-symbol-set 1 'g))
(define hh (make-symbol-set 1 'h))


;; Selector
(define get-weight car)
(define get-symbol-set cadr)

;; Operator
(define (join-symbol-set ss1 ss2)
    (make-symbol-set (+ (get-weight ss1)
                        (get-weight ss2))
                     (append (get-symbol-set ss1)
                             (get-symbol-set ss2))))

;; Test
;; (display (get-weight aa))
;; (newline)
;; (display (get-symbol-set aa))
;; (newline)
;; (display (join-symbol-set aa bb))
;; (newline)

;; Huffman tree
;; Constructor
(define make-huffman-node list)

(define (make-huffman-leaf ss)
    (make-huffman-node ss (list) (list)))

(define empty-huffman-leaf
    (make-huffman-leaf (make-symbol-set 0 (list))))

;; Selector
(define get-huffman-entry car)
(define get-huffman-left cadr)
(define get-huffman-right caddr)

;; Operator
(define (huffman-leaf? node)
    (and (null? (get-huffman-left node))
         (null? (get-huffman-right node))))

(define (huffman-has-left? node)
    (not (null? (get-huffman-left node))))

(define (huffman-has-right? node)
    (not (null? (get-huffman-right node))))

(define (get-node-weight node)
    (get-weight (get-huffman-entry node)))

(define (get-node-symbol-set node)
    (get-symbol-set (get-huffman-entry node)))

(define (merge-node node1 node2)
    (let ((e1 (get-huffman-entry node1))
          (e2 (get-huffman-entry node2)))
      (make-huffman-node (join-symbol-set e1 e2)
                         node1
                         node2)))

;; Test
(define la (make-huffman-leaf aa))
(define lb (make-huffman-leaf bb))
(define lc (make-huffman-leaf cc))
(define ld (make-huffman-leaf dd))
(define le (make-huffman-leaf ee))
(define lf (make-huffman-leaf ff))
(define lg (make-huffman-leaf gg))
(define lh (make-huffman-leaf hh))

;; (display (huffman-leaf? la))
;; (newline)
;; (display (get-huffman-entry la))
;; (newline)
;; (display (get-huffman-left la))
;; (newline)
;; (display (get-huffman-right la))
;; (newline)
;; (display (get-node-weight la))
;; (newline)
;; (display (get-node-symbol-set la))
;; (newline)
;; (display (merge-node la lb))
;; (newline)

;; Rewrite order set for the symbol set
;; constructor
(define make-node-order-set list)

;; Testing
(define nodeos (make-node-order-set la lb lc ld le lf lg lh))

;; selector
(define (get-two-min-weight-node node-order-set)
    ;; To get the two minimum weight huffman tree nodes from the order set. ss-order-set: symbol set order set.
    ;; (list) -> (list)
    (list-get node-order-set -3 -1))

;; ;; Testing
;; (display (get-two-min-weight-node nodeos))
;; (newline)

(define (remove-two-min-weight-node node-order-set)
    ;; To remove the two minimum weight huffman tree nodes from the order set. ss-order-set: symbol set order set.
    ;; (list) -> (list)
    (list-get node-order-set 0 -3))

;; ;; Testing
;; (display (remove-two-min-weight-node nodeos))
;; (define nodeos (remove-two-min-weight-node nodeos))
;; (newline)
;; (display (remove-two-min-weight-node nodeos))
;; (define nodeos (remove-two-min-weight-node nodeos))
;; (newline)

(define (adjoin-node-order-set node node-order-set)
    ;; To adjoin a huffman tree node to the tree node order set.node: tree node; node-order-set: symbol set order set.
    (if (null? node-order-set)
        (list node)
        (let ((half (inexact->exact (floor
                                     (/ (length node-order-set)
                                        2)))))
          (let ((halfnode (list-ref node-order-set half)))
            (let ((w1 (get-node-weight node))
                  (w2 (get-node-weight halfnode)))
              (cond ((= w1 w2) (append
                                    (list-get
                                     node-order-set 0 half)
                                    (list node)
                                    (list-get
                                     node-order-set half -1)))
                    ((> w1 w2) (append
                                (adjoin-node-order-set
                                 node
                                 (list-get
                                  node-order-set 0 half))
                                (list-get node-order-set half -1)))
                    ((< w1 w2) (append
                                (list-get node-order-set
                                          0
                                          (+ half 1))
                                (adjoin-node-order-set
                                 node
                                 (list-get
                                  node-order-set
                                  (+ half 1)
                                  -1))))))))))

;; ;; Testing
;; (display (adjoin-node-order-set (make-huffman-leaf (make-symbol-set 8 'test)) nodeos))
;; (newline)
;; (display (adjoin-node-order-set (make-huffman-leaf (make-symbol-set 3 'test)) nodeos))
;; (newline)
;; (display (adjoin-node-order-set (make-huffman-leaf (make-symbol-set 1 'test)) nodeos))
;; (newline)
;; (display (adjoin-node-order-set (make-huffman-leaf (make-symbol-set 7 'test)) nodeos))
;; (newline)
;; (display (adjoin-node-order-set (make-huffman-leaf (make-symbol-set 2 'test)) nodeos))
;; (newline)
;; (display (adjoin-node-order-set (make-huffman-leaf (make-symbol-set 0 'test)) nodeos))
;; (newline)

(define (generate-huffman-tree node-order-set)
    ;; Generate the huffman tree using the node order set. node-order-set: node order set, initialized with huffman-leaf.
    ;; (list) -> (list)
    (if (= (length node-order-set) 1)
        (car node-order-set)
        (let ((two-small-node (get-two-min-weight-node
                               node-order-set))
              (new-node-order-set (remove-two-min-weight-node
                                   node-order-set)))
          (generate-huffman-tree
           (adjoin-node-order-set
            (merge-node (car two-small-node)
                        (cadr two-small-node))
            new-node-order-set)))))

(define huffmantree (generate-huffman-tree nodeos))
;; (display huffmantree)
;; (newline)

(define (belong-to-set? symbol symbol-set)
    ;; Test whether a symbol belongs to a symbol set. symbol:symbol; symbol-set: symbol set.
    ;; (symbol,list) -> (boolean)
    (cond ((null? symbol-set) #f)
          ((eq? symbol (car symbol-set)) #t)
          (else (belong-to-set? symbol (cdr symbol-set)))))

(define (belong-to-node? symbol node)
    ;; Huffman tree node version
    (belong-to-set? symbol (get-node-symbol-set node)))

;; Test
;; (define rootright (get-huffman-right huffmantree))
;; (display rootright)
;; (newline)
;; (display (belong-to-node? 'a rootright))
;; (newline)

(define (encode-symbol-huffman-tree symbol huffman-tree)
    ;; Encode the symbol given a huffman tree. Left as 0, right as 1. symbol: symbol to be encode; huffman-tree: huffman tree.
    ;; (symbol,list) -> (list)
    (if (huffman-leaf? huffman-tree)
        (list)
        (let ((leftnode (if (huffman-has-left? huffman-tree)
                            (get-huffman-left huffman-tree)
                            empty-huffman-leaf))
              (rightnode (if (huffman-has-right? huffman-tree)
                            (get-huffman-right huffman-tree)
                            empty-huffman-leaf)))
          (cond ((belong-to-node? symbol leftnode)
                 (cons 1 (encode-symbol-huffman-tree
                          symbol
                          leftnode)))
                ((belong-to-node? symbol rightnode)
                 (cons 0 (encode-symbol-huffman-tree
                          symbol
                          rightnode)))
                (else (begin
                       (errormsg "encode-symbol-huffman-tree"
                                 (list symbol huffman-tree))))))))

;; Test
;; (display (encode-symbol-huffman-tree 'a huffmantree))
;; (newline)

(define (encode-string-huffman-tree string-list huffman-tree)
    ;; Encode the string list with the given huffman tree. string-list: string list; huffman-tree: huffman tree.
    ;; (list,list) -> (list)
    (let iter ((sl string-list))
         (if (null? sl)
             (list)
             (append (encode-symbol-huffman-tree
                      (car sl)
                      huffman-tree)
                     (iter (cdr sl))))))
;; ;; Test
;; (define code (encode-string-huffman-tree '(h e a d) huffmantree))
;; (display code)
;; (newline)

(define (decode-bit-huffman-tree bit-list huffman-tree)
    ;; Decode the bit list with  the given huffman tree. bit-list: bit list; huffman-tree: huffman tree.
    (let iter ((current-tree huffman-tree)
               (current-bit bit-list))
         (cond ((huffman-leaf? current-tree)
                (append (get-node-symbol-set current-tree)
                        (iter huffman-tree current-bit)))
               ((null? current-bit) (list))
               ((= (car current-bit) 1)
                (iter (get-huffman-left current-tree)
                      (cdr current-bit)))
               ((= (car current-bit) 0)
                (iter (get-huffman-right current-tree)
                      (cdr current-bit)))
               (else
                (errormsg "decode-bit-huffman-tree"
                          (list current-tree
                                current-bit))))))

;; (display (decode-bit-huffman-tree code
;;                                  huffmantree))
;; (newline)
