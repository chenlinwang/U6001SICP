;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; section 2.2.1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This part mainly contrains functions of list, we will try to implement all the functions that concerns list.
;; Constructor
;; Oh, I have not learned how to write functions with undetermined numbers of input. So skip it.

;; length
(define (own-length l)
    ;; It returns the length of a list.
    ;; l:list.
    ;; (list) -> number
    (define (oliter l n)
        ;; Iterative returns the length of a list.
        ;; l: current left list,n: read element number
        ;; (list number) -> (number)
        (if (null? l)
            n
            (oliter (cdr l) (+ n 1))))
  ;; Starting from n=0
  (oliter l 0))

;; list-ref
(define (own-list-ref l n)
    ;; It gets the nth element of a list. l is a list and n is a non-negtive number
    ;; (list, number) -> (A)
    ;; n >= 0
    (define (olriter l n)
        ;; It gets the nth element of a list. l is a list and n is a non-negtive number
        ;; (list, number) -> (A)
        ;; n >= 0
        (if (= n 0)
            (car l)
            (olriter (cdr l) (- n 1)))
)
  (let ((ll (- (own-length l) 1)))
    (cond ((< n 0) (begin (display "Negtive Number Input, Put Positive For N Please!\n")
                          #f))
          ((> n ll) (begin (display "Exceeding The Maximum Length, Put Number Less Than")
                            (display ll)
                            (newline)
                            #f))
          (else (olriter l n)))))

;; append
(define (own-append l1 l2)
    ;; Append the two list and return a single list
    ;; (list,list) -> (list)
    (if (null? l1)
        l2
        (cons (car l1) (own-append (cdr l1) l2))))

;; Testing
(define l (list 1 2 3 4 5 6))
;; own-list-ref
;; (do ((index 0 (begin (display index)
;;                      (display ":")
;;                      (display (own-list-ref l index))
;;                      (newline)
;;                      (+ index 1))))
;;     ((> index 5) (newline)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; section 2.2.2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (counting-lieves l)
    ;; Counting the lieves of a tree. l: input tree
    ;; (list) -> (number)
    (cond ((null? l) 0)
          ((not (pair? l)) 1)
          (else (+ (counting-lieves (car l))
                   (counting-lieves (cdr l))))))

;; (define t (list 1 2 3 (list 4 5 6 (list 7 8 (list 9)))))
;; (display (counting-lieves t))
;; (newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; section 2.2.3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sum odd square lieves
(define (empty-list? l) (null? l))
(define (element? l) (not (pair? l)))
(define (odd? n) (= 1 (remainder n 2)))
(define (square n) (* n n))

(define (sum-odd-tree t)
    ;; Sum the square of odd lieves of the tree. t: tree-structured list.
    ;; (list) -> (number)
    (cond ((empty-list? t) 0)
          ((element? t) (if (odd? t) (square t) 0))
          (else (+ (sum-odd-tree (car t)) (sum-odd-tree (cdr t))))))

(define t2231 (list 1 (list 2 3 (list 4 5)) (list 6 7)))
;; (display (sum-odd-tree t2231))
;; (newline)
;; For fibs
(load "sec1.2")
(define (even? n) (= 0 (remainder n 2)))
(define (even-fib n)
    ;; Return the list of even fibonacci numbers until the nth. n: number.
    ;; (number) -> (list)
    (if (= n 0)
        (list)
        (let ((fb (Fib n)))
          (if (even? fb)
              (append (even-fib (- n 1)) (list fb))
              (even-fib (- n 1))))))

;; (display (even-fib 10))
;; (newline)
;; (display (even-fib 11))
;; (newline)
;; (display (even-fib 12))
;; (newline)
;; (display (even-fib 13))
;; (newline)

(define (filterout filter)
    ;; return a function that cout apply the filter to a list and filter out elements. fileter: a filter function.
    ;; (A-> boolean) -> (list->list)
    (define (rf l)
        ;; Return function that use filter to filter out l. l: input list.
        ;; (list) -> (list)
        (cond ((empty-list? l) (list))
              ((filter (car l))
               (cons (car l) (rf (cdr l))))
              (else
               (rf (cdr l)))))
  rf)

;; (display ((filterout odd?) (list 1 2 3 4 5 6)))
;; (newline)

(define (cr enumerate modify filter accumulate initial)
    ;; conceptual representation of the process of enumerate, modify filter and accumulate. The former four elements are procedures and the last one is a initial.
    ;; (A->list,B->C,B->boolean,C->D,C) -> D               (filterout filter)
    (let ((filterfunc (filterout filter)))
      (lambda (n)
        (accumulate initial
                    (map modify
                         (filterfunc (enumerate n)))))))

(define (enumerate-tree t)
    ;; get lieves nodes of a tree as a list. t: tree-structed list.
    ;; (list) -> (list)
    (cond ((empty-list? t) (list))
          ((element? t) (list t))
          (else (append (enumerate-tree (car t))
                        (enumerate-tree (cdr t))))))
;; (display (enumerate-tree t2231))
;; (newline)
(define (sum init inputlist)
    ;; Add all element of inputlist to init. init:number; inputlist:list of number.
    ;; (number,list) -> (number)
    (if (empty-list? inputlist)
        init
        (sum (+ init (car inputlist)) (cdr inputlist))))
;; (display (sum 0 (list 1 2 3 4 5)))
;; (newline)

(define sum-odd-tree2 (cr enumerate-tree square odd? sum 0))
;; (display (sum-odd-tree2 t2231))
;; (newline)

(define (enumerate-range n)
    ;; Return a list of number from 1 to n. n: positve number.
    ;; (number) -> (list)
    (if (= 1 n)
        (list 1)
        (append (enumerate-range (- n 1)) (list n))))

(define (fib-even? n) (even? (Fib n)))
(define even-fib2 (cr enumerate-range Fib fib-even? append (list)))
;; (display (even-fib2 12))
;; (newline)
