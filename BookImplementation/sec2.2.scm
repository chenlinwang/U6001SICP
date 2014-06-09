;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; section 2.1.1
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
(do ((index 0 (begin (display index)
                     (display ":")
                     (display (own-list-ref l index))
                     (newline)
                     (+ index 1))))
    ((> index 5) (newline)))
