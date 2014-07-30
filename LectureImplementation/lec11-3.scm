(load "../BookExercise/basic")
(define (loadlec11-3) (load "lec11-3"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ADT table with hash function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tag-hash-table 'hash-table)

;; Constructor
(define (hash-table-make bucket-size hash-function)
    (list tag-hash-table bucket-size hash-function
        (make-vector bucket-size (list))))

(define table-tag-of car)
(define bucket-size-of cadr)
(define hash-function-of caddr)
(define buckets-of cadddr)

(define (hash-table-insert key val tab)
    ;; Insert the binding of key and value into a table.
    ;; (obj,obj,list) -> (list)
    (let ((index
           ((hash-function-of tab) key))
          (buckets (buckets-of tab)))
      (vector-set! buckets index (cons (list key val)
                                       (vector-ref buckets index)))))

(define (hash-table-search key tab)
    ;; Search the value of binding with value key.
    ;; (obj,list) -> (obj)
    (let ((index
           ((hash-function-of tab) key)))
      (let iter ((target-bucket
                  (vector-ref (buckets-of tab) index)))
           (if (null? target-bucket)
               #f
               (let ((firstkey (caar target-bucket)))
                 (if (equal? firstkey key)
                     (cadar target-bucket)
                     (iter (cdr target-bucket))))))))

(define (hash-for-num size)
    ;; Return a function of modular size
    (lambda (num) (remainder num size)))


;; ;; Test
;; (define m4tab (hash-table-make 4 (hash-for-num 4)))
;; (print-out m4tab)
;; (hash-table-insert 15 'fifteen m4tab)
;; (print-out m4tab)
;; (hash-table-insert 13 'ninteen m4tab)
;; (print-out m4tab)
;; (hash-table-insert 27 'twenty-seven m4tab)
;; (print-out m4tab)
;; (print-out (hash-table-search 15 m4tab))
;; (print-out (hash-table-search 13 m4tab))
;; (print-out (hash-table-search 16 m4tab))
