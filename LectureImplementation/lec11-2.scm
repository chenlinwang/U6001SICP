(load "../BookExercise/basic")
(define (loadlec11-2) (load "lec11-2"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ADT table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tag-table 'table)

;; Constructor
(define (table-make) (list tag-table))

(define (table-insert val key tab)
    ;; Insert the binding of key and value into a table.
    ;; (obj,obj,list) -> (list)
    (set-cdr! tab (cons (list val key) (cdr tab))))

(define (table-search key tab)
    ;; Search the value of binding with value key.
    ;; (obj,list) -> (obj)
    (let iter ((tablist (cdr tab)))
         (if (null? tablist)
             #f
             (let ((firstkey (caar tablist)))
               (if (equal? key firstkey)
                   (cadar tablist)
                   (iter (cdr tablist)))))))

;; ;; Test
;; (define t (table-make))
;; (table-insert 'x 15 t)
;; (table-insert 'y 20 t)
;; (display t)
;; (newline)
;; (display (table-search 'x t))
;; (newline)
;; (display (table-search 'y t))
;; (newline)
;; (display (table-search 'z t))
;; (newline)
