(load "../BookExercise/basic")
(define (loadlec11-1) (load "lec11-1"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simple Tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (insert-assoc key value table)
    ;; Inserting a binding into a table.
    ;; (obj,obj,list) -> (list)
    (cons (list key value) table))

(define (search-assoc key table)
    ;; Searching for the value of a key in a table.
    ;; (obj,list) -> (obj/boolean)
    (cond ((null? table) #f)
          ((equal? key (caar table)) (cadar table))
          (else (search-assoc key (cdr table)))))

;; ;; Test
;; (define t (list))
;; (define t (insert-assoc 'x 15 t))
;; (define t (insert-assoc 'y 20 t))
;; (display t)
;; (newline)
;; (display (search-assoc 'x t))
;; (newline)
;; (display (search-assoc 'y t))
;; (newline)
;; (display (search-assoc 'z t))
;; (newline)
