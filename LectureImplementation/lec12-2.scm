(load "../BookExercise/basic")
(define (loadlec12-2) (load "lec12-2"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bad stack
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Constructor
(define stack-make list)

;; Operator
(define stack-empty? null?)

(define (stack-pop stack)
    (if (stack-empty? stack)
        (errormsg "Empty Stack" stack)
        (car stack)))

(define (stack-insert ele stack)
    (cons ele stack))

(define (stacj-delete stack)
    (if (stack-empty? stack)
        (errormsg "Empty Stack" stack)))
