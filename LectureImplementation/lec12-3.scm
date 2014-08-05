(load "../BookExercise/basic")
(define (loadlec12-3) (load "lec12-3"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Correct stack
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; tag name
(define tag-stack 'stack)

;; Implemented data type
(define stack-type list)
(define stack-type? list?)

;; Constructor
(define (stack-make) (stack-type tag-stack))

;; Selector
(define stack-tag car)
(define stack-element cdr)
(define stack-element-first cadr)
(define stack-element-rest cddr)

;; Operator
(define (stack-not-error stack)
  (errormsg "Not a stack object!" stack))

(define (stack-empty-error info)
  (errormsg "Stack empty!" info))
(define stack-element-add cons)


(define (stack? stack)
  (and (stack-type? stack) (eq? 'stack (stack-tag stack))))

(define (stack-empty? stack)
  (if (not (stack? stack))
      (stack-not-error stack)
      (null? (stack-element stack))))

;; Mutator
(define stack-mutate-tag set-car!)
(define stack-mutate-element set-cdr!)

(define (stack-insert ele stack)
  (if (stack? stack)
      (stack-mutate-element
       stack
       (stack-element-add
        ele
        (stack-element stack)))
      (stack-not-error stack)))

(define (stack-pop stack)
  (if (stack-empty? stack)
      (stack-empty-error "Can't not pop an element!")
      (let ((popele (stack-element-first stack)))
        (stack-mutate-element
         stack
         (stack-element-rest stack))
        popele)))

(define (stack-delete stack)
  (if (stack-empty? stack)
      (stack-empty-error "Can't not delete an element!")
      (stack-mutate-element
       stack
       (stack-element-rest stack))))

;; Test
(define s (stack-make))
;; (print-out (stack? s))
;; (print-out (stack-empty? s))


;; ;; For pop and delete
;; (stack-pop s)
;; (stack-delete s)

;; ;; For Inserting element
;; (let iter ((rest (list 1 2 3 4)))
;;   (if (null? rest)
;;       #f
;;       (let ((first (car rest)))
;;         (stack-insert first s)
;;         (print-out s)
;;         (iter (cdr rest)))))

;; ;; For pop and delete
;; (let iter ((rest (list 1 2)))
;;   (if (null? rest)
;;       #f
;;       (begin (print-out (stack-pop s))
;;              (stack-delete s)
;;              (print-out s)
;;              (iter (cdr rest)))))
