(load "../BookExercise/basic")
(define (loadlec12-6) (load "lec12-6"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; short good queue
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tag
(define tag-queue 'queue)

;; Constructor
(define (queue-make)
  (let ((tmp (list tag-queue (list))))
    (set-cdr! (cdr tmp) (cadr tmp))
    tmp))

;; Error
(define (queue-not-error q)
  (errormsg "Not a Queue Object!" q)
  #f)

(define (queue-empty-error location)
  (errormsg "Empty Queue!" location)
  #f)

(define (queue-no-rear-error q)
  (errormsg "Queue with no Rear!" q)
  #f)

;; Operator
(define (queue-status? q sym len)
  (and (list? q)
       (sym (length q) len)
       (eq? tag-queue (car q))))

(define (queue? q)
  (queue-status? q > 1))

;; Selector Generator
(define (queue-selector-generator sel)
  (lambda (q)
    (if (queue? q)
        (sel q)
        (queue-not-error q))))

;; Selectors
(define queue-tag (queue-selector-generator car))
(define queue-pointer (queue-selector-generator cdr))
(define queue-elements (queue-selector-generator cadr))
(define (queue-rear q)
  (if (queue-status? q = 3)
      (cddr q)
      (queue-no-rear-error q)))

;; Operator
(define (queue-empty? q)
  (let ((elements (queue-elements q)))
    (if elements
        (null? elements))))

;; Selector
(define (queue-elements-front q)
  (if (queue-empty? q)
      (queue-empty-error "Can't get the front element of the queue.")
      (caadr q)))

;; Mutator
(define (queue-set-generator queue-location set-location)
  (lambda (ele q)
    (let ((pointer (queue-location q)))
      (if pointer
          (set-location pointer ele)))))

(define queue-set-elements
  (queue-set-generator queue-pointer set-car!))

(define queue-set-rear
  (queue-set-generator queue-pointer set-cdr!))

(define queue-set-elements-rear
  (queue-set-generator queue-rear set-cdr!))

(define (queue-insert ele q)
  (let ((newele (list ele))
        (elements (queue-elements q)))
    (if elements
        (if (null? elements)
            (begin (queue-set-elements newele q)
                   (queue-set-rear newele q)
                   q)
            (begin (queue-set-elements-rear newele q)
                   (queue-set-rear newele q)
                   q)))))
;; Mutator
(define (queue-delete-generator return name)
  (lambda (q)
    (if (queue-empty? q)
        (queue-empty-error (string-append "Can't " name "!"))
        (let ((elements (queue-elements q)))
          (let ((first (car elements))
                (rest (cdr elements)))
            (queue-set-elements rest q)
            (if (null? rest)
                (queue-set-rear rest q))
            (return (list q first)))))))

(define queue-delete (queue-delete-generator
                      (lambda (l) (car l)) "delete"))
(define queue-pop (queue-delete-generator
                   (lambda (l) (cadr l)) "pop"))

;; ;; Test
;; (define q (queue-make))
;; (print-out q)
;; (print-out (queue-insert 1 q))
;; (print-out (queue-insert 2 q))
;; (print-out (queue-delete q))
;; (print-out (queue-pop q))
;; (print-out (queue-delete q))
;; (print-out (queue-pop q))
;; (print-out (queue-insert 3 q))
;; (print-out "---selector---")
;; (print-out (queue-tag q))
;; (print-out (queue-pointer q))
;; (print-out (queue-elements q))
;; (print-out (queue-rear q))
;; (print-out (queue-elements-front q))
