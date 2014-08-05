(load "../BookExercise/basic")
(define (loadlec12-5) (load "lec12-5"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; good queue
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; tags
(define tag-queue 'queue)

;; type
;; For the queue ADT, the whole data
(define queue-data-type list)
(define queue-data-type? list?)

;; For the front rear pointer in the ADT
(define queue-front-rear-type cons)

;; For the real queue insdie ADT
(define queue-element-type list)
(define queue-element-type? list?)

;; Selector
;; Acting on queue ADT
(define queue-tag car)
(define queue-element cadr)
(define queue-element-front-rear cdr)
(define queue-element-rear cddr)

;; Acting on real queue elements
(define queue-element-rest cdr)
(define queue-element-first car)

;; Constructor
(define (queue-make)
  (let ((q (queue-data-type tag-queue (queue-element-type))))
    ;; Link back the rear to front
    (queue-set-element-rear (queue-element-front-rear q)
                            (queue-element q))
    q))

;; Error
(define (queue-empty-error info)
  (errormsg "Empty Queue!" info))

(define (queue-not-error q)
  (errormsg "Not a Queue!" q))

;; Operator
(define queue-element-empty? null?)

(define (queue? q)
  (and (queue-data-type? q) (eq? tag-queue (queue-tag q))))

(define (queue-empty? q)
  (if (queue? q)
      (queue-element-empty? (queue-element q))
      (queue-not-error q)))

;; Mutator
(define queue-set-element-front set-car!)
(define queue-set-element-rear set-cdr!)

(define (queue-insert ele q)
  (if (queue? q)
      (let ((newelement (queue-element-type ele))
            (front-rear (queue-element-front-rear q)))
        ;; See whether the queue is empty
        (if (queue-element-empty? (queue-element q))
            ;; Link both front and rear to the newelement if the queue is empty
          (begin
            (queue-set-element-front front-rear newelement)
            (queue-set-element-rear front-rear newelement))
          (let ((rear (queue-element-rear q)))
            (queue-set-element-rear rear newelement)
            (queue-set-element-rear front-rear newelement)))
        ;; Return queue
        q)
      (queue-not-error q)))

(define (queue-delete q)
  (if (queue-empty? q)
      (queue-empty-error "Can't delete element in an empty queue!")
      (let ((newelements (queue-element-rest (queue-element q)))
            (front-rear (queue-element-front-rear q)))
        ;; Link front to the new queue
        (queue-set-element-front front-rear newelements)
        ;; See whether the new queue is empty
        (if (queue-element-empty? newelements)
            ;; Link rear to the front if so
            (queue-set-element-rear front-rear newelements))
        ;; Return queue
        q)))

(define (queue-pop q)
  (if (queue-empty? q)
      (queue-empty-error "Can't pop element from an empty queue!")
      (let ((newelements (queue-element-rest (queue-element q)))
            (popelement (queue-element-first (queue-element q)))
            (front-rear (queue-element-front-rear q)))
        ;; link the front to the new queue
        (queue-set-element-front front-rear newelements)
        ;; See whether the new queue is empty
        (if (queue-element-empty? newelements)
            ;; Link rear to the front is so
            (queue-set-element-rear front-rear newelements))
        ;; Return the pop element
        popelement)))

;; ;; Test
;; (define q (queue-make))
;; (print-out (queue-tag q))
;; (print-out (queue-element q))
;; (print-out (queue-element-front-rear q))
;; (print-out (queue-element-rear q))

;; (print-out (queue-insert 1 q))
;; (print-out (queue-insert 2 q))
;; (print-out (queue-tag q))
;; (print-out (queue-element q))
;; (print-out (queue-element-front-rear q))
;; (print-out (queue-element-rear q))

;; (print-out (queue-delete q))
;; (print-out (queue-tag q))
;; (print-out (queue-element q))
;; (print-out (queue-element-front-rear q))
;; (print-out (queue-element-rear q))

;; (print-out (queue-delete q))
;; (print-out (queue-tag q))
;; (print-out (queue-element q))
;; (print-out (queue-element-front-rear q))
;; (print-out (queue-element-rear q))

;; ;; (print-out (queue-pop q))
;; ;; (print-out (queue-tag q))
;; ;; (print-out (queue-element q))
;; ;; (print-out (queue-element-front-rear q))
;; ;; (print-out (queue-element-rear q))

;; (print-out (queue-pop q))
;; (print-out (queue-tag q))
;; (print-out (queue-element q))
;; (print-out (queue-element-front-rear q))
;; (print-out (queue-element-rear q))

;; (print-out (queue-insert 1 q))
;; (print-out (queue-tag q))
;; (print-out (queue-element q))
;; (print-out (queue-element-front-rear q))
;; (print-out (queue-element-rear q))
