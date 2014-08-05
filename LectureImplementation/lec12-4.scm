(load "../BookExercise/basic")
(define (loadlec12-4) (load "lec12-4"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bad queue
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Constructor
(define queue list)

;; Operator
(define queue-empty? null?)

(define (queue-delete q)
  (if (queue-empty? q)
      (errormsg "Empty queue!" q)
      (cdr q)))

(define (queue-pop q)
  (if (queue-empty? q)
      (errormsg "Empty queue!" q)
      (car q)))

(define (queue-insert ele q)
  (let iter ((rest q))
    (if (queue-empty? rest)
        (list ele)
        (cons (car rest) (iter (cdr rest))))))

;; Test
(define q1 (queue 1 2 3 4))
(define q2 (queue))
(print-out (queue-delete q1))
(print-out (queue-delete q2))
(print-out (queue-pop q1))
(print-out (queue-pop q2))
(print-out (queue-insert 5 q1))
