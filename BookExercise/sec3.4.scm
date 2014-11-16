(load "../BookExercise/basic")
(load "../LectureImplementation/lec22")
(define (loadexe34) (load "../BookExercise/sec3.4"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3.47
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A semaphore (of size n) is a generalization of a mutex. Like a mutex, a semaphore supports acquire and release operations, but it is more general in that up to n processes can acquire it concurrently. Additional processes that attempt to acquire the semaphore must wait for release operations. Give implementations of semaphores.
;; a. in terms of mutexes
;; b. in terms of atomic test-and-set! operations.

;; a it is very stupid to use the mutex to implement the semaphore.
;; b.

(define (make-semaphore n)
  (let ((the-semaphore (cons 0 n)))
    (define (dispatch m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! the-semaphore)
                 (dispatch 'acquire)))
            ((eq? m 'release)
             (clear-semaphore the-semaphore))
            (else
             (error "undefined operator -- " m))))
    dispatch))

(define (test-and-set! the-semaphore)
  (cond ((= (car the-semaphore)
            (cdr the-semaphore))
         #t)
        (else (set-car! the-semaphore
                        (+ 1 (car the-semaphore)))
              #f)))

(define (clear-semaphore the-semaphore)
  (set-car! the-semaphore (- (car the-semaphore) 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3.48
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Explain in detail why the deadlock-avoidance method described above, (i.e., the accounts are numbered, and each process attempts to acquire the smaller-numbered account first) avoids deadlock in the exchange problem. Rewrite serialized-exchange to incorporate this idea. (You will also need to modify make-account so that each account is created with a number, which can be accessed by sending an appropriate message.)

;; the reason why two will become dead lock is that two procedures in the same serialization would lock the one of the needed accounts and wait for the other to unlock, which takes forever.

(define (ordered-serialized-exchange-account a1 a2)
  ((let ((s (if (< (a1 'number)
                     (a2 'number))
                 (cons (a1 'serializer)
                       (a2 'serializer))
                 (cons (a2 'serializer)
                       (a1 'serializer)))))
     (((car s) ((cdr s) exchange-account)) a1 a2))))
