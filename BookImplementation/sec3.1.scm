(load "../BookExercise/basic")
(define (loadimp31) (load "../BookImplemetation/sec3.1.scm"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; section 3.1.2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require-extension random-mtzig)

(define random-init 1234)

(define (set-random-init seed)
  (set! random-init seed))

(define rand
  (let ((random-seed (random-mtzig:init random-init)))
    (lambda () (random-mtzig:random! random-seed))))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (let iter ((remaining-trials trials)
             (trials-passed 0))
    (cond ((= remaining-trials 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- remaining-trials 1)
                 (+ trials-passed 1)))
          (else
           (iter (- remaining-trials 1)
                 trials-passed)))))
;; Test
;; (print-out (estimate-pi 10000))


;; (exit)
