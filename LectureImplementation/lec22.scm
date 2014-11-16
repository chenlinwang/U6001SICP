(load "../BookExercise/basic")
(define (loadlec22) (load "../LectureImplementation/lec22"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bank account revisit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-account balance)
  (define (deposite amount)
    (set! balance (+ balance amount))
    balance)
  (define (withdraw amount)
    (cond ((> amount balance)
           'not-enough-balance)
          (else
           (set! balance (- balance amount))
           balance)))
  (define (dispatch m)
    (cond ((eq? m 'deposite) deposite)
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'balance) balance)
          (else (error "undefined operator -- " m))))
  dispatch)

;; ;; test
;; (define paul (make-account 100))
;; (print ((paul 'withdraw) 200))
;; (print ((paul 'deposite) 100))
;; (print (paul 'balance))
;; (exit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the serializer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-mutex
(define (make-mutex)
  (let ((the-mutex (list #f)))
    (define (dispatch m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! the-mutex)
                 (dispatch 'acquire)))
            ((eq? m 'release)
             (set-car! the-mutex #f))
            (else
             (error "undefined operator -- " m))))
    dispatch))

(define (test-and-set! the-mutex)
  (cond ((car the-mutex) #t)
        (else (set-car! the-mutex #t)
              #f)))

;; the serialization
(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((result (apply p args)))
          (mutex 'release)
          result))
      serialized-p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the new bank account
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (make-serialized-account balance)
  (define (deposite amount)
    (set! balance (+ balance amount))
    balance)
  (define (withdraw amount)
    (cond ((> amount balance)
           'not-enough-balance)
          (else
           (set! balance (- balance amount))
           balance)))
  (define (dispatch m)
    (let ((protect-dw (make-serializer)))
      (cond ((eq? m 'deposite) (protect-dw deposite))
            ((eq? m 'withdraw) (protect-dw withdraw))
            ((eq? m 'balance) balance)
            (else (error "undefined operator -- " m)))))
  dispatch)

;; ;; test
;; (define paul (make-serialized-account 100))
;; (print ((paul 'withdraw) 200))
;; (print ((paul 'deposite) 100))
;; (print (paul 'balance))
;; (exit)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the exchange account
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (exchange-account a1 a2)
  (let ((difference (- (a1 'balance)
                       (a2 'balance))))
    ((a1 'withdraw) difference)
    ((a2 'deposite) difference)))

;; ;;test
;; (define peter (make-serialized-account 100))
;; (define paul (make-serialized-account 200))
;; (exchange-account peter paul)
;; (print (peter 'balance))
;; (print (paul 'balance))
;; (exit)

(define (make-serialized-account-alone balance)
  (define (deposite amount)
    (set! balance (+ balance amount))
    balance)
  (define (withdraw amount)
    (cond ((> amount balance)
           'not-enough-balance)
          (else
           (set! balance (- balance amount))
           balance)))
  (define (dispatch m)
    (let ((protect (make-serializer)))
      (cond ((eq? m 'deposite) deposite)
            ((eq? m 'withdraw) withdraw)
            ((eq? m 'serializer) protect)
            ((eq? m 'balance) balance)
            (else (error "undefined operator -- " m)))))
  dispatch)


(define (deposite account amount)
  (let ((s (account 'serializer))
        (d (account 'deposite)))
    ((s d) amount)))

(define (withdraw account amount)
  (let ((s (account 'serializer))
        (d (account 'withdraw)))
    ((s d) amount)))

;;test
(define peter (make-serialized-account-alone 100))
(define paul (make-serialized-account-alone 200))
;; (print (peter 'balance))
;; (print (withdraw peter 110))
;; (print (withdraw peter 50))
;; (print (deposite peter 100))
;; (print (peter 'balance))
;; (exit)


(define (serialized-exchange-account a1 a2)
  (let ((s1 (a1 'serializer))
        (s2 (a2 'serializer)))
    ((s1 (s2 exchange-account))
     a1
     a2)))

;; ;; test
;; (serialized-exchange-account peter paul)
;; (print (peter 'balance))
;; (print (paul 'balance))
;; (exit)
