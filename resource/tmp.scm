(define (loadtmp) (load "tmp.scm"))
;; ;; Test for do
;; (let ((x '(1 3 5 7 9)))
;;   (display x)
;;   (newline)
;;   (do ((x x (begin (display "X:")
;;                    (display x)
;;                    (newline)
;;                    (cdr x)))
;;        (sum 0 (begin (display "Sum:")
;;                             (display sum)
;;                             (newline)
;;                             (+ sum (car x)))))
;;       ((null? x) (display "After termination...")
;;        (display "\nFinal Result is ")
;;        (display sum)
;;        (newline)
;;        sum)
;;     (display "Everag Stage:")
;;               (display x)
;;               (display "\t")
;;               (display sum)
;;               (newline)))

;; Test for bla bla
(define (new-cons x y)
  (lambda (msg)
    (define x 100)
    (cond ((eq? msg 'give-car)
           (lambda () x))
          (else
           (error "undefined message -- " msg)))))

;; (define t (new-cons 1 2))
;; (display ((t 'give-car)))
;; (newline)

;; Test for the non-fit-length input
(define (test-mul . x)
  (display x)
  (newline)
  x)

;; (test-mul)
;; (test-mul 1 2 3 4 5)

;; Test for case
(define (case-test msg)
  (case msg
    ((NAME) (display "name"))
    (else (display "else")))
  (newline))

;; (case-test 'NAME)
;; (case-test 'else)

;; (apply case-test '(NAME))

;; ;; Test for the variable passing through the functions
;; ;; to reduce the redundant function to return class variable value
;; (define (class-variable v)
;;   (lambda (self)
;;     (set! v 3)
;;     v))

;; ;; example class to show my doubts
;; (define (make-ob p)
;;   (lambda (method)
;;     (case method
;;       ((p) (class-variable p))
;;       ((dp) (lambda (self) p))
;;       ((set-p) (lambda (self new-p) (set! p new-p)))
;;       (else 'non-method))))

;; (define ob (make-ob 1))
;; (apply (ob 'set-p) (list ob 2))
;; (display (apply (ob 'p) (list ob)))
;; (newline)
;; (display (apply (ob 'dp) (list ob)))
;; (newline)

(exit)
