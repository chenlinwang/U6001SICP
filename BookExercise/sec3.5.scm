(load "../BookExercise/basic")
(define (loadexe35) (load "sec3.5"))
(load "../BookImplementation/sec3.5")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3.50
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Complete the following definition, which generalizes stream-map to allow procedures that take multiple arguments, analogous to map in section 2.2.3, footnote 12.

(define (stream-map proc . argstreams)
  ;; (write "stream-map-argstreams:")
  ;; (print argstreams)
  (if (any (map stream-null? argstreams))
      the-empty-stream
      (cons
       (apply proc (map (lambda (s) (stream-car s)) argstreams))
       (memo-proc (lambda ()
                    (apply stream-map
                           (cons proc (map (lambda (s) (stream-cdr s))
                                           argstreams))))))))

;; ;; test
;; (define int1to3 (stream-enumerate-interval 1 3))
;; ;;(define t (stream-map (lambda (n) n) int1to3))
;; ;; (print t)
;; ;; (print (stream-cdr t))

;; ;; single
;; (display-stream (stream-map (lambda (n) n) int1to3))
;; ;; double
;; (display-stream (stream-map + int1to3 int1to3))
;; ;; triple
;; (display-stream (stream-map + int1to3 int1to3 int1to3))
;; (exit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3.51
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; In order to take a closer look at delayed evaluation, we will use the following procedure, which simply returns its argument after printing it:
;; (define (show x)
;;   (print x)
;;   x)
;; ;; What does the interpreter print in response to evaluating each expression in the following sequence?
;; (define x (stream-map show (stream-enumerate-interval 0 10)))
;; (stream-ref x 5)
;; (stream-ref x 7)
;; (exit)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3.52
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (define sum 0)
;; (print sum)
;; (define (accum x)
;;   (set! sum (+ x sum))
;;   sum)
;; (print accum)
;; (define seq (stream-map accum (stream-enumerate-interval 1 20)))
;; (print seq)
;; (define y (stream-filter even? seq))
;; (print y)
;; (define z (stream-filter (lambda (x) (= (remainder x 5) 0))
;; seq))
;; (print z)
;; (print (stream-ref y 7))
;; (print (display-stream z))
;; (exit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3.54
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define a procedure mul-streams, analogous to add-streams, that produces the elementwise product of its two input streams. Use this together with the stream of integers to complete the following definition of the stream whose nth element (counting from 0) is n + 1 factorial:

(define factorials (cons 1 (memo-proc (lambda ()
                                        (stream-multiply (intsinitwith 2)
                                                         factorials)))))

;; ;; test
;; (display-stream-ref factorials 6)
;; (exit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3.55
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Define a procedure partial-sums that takes as argument a stream S and returns the stream whose elements are S0, S0 + S1, S0 + S1 + S2, .... For example, (partial-sums integers) should be the stream 1, 3, 6, 10, 15, ....

(define partial-sums (cons 0 (memo-proc (lambda ()
                                          (stream-plus (intsinitwith 1)
                                                       partial-sums)))))

;; ;; test
;; (display-stream-ref partial-sums 5)
;; (exit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3.56
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A famous problem, first raised by R. Hamming, is to enumerate, in ascending order with no repetitions, all positive integers with no prime factors other than 2, 3, or 5. One obvious way to do this is to simply test each integer in turn to see whether it has any factors other than 2, 3, and 5. But this is very inefficient, since, as the integers get larger, fewer and fewer of them fit the requirement. As an alternative, let us call the required stream of numbers S and notice the following facts about it.
;; 1. S begins with 1.
;; 2. The elements of (scale-stream S 2) are also elements of S.
;; 3. The same is true for (scale-stream S 3) and (scale-stream 5 S).
;; 4. These are all the elements of S.
;; Now all we have to do is combine elements from these sources. For this we define a procedure merge that combines two ordered streams into one ordered result stream, eliminating repetitions:
;; #+BEGIN_SRC scheme
;; (define (merge s1 s2)
;;   (cond ((stream-null? s1) s2)
;;         ((stream-null? s2) s1)
;;         (else
;;          (let ((s1car (stream-car s1))
;;                (s2car (stream-car s2)))
;;            (cond ((< s1car s2car)
;;                   (cons s1car (memo-proc (lambda ()
;;                                            (merge (stream-cdr s1) s2)))))
;;                  ((> s1car s2car)
;;                   (cons s2car (memo-proc (lambda ()
;;                                            (merge s1 (stream-cdr s2))))))
;;                  (else
;;                   (cons s1car
;;                         (memo-proc (lambda ()
;;                                      (merge (stream-cdr s1)
;;                                             (stream-cdr s2)))))))))))
;; #+END_SRC
;; Then the required stream may be constructed with merge, as follows:
;; #+BEGIN_SRC scheme
;; (define S (cons 1 (memo-proc (lambda ()
;;                                (merge (stream-scale S 2)
;;                                       (merge (stream-scale S 3)
;;                                              (stream-scale S 5)))))))
;; test
;; (display-stream-ref S 15)
;; (exit)
;; #+END_SRC
;; Fill in the missing expressions in the places marked <??> above.


;; The text gives only merge between two streams, let's expand it to multiple first with repetition.

;; stream merger according to special function
(define (stream-merge order same? . streams)
  ;; (write "all streams:")
  ;; (print streams)
  (cond ((null? streams) the-empty-stream)
        ((let* ((non-null-streams (filter (lambda (s) (not (stream-null? s)))
                                          streams))
                ;; (tmp (begin (write "non-null-streams:")
                ;;             (print non-null-streams)))
                (chosen-element (apply order (map stream-car non-null-streams))))
           ;; (write "chosen element:")
           ;; (print chosen-element)
           ;; (print "ok")
           (cons chosen-element (memo-proc (lambda ()
                                             (let ((rest-streams (foundmap (lambda (s) (same? chosen-element (stream-car s)))
                                                                           cons stream-cdr the-empty-stream non-null-streams)))
                                               (apply stream-merge (cons order (cons same? rest-streams)))))))))))

;; ;; test
;; (define intsinitwith0 (intsinitwith 0))
;; (define intsinitwith1 (intsinitwith 1))
;; (define int0to3 (stream-enumerate-interval 0 3))
;; (define m (stream-merge min = intsinitwith0 intsinitwith1 int0to3))
;; (display-stream-ref m 15)
;; (exit)

(define (stream-remove-redundant stream same?)
  (cons (stream-car stream)
        (memo-proc (lambda ()
                     (stream-remove-redundant (stream-filter (lambda (n) (not (same? n (stream-car stream))))
                                                             (stream-cdr stream))
                                              same?)))))

;; (define s (stream-remove-redundant (cons 1
;;                                          (memo-proc (lambda ()
;;                                                       (cons 2 (memo-proc (lambda ()
;;                                                                            (stream-merge min
;;                                                                                          =
;;                                                                                          (stream-scale s 2)
;;                                                                                          (stream-scale s 3)
;;                                                                                          (stream-scale s 5))))))))
;;                                    =))

(define s (stream-remove-redundant (cons 1 (memo-proc (lambda ()
                                                        (stream-merge min
                                                                      =
                                                                      (stream-scale s 2)
                                                                      (stream-scale s 3)
                                                                      (stream-scale s 5)))))
                                   =))
;; (print 'start)
;; (print (stream-cdr s))
;; (print 'end)
;; (display-stream-ref s 15)
;; (exit)
