(load "../BookExercise/basic")
(define (loadimp35) (load "../BookImplementation/sec3.5"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3.5.1 stream
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; implementing directly the delay and force it
;; delay without memorize result, can't define it as this because scheme is cbv
;; (define (delay exp) (lambda () exp))

;; force it
(define (force-it delay-object) (delay-object))

;; memorize procedure result
(define (memo-proc proc)
  (let ((run-already #f)
        (result #f))
    (lambda ()
      (cond (run-already result)
            (else
             (set! run-already #t)
             (set! result (proc))
             result)))))

;; optimize the delay, similar reasons
;; (define (delay exp) (memo-proc (lambda () exp)))

;;------------------------------
;; the stream object
(define stream-car car)
(define (stream-cdr stream) (force-it (cdr stream)))
(define the-empty-stream (list))
(define stream-null? null?)

;; stream-cons, similar reasons
;; (define (stream-cons a b) (cons a (delay b)))

;; stream operators
(define (stream-ref stream n)
  (cond ((= n 0) (stream-car stream))
        (else (stream-ref (stream-cdr stream) (- n 1)))))

(define (stream-map proc stream)
  (cond ((stream-null? stream) the-empty-stream)
        (else (cons (proc (stream-car stream))
                    (memo-proc (lambda ()
                                 (stream-map proc
                                             (stream-cdr stream))))))))

;; multiple stream-map
(define (stream-map proc . streams)
  (cond ((any (map stream-null? streams))
         the-empty-stream)
        (else
         (cons
          (apply proc (map (lambda (s) (stream-car s))
                           streams))
          (memo-proc (lambda ()
                       (apply stream-map
                              (cons proc
                                    (map (lambda (s) (stream-cdr s))
                                         streams)))))))))

(define (stream-for-each proc stream)
  (cond ((stream-null? stream) 'done)
        (else (proc (stream-car stream))
              (stream-for-each proc (stream-cdr stream)))))

(define (display-stream stream)
  (stream-for-each print stream))

(define (display-stream-ref stream ref)
  (display-stream (stream-map (lambda (a b) a)
                              stream
                              (stream-enumerate-interval 1 ref))))

(define (stream-filter pre stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pre (stream-car stream))
         (cons (stream-car stream)
               (memo-proc (lambda () (stream-filter pre
                                                    (stream-cdr stream))))))
        (else
         (stream-filter pre (stream-cdr stream)))))

;; stream-add
(define (stream-add s1 s2)
  ;; (write "s1:")
  ;; (print s1)
  ;; (print (stream-cdr s1))
  ;; (write "s2:")
  ;; (print s2)
  ;; (print (stream-cdr s2))
  (cond ((or (stream-null? s1)
             (stream-null? s2))
         the-empty-stream)
        (else
         ;; (print "not null!")
         (cons (+ (stream-car s1)
                  (stream-car s2))
               (memo-proc (lambda () (stream-add (stream-cdr s1)
                                                 (stream-cdr s2))))))))

;; test
(define one (cons 1 (memo-proc (lambda () one))))
(define (stream-enumerate-interval low high)
  (cond ((> low high) the-empty-stream)
        (else
         (cons low (memo-proc (lambda () (stream-enumerate-interval (+ 1 low) high)))))))
;; (define int1to5 (stream-enumerate-interval 1 5))
;; (define ints (cons 1 (memo-proc (lambda ()
;;                                   (stream-add one ints)))))
;; (print (map (lambda (n) (stream-ref ints n))
;;             (list 0 1 2 3 4)))
(define (intsinitwith a)
  (let ((intsinitwitha #f))
    (set! intsinitwitha (cons a
                             (memo-proc
                              (lambda ()
                                (stream-add one intsinitwitha)))))
    intsinitwitha))

;; (define intsinitwith3 (intsinitwith 3))
;; (print (map (lambda (n) (stream-ref intsinitwith3 n))
;;             (list 0 1 2 3)))
;; (exit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; section 3.5.2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;------------------------------
;; explicitly using streams
(define (intsinitwith a)
  (cons a (memo-proc (lambda ()
                       (intsinitwith (+ a 1))))))

;; test
;; (define intsinitwith1 (intsinitwith 1))

(define (divide? a b) (= (remainder a b) 0))

;; (define intsdividewith7 (stream-filter (lambda (a) (divide? a 7))
;;                                        intsinitwith1))

;; fibonacci
(define fib (cons 0
                  (memo-proc (lambda ()
                               (cons 1
                                     (memo-proc (lambda ()
                                                  (stream-map + fib (stream-cdr fib)))))))))

;; the seive for the prime number
(define (seive stream)
  (cons (stream-car stream)
        (memo-proc (lambda ()
                     (seive
                      (stream-filter (lambda (a) (not (divide? a (stream-car stream))))
                                     (stream-cdr stream)))))))

(define prime (seive (intsinitwith 2)))
;; (display-stream-ref prime 20)


;;------------------------------
;; implicit definition
(define (constant-stream a)
  (let ((constant #f))
    (set! constant (cons a (memo-proc (lambda () constant))))
    constant))

(define ones (constant-stream 1))
;; (define twos (constant-stream 2))
;; (display-stream-ref twos 5)

(define (stream-plus s1 s2) (stream-map + s1 s2))
(define (stream-minus s1 s2) (stream-map - s1 s2))
(define (stream-multiply s1 s2) (stream-map * s1 s2))
(define (stream-divide s1 s2) (stream-map / s1 s2))
(define (stream-scale stream scalar) (stream-map (lambda (x) (* x scalar)) stream))

;; (define threes (stream-plus ones twos))
;; (display-stream-ref threes 5)

(define (exponient-stream a)
  (let ((exponient #f))
    (set! exponient (cons 1 (memo-proc (lambda ()
                                         (stream-scale exponient a)))))
    exponient))

;; (define power2 (exponient-stream 2))
;; (display-stream-ref power2 10)
;; (exit)

(define (self-producted-stream init procede)
  (let ((stream #f))
    (set! stream (cons init (memo-proc (lambda ()
                                         (self-producted-stream (procede (stream-car stream)) procede)))))
    stream))

;; ;;test
;; (define power3 (self-producted-stream 1 (lambda (x) (* x 3))))
;; (display-stream-ref power3 10)
;; (exit)

(define (stream-accumulate stream dt)
  (apply stream-map (cons +
                          (cons stream
                                (accumulate cons
                                            (lambda (n)
                                              (let ((next-stream (stream-cdr stream)))
                                                (set! stream next-stream)
                                                stream))
                                            (list)
                                            (range (- dt 1)))))))

;; (define fours (stream-accumulate twos 2))
;; ;; (display-stream-ref fours 5)
;; (define fives (stream-scale ones 5))
;; ;; (display-stream-ref fives 5)
(define (stream-integral stream init)
  (let ((integral #f))
    (set! integral (cons (+ init
                            (stream-car stream))
                         (memo-proc (lambda ()
                                      (stream-plus integral
                                                   (stream-cdr stream))))))
    integral))
;; (define ints (stream-integral ones 0))
;; ;; (display-stream-ref ints 5)
;; ;; (exit)

;; the prime again
(define prime (cons 2 (memo-proc (lambda ()
                                   (stream-filter prime? (intsinitwith 3))))))
(define (prime? n)
  (let iter ((rest-stream prime))
    (cond ((> (square (stream-car rest-stream)) n) #t)
          ((divide? n (stream-car rest-stream)) #f)
          (else (iter (stream-cdr rest-stream))))))
;; (display-stream-ref prime 1000)
;; (exit)
