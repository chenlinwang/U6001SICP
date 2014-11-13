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
(define (stream-cdr stream)
  (cond ((stream-null? stream) stream)
        (else (force-it (cdr stream)))))
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
  (cond ((or (stream-null? stream)
             (< ref 0))
         'done)
        (else (print (stream-car stream))
              (display-stream-ref (stream-cdr stream) (- ref 1)))))

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
(define twos (constant-stream 2))
;; (display-stream-ref twos 5)

(define (stream-plus s1 s2) (stream-map + s1 s2))
(define (stream-minus s1 s2) (stream-map - s1 s2))
(define (stream-multiply s1 s2) (stream-map * s1 s2))
(define (stream-divide s1 s2) (stream-map / s1 s2))
(define (stream-scale stream scalar) (stream-map (lambda (x) (* x scalar)) stream))

(define threes (stream-plus ones twos))
;; (display-stream-ref threes 5)

(define (exponent-stream a)
  (let ((exponent #f))
    (set! exponent (cons 1 (memo-proc (lambda ()
                                         (stream-scale exponent a)))))
    exponent))

;; (define power2 (exponent-stream 2))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; section 3.5.3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; formulating the iterations as stream processes

;; approximation to square root of 2
(define (newton-sqrt-approximation a)
  (lambda (x)
    (/ (+ (square x) a) (* 2 x))))

(define newton-sqrt-2 (newton-sqrt-approximation 2))

(define newton-sqrt-2-stream (self-producted-stream 3 newton-sqrt-2))
;; (display-stream-ref newton-sqrt-2-stream 10)
;; (exit)

;; approximation of pi
(define (pi-sum n)
  (cons (/ 4 n)
        (memo-proc (lambda ()
                     (stream-scale (pi-sum (+ n 2))
                                   -1)))))
;; (display-stream-ref (pi-sum 1) 15)
;; (exit)

(define pi-sum-sequence (pi-sum 1))
(define pi-stream (stream-integral pi-sum-sequence 0))
;; (display-stream-ref pi-stream 15)
;; (exit)

(define (aitken-delte-square stream)
  (let iter ((s1 stream)
             (s2 (stream-cdr stream))
             (s3 (stream-cdr (stream-cdr stream))))
    (cond ((stream-null? s3)
           the-empty-stream)
          (else
           (let ((v1 (stream-car s1))
                 (v2 (stream-car s2))
                 (v3 (stream-car s3)))
             (let ((numerator (square (- v3 v2)) )
                   (denominator (+ v1 (- (* 2 v2)) v3)))
               (if (= denominator 0)
                   the-empty-stream
                   (cons (- v3 (/ numerator denominator))
                         (memo-proc (lambda ()
                                      (iter s2 s3 (stream-cdr s3))))))))))))

;; (define pi-stream-aitken (aitken-delte-square pi-stream))
;; (display-stream-ref pi-stream-aitken 15)
;; (exit)

(define (accelerate-stream stream method)
  (let ((a-stream (method stream)))
    (cond ((stream-null? a-stream)
           the-empty-stream)
          (else
           (cons (stream-car a-stream)
                 (memo-proc (lambda ()
                              (accelerate-stream a-stream method))))))))

;; (display-stream-ref (accelerate-stream pi-stream aitken-delte-square) 15)
;; (exit)

;; the aitken's delta square process would introduce the denominator as 0, thus create an empty stream.  At the equality, s(n+1) - 2s(n) + s(n-1) = 0, reaches the computers' precision. But we could acturally build an infinity precision system using the stream.

;;--------------------
;; to generate all the pairs with i <= j, belonging to all natural numbers

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons (stream-car s1)
            (memo-proc (lambda ()
                         (interleave s2 (stream-cdr s1)))))))

;; devide the triangle into three parts, the first element of the first row, the rest of the first row and the rest triangle
(define (ordered-pairs s t)
  (cons (cons (stream-car s)
              (stream-car t))
        (memo-proc (lambda ()
                     (interleave (stream-map (lambda (n) (cons (stream-car s)
                                                               n))
                                             (stream-cdr t))
                                 (ordered-pairs (stream-cdr s)
                                                (stream-cdr t)))))))

(define ints (intsinitwith 1))
(define ordered-ints (ordered-pairs ints ints))

;; (display-stream-ref ordered-ints 20)
;;(exit)

(define prime-pairs  (stream-filter (lambda (s) (prime? (+ (car s)
                                                           (cdr s))))
                                    ordered-ints))

;; (display-stream-ref prime-pairs 20)
;; (exit)

;;--------------------

(define (stream-integral-dt integrand init dt)
  (let ((result #f))
    (set! result (cons init
                       (memo-proc (lambda ()
                                    (stream-plus (stream-scale integrand dt)
                                                 result)))))
    result))

;; (define one-integral-2 (stream-integral-dt ones 0 2))
;; (display-stream-ref one-integral-2 10)
;; (exit)
