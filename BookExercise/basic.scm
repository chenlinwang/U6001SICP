;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic operations that scheme ought to provide but fail
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Determind
(define (even? x) (= (remainder x 2) 0))
(define (odd? x) (= (remainder x 2) 1))

;; Mathmatics
(define (absolute x) (if (> x 0) x (- x)))
(define (square x) (* x x))
(define (cube x) (* x x x))
(define (exponent-iter base exp product)
    (cond ((= exp 0) product)
          ((= exp 1) (* base product))
          ((even? exp) (exponent-iter (square base) (/ exp 2) product))
          (else (exponent-iter base (- exp 1) (* product base)))))

(define (exponentiation base exp)
    (if (< exp 0)
        (exponent-iter (/ 1 base) (- exp) 1)
        (exponent-iter base exp 1)))

(define (average x . y)
  ;; Get the average number of the input.
  ;; (number,list) -> (number)
    (define (additer rest total)
      ;; iteratively add rest to total. rest: list holding all addents; total: the augend.
        ;; (list,numer) -> (number)
        (if (null? rest)
            total
            (additer (cdr rest) (+ total (car rest)))))
  (/ (additer (cons x y) 0) (+ 1 (length y))))

(define (gcd n1 n2)
  ;; Calculate the greatest common dividor
  (let iter ((first n1)
             (second n2))
    (let ((remain (remainder first second)))
      (if (= remain 0)
          second
          (iter second remain)))))

;; generate the range from 0 to n-1
(define (range n)
  (let iter ((rest 0))
    (cond ((>= rest n) (list))
          (else (cons rest (iter (+ 1 rest)))))))

(define (get-group n r)
  ;; Convert n to a r-group member
  ;; (number,number) -> (number)
  (cond ((< n 0) (get-group (+ n r) r))
        ((>= n r) (get-group (- n r) r))
        (else n)))

;; Return part of the list
(define (list-get l n m)
    ;; get the list member from n to m-1. l: list; n: starting number, m: ending number.
    ;; (list,number,number) -> (list)
    (define (gi l now start end)
        ;; recursively go through the list. l: the list; now: the place of the first element; start: start number; end: end number;
        ;; (list,number,number,number) -> (list)
        (cond ((< now start) (gi (cdr l) (+ now 1) start end))
              ((< now end) (cons (car l) (gi (cdr l) (+ now 1) start end)))
              (else (list))))
    (let ((listlen (+ (length l) 1)))
      (let ((start (get-group n listlen))
            (end (get-group m listlen)))
        (if (> start end)
            (errormsg "list-get:start passes end." (list (list n start) (list m end)))
            (gi l 0 start end)))))

;; Display out
(define (print-out line) (display line) (newline))

;; Error message handler
(define (errormsg msg info)
    ;; Display message and information.
    (display "Error:\t")
    (print-out msg)
    (display "\t")
    (print-out info)
    #f)
;; Warning message handler
(define (warningmsg msg info)
  ;; Display message and information.
  (display "Warning:\t")
  (print-out msg)
  (display "\t\t")
  (print-out info)
  #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sorting Methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Merge Sort
(define (merge-sort l)
    ;; Sorting list using merge sort.l:list.
    ;; (list) -> (list)
    (let ((llen (length l)))
      ;; See how long is the list
      (if (= llen 1)
          ;; return if length is one
          l
          ;; Otherwise, break it into two list
          (let iter ((former (merge-sort
                              (list-get l 0 (floor (/ llen 2)))))
                     (latter (merge-sort
                              (list-get l (floor (/ llen 2)) -1))))
               ;; If any one is empty, stop.
               (cond ((null? former) latter)
                     ((null? latter) former)
                     ;; Otherwise, continue
                     (else
                      (let ((f1 (car former))
                            (l1 (car latter)))
                        (if (<= f1 l1)
                            (cons f1 (iter (cdr former) latter))
                            (cons l1 (iter former (cdr latter)))))))))))

;; ;; Testing
;; (print-out (merge-sort (list 22 42 4 1 4 5 3)))

;; Extended Merger Sort
(define (ext-merge-sort l . cal)
  ;; Sorting list using merge sort.l:list.
  ;; (list) -> (list)
  (let ((cal (cond ((null? cal) (lambda (x) x))
                   ((procedure? (car cal)) (car cal))
                   (else (lambda (x) x)))))
    (let iter1 ((l l))
      (let ((llen (length l)))
        ;; See how long is the list
        (if (= llen 1)
            ;; return if length is one
            l
            ;; Otherwise, break it into two list
            (let iter2 ((former (iter1 (list-get l 0 (floor (/ llen 2)))))
                        (latter (iter1 (list-get l (floor (/ llen 2)) -1))))
              ;; If any one is empty, stop.
              (cond ((null? former) latter)
                    ((null? latter) former)
                    ;; Otherwise, continue
                    (else
                     (let ((f1 (car former))
                           (l1 (car latter)))
                       (if (<= (cal f1)
                               (cal l1))
                           (cons f1 (iter2 (cdr former) latter))
                           (cons l1 (iter2 former (cdr latter)))))))))))))

;; ;; test
;; (define t '((10) (2) (3) (4) (5) (8)))
;; (print-out (ext-merge-sort t car))
;; (exit)

;; Quick Sort
(define (quick-sort l)
  ;; Quick sort of list l.
  ;; (list) -> (list)

  ;; Calculating the maximum index number of list l
  (define (listMaxRef l) (- (length l) 1))

  ;; Chosing a pivot for the algorithm
  (define (pivot l LMR)
    (average (list-ref l 0)
               (list-ref l LMR)
               (list-ref l (inexact->exact (floor (/ LMR 2))))))

  ;; Initially try to check the list
  (let iter1 ((rest l)
              (LMR (listMaxRef l)))
    ;; If there is 1 or less element, return the list.
    ;; (print-out rest)
    ;; (print-out LMR)
    (cond ((<= LMR 0) rest)
          ;; If there are two elements, compare and return the right ordered list.
          ((= LMR 1)
           (if (< (car rest) (cadr rest))
               rest
               (list (cadr rest) (car rest))))
          (else
           ;; Else calculate the pivot
           (let ((p (pivot rest LMR)))
             ;; (print-out p)
             ;; Process with the pivot using iters
             (let iter2 ((rest rest)
                         (smaller (list))
                         (bigger (list)))
               ;; If finishing compare, recursively use iter1
               (cond ((null? rest) (append (iter1 smaller (listMaxRef smaller))
                                           (iter1 bigger (listMaxRef bigger))))
                     ;; If it is smaller or equal, put it into smaller
                     ((<= (car rest) p)
                      (set! smaller (cons (car rest) smaller))
                      (iter2 (cdr rest) smaller bigger))
                     ;; If it is bigger or equal, put it into bigger
                     (else
                      (set! bigger (cons (car rest) bigger))
                      (iter2 (cdr rest) smaller bigger))))))))
  )

;; Test
;; (print-out (quick-sort (list 22 42 4 1 4 5 3)))
;; (exit)

(define (ext-quick-sort l . cal)
  ;; Quick sort of list l.
  ;; (list) -> (list)

  ;; set the calculation function
  (set! cal (cond ((null? cal) (lambda (x) x))
                  ((procedure? (car cal)) (car cal))
                  (else (lambda (x) x))))

  ;; Calculating the maximum index number of list l
  (define (listMaxRef l) (- (length l) 1))

  ;; Chosing a pivot for the algorithm
  (define (pivot l LMR)
    (/ (apply +
              (map cal (list (list-ref l 0)
                             (list-ref l LMR)
                             (list-ref l (inexact->exact (floor (/ LMR 2)))))))
       3))

  ;; Initially try to check the list
  (let iter1 ((rest l)
              (LMR (listMaxRef l)))
    ;; If there is 1 or less element, return the list.
    ;; (print-out rest)
    ;; (print-out LMR)
    (cond ((<= LMR 0) rest)
          ;; If there are two elements, compare and return the right ordered list.
          ((= LMR 1)
           (if (< (cal (car rest))
                  (cal (cadr rest)))
               rest
               (list (cadr rest) (car rest))))
          (else
           ;; Else calculate the pivot
           (let ((p (pivot rest LMR)))
             ;; (print-out p)
             ;; Process with the pivot using iters
             (let iter2 ((rest rest)
                         (smaller (list))
                         (bigger (list)))
               ;; If finishing compare, recursively use iter1
               (cond ((null? rest) (append (iter1 smaller (listMaxRef smaller))
                                           (iter1 bigger (listMaxRef bigger))))
                     ;; If it is smaller or equal, put it into smaller
                     ((<= (cal (car rest)) p)
                      (set! smaller (cons (car rest) smaller))
                      (iter2 (cdr rest) smaller bigger))
                     ;; If it is bigger or equal, put it into bigger
                     (else
                      (set! bigger (cons (car rest) bigger))
                      (iter2 (cdr rest) smaller bigger)))))))))
;; ;; Test
;; (define t '(19 2 3 4 4 5 2 6))
;; (print-out (ext-quick-sort t))
;; (define t '((19) (2) (4) (2) (5) (19) (6)))
;; (print-out (ext-quick-sort t car))
;; (exit)

;; ;; Test
;; (print-out (gcd 24 30))
;; (print-out (gcd 5 7))
;; (print-out (gcd 10 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auxiliary functions for oop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for getting the method of the object
(define (get-method object method-name)
  (if (procedure? object)
      (object method-name)
      (error "Not a procedure-based object -- " object)))

;; for applying the method for the object
(define (ask object method-name . args)
  (let ((method (get-method object method-name)))
    (if (procedure? method)
        (apply method (cons object args))
        (error "Can't find the method in object -- " method-name))))

(define (class-variable v)
  (lambda (self) v))

;; (exit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; logical operators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (all l)
  (cond ((null? l) #t)
        ((not (car l)) #f)
        (else (all (cdr l)))))

(define (any l)
  (cond ((null? l) #f)
        ((car l) #t)
        (else (any (cdr l)))))

;; ;; test
;; (print (all (list #t #t #t #f)))
;; (print (any (list #f #f #f #t)))
;; (exit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; accumulate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; accumulate the list with certain connection after some computation
(define (accumulate connect compute init . l)
  (let iter ((rest l))
    (cond ((any (map null? rest)) init)
          (else (connect (apply compute (map car rest))
                         (iter (map cdr rest)))))))
;; ;; test
;; (print (accumulate cons + (list) (range 5) (range 10)))
;; (print (accumulate + square 0 (range 5)))
;; (exit)

(define (recursion connect compute first rest terminate init . l)
  (let iter ((remain l))
    (cond ((any (map terminate remain)) init)
          (else (connect (apply compute (map first remain))
                         (iter (map rest remain)))))))

;; test
;; (print (recursion + square car cdr null? 0 (range 5)))
;; (exit)

;; filter
(define (filter pre l)
  (let iter ((rest l))
    (cond ((null? rest) (list))
          ((pre (car rest)) (cons (car rest)
                                  (iter (cdr rest))))
          (else (iter (cdr rest))))))

;; ;; test
;; (print (filter even? (range 10)))
;; (exit)

;; found map:
(define (foundmap pre connect compute init l)
  (let iter ((rest l))
    (cond ((null? rest) init)
          ((pre (car rest)) (connect (compute (car rest)) (cdr rest)))
          (else (connect (car rest) (iter (cdr rest)))))))

;; ;; test
;; (print (foundmap even? cons square (list) (list 1 3 5 8 2 4)))
;; (exit)

;; remove map:
(define (removemap pre connect init l)
  (let iter ((rest l))
    (cond ((null? rest) init)
          ((pre (car rest)) (cdr rest))
          (else (connect (car rest) (iter (cdr rest)))))))

;; ;; test
;; (print (removemap even? cons (list) (list 1 3 5 8 2 4)))
;; (exit)
