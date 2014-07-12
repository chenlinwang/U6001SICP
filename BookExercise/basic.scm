;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic operations that scheme ought to provide but fail
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Determind
(define (even? x) (= (remainder x 2) 0))
(define (odd? x) (= (remainder x 2) 1))

;; Mathmatics
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

;; Error message handler
(define (errormsg msg info)
    ;; Display message and information.
    (display "Error:\t")
    (display msg)
  (newline)
  (display "\t")
  (display info)
  (newline))

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
;; (display (merge-sort (list 22 42 4 1 4 5 3)))
;; (newline)
