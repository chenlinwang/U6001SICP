(define (factor n)
  (if (= n 1)
      1
      (* n (factor (- n 1)))))

;; (print
;;  (((lambda (x)
;;      (lambda (n)
;;        (if (= n 1)
;;            1
;;            (* n ((x x) (- n 1))))))
;;    (lambda (x)
;;      (lambda (n)
;;        (if (= n 1)
;;            1
;;            (* n ((x x) (- n 1))))))) 3))

(define y-combinator (lambda (f)
                       ((lambda (g)
                          (g g))
                        (lambda (x)
                          (f
                           (lambda (v) ((x x) v)))))))

;; (print ((y-combinator (lambda (x)
;;                        (lambda (n)
;;                          (if (= n 1)
;;                              1
;;                              (* n (x (- n 1)))))))
;;         3))

;; (print ((y-combinator (lambda (x)
;;                         (lambda (n)
;;                           (if (= n 1)
;;                               1
;;                               (+ n (x (- n 1)))))))
;;         10))

;; ((lambda (odd even)
;;    (lambda (y)
;;      (if (= y 0) #t ((odd even odd) (- y 1)))))
;;  (lambda (even odd)
;;    (lambda (y)
;;      (if (= y 0) #f ((even odd even) (- y 1)))))
;;  (lambda (odd even)
;;    (lambda (y)
;;      (if (= y 0) #t ((odd even odd) (- y 1))))))

;; (print (((lambda (odd even)
;;            (lambda (y)
;;              (if (= y 0) #t ((odd even odd) (- y 1)))))
;;          (lambda (even odd)
;;            (lambda (y)
;;              (if (= y 0) #f ((even odd even) (- y 1)))))
;;          (lambda (odd even)
;;            (lambda (y)
;;              (if (= y 0) #t ((odd even odd) (- y 1))))))
;;         4)
;;        )

;; (define yy-combinator
;;   (lambda (b a)
;;     ((lambda (g f)
;;        (g f g))
;;      (lambda (odd even)
;;        (b
;;         (lambda (v) ((odd even odd) v))))
;;      (lambda (even odd)
;;        (a
;;         (lambda (v) ((even odd even) v)))))))

;; (print ((yy-combinator
;;          (lambda (m)
;;            (lambda (y)
;;              (if (= y 0) #t (m (- y 1)))))
;;          (lambda (m)
;;            (lambda (y)
;;              (if (= y 0) #f (m (- y 1))))))
;;         5)
;;        )


;; for the recursion with two parameters
(define (arch n m)
  (cond ((and (= n 0)
              (= m 0))
         1)
        ((not (or (= n 0)
                  (= m 0)))
         (+ (arch (- n 1) (- m 1)) 4))
        ((not (= n 0))
         (+ (arch (- n 1) m) 2))
        ((not (= m 0))
         (+ (arch n (- m 1)) 1))))

;;(print (arch 3 5))
(define y-combinator-2 (lambda (h)
                         ((lambda (f)
                            (f f))
                          (lambda (x)
                            (h
                             (lambda (v1 v2) ((x x) v1 v2)))))))
(print ((y-combinator-2
         (lambda (g)
           (lambda (n m)
             (cond ((and (= n 0)
                         (= m 0))
                    1)
                   ((not (or (= n 0)
                             (= m 0)))
                    (+ (g (- n 1) (- m 1)) 4))
                   ((not (= n 0))
                    (+ (g (- n 1) m) 2))
                   ((not (= m 0))
                    (+ (g n (- m 1)) 1)))))
         )
        3 5))

(exit)
