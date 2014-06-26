;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; section 2.3.1 quotations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (memq item itemlist)
    ;; To see whether a item is in itemlist. item: item; itemlist: list.
    ;; (EQtype, list) -> (boolean)
    (cond ((null? itemlist) #f)
          ((eq? item (car itemlist)) #t)
          (else (memq item (cdr itemlist)))))

;; (display (memq 'a '(b c d e f)))
;; (display (memq 'a '(a b c d)))
;; (display (memq 'a '(a)))
;; (newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sec1.1.scm 2.3.2 deriverative
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constructor
(define (make-sum e1 e2)
  ;; make a sum out of e1 and e2.e1:addend; e2:augend.
  ;; (A,A) -> (list/number)
  (cond ((and (number? e1) (number? e2)) (+ e1 e2))
        ((and (number? e1) (= e1 0)) e2)
        ((and (number? e2) (= e2 0)) e1)
        (else (list '+ e1 e2) )))

(define (make-product e1 e2)
  ;; make a product out of e1 and e2.e1:multiplier;e2:multiplicand.
  ;; (A,A) -> (list/number)
  (cond ((and (number? e1) (number? e2)) (* e1 e2))
        ((and (number? e1) (= e1 1)) e2)
        ((and (number? e1) (= e1 0)) 0)
        ((and (number? e2) (= e2 1)) e1)
        ((and (number? e2) (= e2 0)) 0)
        (else (list '* e1 e2) )))

;; Selectors
(define addend cadr)
(define augend caddr)
(define multiplier cadr)
(define multiplicant caddr)
;; The Operators
(define (expr? expr) (and (list? expr) (= (length expr) 3)))
(define (sum? expr) (and (expr? expr) (eq? '+ (car expr))))
(define (product? expr) (and (expr? expr) (eq? '* (car expr))))
(define (same-symbol? e1 e2) (eq? e1 e2))

(define (differentiate expr var)
  ;; To differentiate an expr within var. expr:expression to be differentiate; var:respect variable.
  ;; (list,symbol) -> (list)
  (cond ((number? expr) 0)
        ((symbol? expr) (if (same-symbol? expr var) 1 0))
        ((sum? expr) (make-sum (differentiate (addend expr) var)
                               (differentiate (augend expr) var)))
        ((product? expr) (make-sum (make-product (differentiate (multiplier expr) var)
                                                 (multiplicant expr))
                                   (make-product (multiplier expr)
                                                 (differentiate (multiplicant expr) var))))
        (else (begin (display "Error: unrecongnized symbol:\n")
                     (display "\t")
                     (display expr)
                     (newline)))))
