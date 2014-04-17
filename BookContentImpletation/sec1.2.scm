;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Factorial
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Doing it downwards
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

;; Doing it upwards
(define (factorial-up n)
  ;; A recursion to mulitply until reach n
  (define (up result next)
    (if (= next n)
        (* result n)
        (up (* result next) (+ next 1))))
  (up 1 1))
