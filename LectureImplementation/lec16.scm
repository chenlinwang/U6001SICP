(load "../BookExercise/basic")
(define (loadlec16) (load "../LectureImplementation/lec16"))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; People Objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; person object
(define (make-person fname lname)
  (lambda (method)
    (case method
      ((class-name) (lambda (self) 'person))
      ((first-name) (lambda (self) fname))
      ((last-name) (lambda (self) lname))
      ((preferred-name) (lambda (self) (display fname)))
      ((whoru?) (lambda (self) (print-out fname)))
      ((repeat) (lambda (self list-of-stuff)
               (let iter ((things list-of-stuff))
                 (cond ((null? things))
                       (else
                        (display " ")
                        (display (car things))
                        (iter (cdr things)))))))
      ((sign-name) (lambda (self)
                     (display " -- said ")
                     (ask self 'whoru?)))
      ((say) (lambda (self list-of-stuff)
               (ask self 'repeat list-of-stuff)
               (display ".\n")
               (ask self 'sign-name)))
      ((question) (lambda (self person list-of-stuff)
                    (ask person 'preferred-name)
                    (display ",")
                    (ask self 'repeat list-of-stuff)
                    (display "?\n")
                    (ask self 'sign-name)
                    (ask person 'answer self list-of-stuff)))
      (else 'non-method))))


;; ;; Test
;; (define wcl (make-person 'Chenlin 'Wang))
;; (print-out (ask wcl 'class-name))
;; (print-out (ask wcl 'first-name))
;; (print-out (ask wcl 'last-name))
;; (ask wcl 'whoru?)
;; (ask wcl 'say '(the Weather is good))

;; professor class
(define (make-professor fname lname)
  (let ((supper-class (make-person fname lname)))
    (lambda (method)
      (case method
        ((class-name) (lambda (self) 'professor))
        ((preferred-name) (lambda (self)
                            (display "Professor ")
                            (display lname)))
        ((whoru?) (lambda (self)
                    (display "Professor ")
                    (print-out lname)))
        ((lecture) (lambda (self list-of-stuff)
                     (display "Therefore,")
                     (ask self 'say list-of-stuff)))
        ((answer) (lambda (self person list-of-stuff)
                    (ask person 'preferred-name)
                    (display ", I am not sure. Let me look it up and get back to you later.\n")
                    (ask self 'sign-name)))
        (else (get-method supper-class method))))))

;; ;; Test
;; (define pwcl (make-professor 'Chenlin 'Wang))
;; (print-out (ask pwcl 'class-name))
;; (print-out (ask pwcl 'first-name))
;; (print-out (ask pwcl 'last-name))
;; (ask pwcl 'whoru?)
;; (ask pwcl 'say '(the Weather is good))
;; (ask pwcl 'lecture '(the earth is round))

;; the arrogent professer class
(define (make-arrogent-professor fname lname)
  (let ((supper-class (make-professor fname lname)))
    (lambda (method)
      (case method
        ((class-name) (lambda (self) 'arrogent-professor))
        ((repeat) (lambda (self list-of-stuff)
                    (ask supper-class 'repeat list-of-stuff)
                    (display ", obviously")))
        ((answer) (lambda (self person list-of-stuff)
                    (ask person 'preferred-name)
                    (display ",")
                    (case (ask person 'class-name)
                      ((professor) (ask supper-class 'say
                                        '(I thought you had a paper on that matter)))
                      (else (ask supper-class 'say
                                 '(this should be obvious for you))))))
        (else (get-method supper-class method))))))

;; ;; Test
;; (define apwcl (make-arrogent-professor 'Chenlin 'Wang))
;; (print-out (ask apwcl 'class-name))
;; (print-out (ask apwcl 'first-name))
;; (print-out (ask apwcl 'last-name))
;; (ask apwcl 'whoru?)
;; (ask apwcl 'say '(the Weather is good))
;; (ask apwcl 'lecture '(the earth is round))

;; student class
(define (make-student fname lname)
  (lambda (method)
    (let ((supper-class (make-person fname lname)))
      (case method
      ((class-name) (lambda (self) 'student))
      ((repeat) (lambda (self list-of-stuff)
                  (display "Eh,")
                  (ask supper-class 'repeat list-of-stuff)))
      ((question) (lambda (self professor list-of-stuff)
                    (display "Excuse me, ")
                    (ask professor 'preferred-name)
                    (display ".")
                    (ask self 'repeat list-of-stuff)
                    (display "?\n")
                    (ask self 'sign-name)
                    (ask profess self list-of-stuff)))
      (else (get-method supper-class method))))))

;; ;; Test
;; (define swcl (make-student 'Chenlin 'Wang))
;; (print-out (ask swcl 'class-name))
;; (print-out (ask swcl 'first-name))
;; (print-out (ask swcl 'last-name))
;; (ask swcl 'whoru?)
;; (ask swcl 'say '(the Weather is good))


;; ;; Test on question and answer
;; (define p (make-person 'Peter 'Parker))
;; (define pr (make-professor 'Stephen 'Hawking))
;; (define apr (make-arrogent-professor 'Sheldon 'Cooper))
;; (define s (make-person 'Harry 'Potter))

;; (ask p 'question pr '(how is the universe came about))
;; (ask p 'question apr '(how is the universe came about))
;; (ask s 'question pr '(how is the universe came about))
;; (ask s 'question apr '(how is the universe came about))
;; (ask pr 'question apr '(how is the universe came about))
;; (exit)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The battle ship game
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helper function with the velocity and the acceleration

;; module
(define (vector-module v)
  (sqrt (+ (square (vector-ref v 0))
           (square (vector-ref v 1)))))

(define (vector-add v1 v2)
  (vector (+ (vector-ref v1 0)
             (vector-ref v2 0))
          (+ (vector-ref v1 1)
             (vector-ref v2 1))))

(define (vector-scalar v s)
  (vector (* s (vector-ref v 0))
          (* s (vector-ref v 1))))

(define (class-variable v)
  (lambda (self) v))

;; Abstract Class: game obeject that the object
(define (make-game-object position velocity linear-acceleration tangent-acceleration maximum-speed maximum-acceleration)
  (if (or (< maximum-speed 0)
          (< maximum-acceleration 0))
      (error "Speed and Acceleration should be non-positive!"))
  (lambda (method)
    (case method
      ((current-position) (class-variable position))
      ((current-velocity) (class-variable velocity))
      ((linear-acceleration) (class-variable linear-acceleration))
      ((tangent-acceleration) (class-variable tangent-acceleration))
      ((current-acceleration) (class-variable (vector-add linear-acceleration
                                                          tangent-acceleration)))
      ((process-velocity) (lambda (self)
                            (let ((new-velocity (vector-add velocity linear-acceleration)))
                              (let ((new-velocity-modular (vector-module velocity)))
                                (cond ((> new-velocity-modular maximum-speed)
;; stop here right on how to solve the speed changing problem!!!!
                                       (set! new-velocity (vector-scalar (/ maximum-speed new-velocity-modular)
                                                                         new-velocity))))))))
      ((set-linear-acceleration) (lambda (self la)
                                   (set! linear-acceleration
                                         la))))))

;; Test
(define gmt (make-game-object #(0 0) #(1 3) #(1 3) #(2 2) 10 10))
(print-out (ask gmt 'linear-acceleration))
(ask gmt 'set-linear-acceleration #(2 6))
(print-out (ask gmt 'linear-acceleration))
(exit)

;; Abstract Class: Torpedo
;; the class to denote bastic information about the




;; The ship object
;; The ship object has the following variables and methods
;; Variable:
;;   name: string
;;   location: two-dimensional numeric vector
;;   velocity: two-dimensional numeric vector
;;   maximum-speed: numeric
;;   maximum-torpedo: numeric
;;   remaining-torped: numeric
;; Method:
;;   move: ( -> two-dimensional numeric vector)
;;       add the velocity to the current location
;;       return the current location
;;   steer: (two-dimensional numeric vector -> two-dimensional numeric vector)
;;       change the velocity
;;       return the current velocity
;;   fire-torpedo
