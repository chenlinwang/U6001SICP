(define (loadtmp) (load "tmp.scm"))
(define (loadsys) (load "objsys.scm"))
(define (loadtype) (load "objtypes.scm"))
(define (loadset) (load "setup.scm"))
(define (loadall)
  (loadsys)
  (loadtype)
  (loadset))

(loadall)
;; ;; Test for debug
;; (define c (create-container))
;; (write-line (ask c 'HAVE-THING? 'tese))

;; ;; Warmup exercise 1
(setup 'Chenlin)
;;(ask (ask me 'location) 'name)
;;(ask me 'look-around)


;; ;; Warming up execise 3, drawing the map of the mit
;; (define mit (create-world))

;; (define locnum (map (lambda (loc)
;;                       (cons
;;                        (length (ask loc 'exits))
;;                        loc))
;;                     mit))

;; ;; slow mit-scheme
;; (define (slow-sort l . cal)
;;   (set! cal (cond ((null? cal) (lambda (x) x))
;;                   ((procedure? (car cal)) (car cal))
;;                   (else (lambda (x) x))))

;;   (let iter1 ((order '())
;;               (rest l))
;; ;;    (write-line order)
;;     (cond ((null? rest) order)
;;           (else
;;            (let iter2 ((before '())
;;                        (largest (car rest))
;;                        (after (cdr rest)))
;;              ;; (write-line before)
;;              ;; (write-line largest)
;;              ;; (write-line after)
;;              (cond ((null? after)
;;                     (iter1 (cons largest order)
;;                            before))
;;                    ((>= (cal largest) (cal (car after)))
;;                     (iter2 (append before (list (car after)))
;;                            largest
;;                            (cdr after)))
;;                    (else
;;                     (iter2 (cons largest before)
;;                            (car after)
;;                            (cdr after)))))))))
;; ;; test
;; ;; (define t '(2 3 54 2 4 5 1 2 3 0))
;; ;; ;; (write-line (slow-sort t))
;; ;; (define t '((2) (3) (54) (2) (4) (5) (1) (2) (3) (0)))
;; ;; (write-line (slow-sort t car))

;; (set! locnum (slow-sort locnum car))

;; (map (lambda (loc1)
;;        (let ((loc1 (cdr loc1)))
;;          (display (ask loc1 'name))
;;          (display ":\t")
;;          (let ((next (map (lambda (e)
;;                             (let ((loc2 (ask e 'to)))
;;                               (list (ask e 'direction)
;;                                     (ask loc2 'name))))
;;                           (ask loc1 'exits))))
;;            (display (slow-sort next (lambda (p)
;;                                       (let ((d (car p)))
;;                                         (case d
;;                                           ((east) 1)
;;                                           ((north) 2)
;;                                           ((west) 3)
;;                                           ((south) 4)
;;                                           ((up) 5)
;;                                           ((down) 6)
;;                                           (else (error "wrong direction! -- " d))))))))
;;          (newline)))
;;      locnum)

;; Computer Exercise 1
;; Thing part
;; The show procedure could show instance as well as the handler, so just move up the inheritance tree until reach the thing part.

;;  HANDLER: #[compound-procedure 23 handler]
;;  TYPE: thing
;; (methods (install #[compound-procedure 28])
;;                   (location #[compound-procedure 27])
;;                            (destroy #[compound-procedure 26])
;;                                     (emit #[compound-procedure 25]))
;;   Parent frame: #[environment 29]
;;   named-part:   #[compound-procedure 30 handler]
;;     Parent frame: global-environment
;;     self:         (instance #[compound-procedure 2 handler])
;;     name:         chenlin
;;     location:     (instance #[compound-procedure 8 handler])
;; Value: handler

;; Container part
;;  HANDLER: #[compound-procedure 16 handler]
;;  TYPE: container
;; (methods (things #[compound-procedure 34])
;;                   (have-thing? #[compound-procedure 33])
;;                            (add-thing #[compound-procedure 32])
;;                                     (del-thing #[compound-procedure 31]))
;;   Parent frame: #[environment 35]
;;   root-part:    #[compound-procedure 36 handler]
;;   things:       ()
;;     Parent frame: global-environment
;;     self:         (instance #[compound-procedure 2 handler])
;; Value: handler


;; Self
;; The self is just a list of a tag #instance and the handler. It will not change unless you go up to the super class to find inherited methods.

;; Computer Exercise 2
;; Done

;; Computer Exercise 3
;; Done

;; Computer Exercise 4

;; (define (create-the-ring name location)
;;   (create-instance ring-of-obfuscation name location))

;; (define (ring-of-obfuscation self name location)
;;   (let ((mobile-part (mobile-thing self name location)))
;;     (make-handler
;;      'ring-of-obfuscation
;;      (make-methods
;;       'speak
;;       (lambda ()
;;         (write-line "One ring to rule them all!")))
;;      mobile-part)))


;; ;; create the ring at the place I stand.
;; (define r1 (create-the-ring 'ring-one (ask me 'location)))
;; (define me2 (create-avatar 'chenlin-avatar (ask me 'location)))
;; (ask me 'location)
;; (ask me 'look-around)
;; (newline)
;; (ask me 'feel-the-force)
;; (newline)
;; (ask me2 'take r1)
;; (newline)
;; (ask me 'look-around)
;; (newline)
;; (ask me 'feel-the-force)

;; Computer Exercise 5
;; (define (create-wand name location)
;;   (create-instance wand name location))

;; (define (wand self name location)
;;   (let ((mobile-part (mobile-thing self name location)))
;;     (make-handler
;;      'wand
;;      (make-methods
;;       'HOLDER
;;       (lambda ()
;;         (let ((holder (ask self 'location)))
;;           (if (ask holder 'is-a 'person)
;;               holder
;;               (ask self 'emit (list
;;                                'wand
;;                                (ask self 'name)
;;                                'has
;;                                'no
;;                                'holders.)))))

;;       'ZAP
;;       (lambda (target)
;;         (let ((holder (ask self 'holder)))
;;           (if holder
;;               (let ((spells (ask holder 'has-a 'spell)))
;;                 (if (null? spells)
;;                     (ask holder 'emit
;;                          (list (ask holder 'name)
;;                                'waved
;;                                'hardly
;;                                'at
;;                                (ask target 'name)
;;                                '.
;;                                'But 'nothing 'happened.))
;;                     (let ((spell (pick-random spells)))
;;                       (ask holder 'emit
;;                            (list (ask holder 'name)
;;                                  'waved
;;                                  'hardly
;;                                  'at
;;                                  (ask target 'name)
;;                                  '--
;;                                  'Saying
;;                                  '--
;;                                  (ask spell 'incant)
;;                                  ))
;;                       (ask spell 'use holder target))))
;;               #F)))

;;       'WAVE
;;       (lambda ()
;;         (let ((holder (ask self 'holder)))
;;           (if holder
;;               (let ((target (pick-random
;;                              (ask (ask holder 'location)
;;                                   'things))))
;;                 (ask self 'zap target))
;;               #F)))
;;       )
;;      mobile-part)))

;; (define w (create-wand 'wooden-wand (ask me 'location)))
;; (ask me 'look-around)
;; (ask w 'holder)
;; (ask me 'take-all)
;; (ask w 'wave)

;; Computer Exercise 6
;; (define w (create-wand 'wooden-wand (ask me 'location)))
;; (define s (create-spell 'wind-of-doom (ask me 'location)
;;                         "windiano doomino"
;;                         (lambda (caster target)
;;                           (let ((prefix (list 'A 'blazed 'wind 'came 'at
;;                                               (ask target 'name)
;;                                               '--)))
;;                             (cond ((ask target 'is-a 'person)
;;                                    (ask target 'emit (append prefix
;;                                                              (list (ask target 'name)
;;                                                                    'yelled
;;                                                                    'with
;;                                                                    'great
;;                                                                    'pain)))
;;                                    (ask target 'suffer 2 caster))
;;                                   (else
;;                                    (ask target 'emit (append prefix
;;                                                              (list
;;                                                               (ask target 'name)
;;                                                               'broke
;;                                                               'into
;;                                                               'pieces)))
;;                                    (ask target 'destroy)))))))

;; (define r (create-the-ring 'ring (ask me 'location)))
;; (ask me 'take w)
;; (ask me 'take s)
;; (define me2 (create-avatar 'chenlin-avartar (ask me 'location)))
;; (ask me 'look-around)
;; (ask w 'zap r)
;; (ask w 'zap me2)

;; Computer Exercise 7
;; (define (create-wit-student name birthplace activity miserly)
;;   (create-instance wit-student name birthplace activity miserly))


;; (define (wit-student self name birthplace activity miserly)
;;   (let ((auto-part (autonomous-person self name birthplace activity miserly)))
;;     (make-handler
;;      'WIT-STUDENT
;;      (make-methods
;;       'INSTALL
;;       (lambda ()
;;         (ask auto-part 'install)
;;         (ask clock 'add-callback
;;              (create-clock-callback 'ZAP-OR-WAVE self
;;                                     'zap-or-wave)))
;;       'ZAP-OR-WAVE
;;       (lambda ()
;;         (let ((current-loc (ask self 'location)))
;;           ;; finding a wand
;;           (let ((wands (find-all current-loc 'wand)))
;;             (if (not (null? wands))
;;                 (map (lambda (w)
;;                        (ask self 'take w))
;;                      wands)
;;                 (ask self 'say '(no wands in the current room))))

;;           ;; See if there are wands to use
;;           (let ((wands (find-all self 'wand)))
;;             (if (null? wands)
;;                 (ask self 'say '(damn! no wand to practice))
;;                 (let ((wand (pick-random wands)))
;;                   ;; find a people to use
;;                   (let ((people (find-all current-loc 'person)))
;;                     (cond ((null? people)
;;                            (ask self 'say '(no people around, just wave the wand))
;;                            (ask wand 'wave))
;;                           (else
;;                            (let ((random-perpson (pick-random people)))
;;                              (ask self 'say (list
;;                                            'find
;;                                            (ask random-perpson
;;                                                 'name)
;;                                            'zap
;;                                            'at
;;                                            (ask random-perpson
;;                                                 'name)))
;;                              (ask wand 'zap random-perpson)))))))))))
;;      auto-part)))


;; Computer Exercise 8
;; (define (create-wit-professor name birthplace activity miserly)
;;   (create-instance wit-professor name birthplace activity miserly))

;; (define (wit-professor self name birthplace activity miserly)
;;   (let ((auto-part (autonomous-person self name birthplace activity miserly)))
;;     (make-handler
;;      'wit-professor
;;      (make-methods
;;       'INSTALL
;;       (lambda ()
;;         (ask auto-part 'install)
;;         (ask clock 'add-callback
;;              (create-clock-callback 'TEACH self 'teach)))
;;       'TEACH
;;       (lambda ()
;;         (let ((students (find-all (ask self 'location) 'wit-student)))
;;           (cond ((null? students)
;;                  (ask self 'say '(no students around looks like time to relax)))
;;                 (else
;;                  (map (lambda (s)
;;                         (let ((teached-spell (clone-spell (pick-random (ask chamber-of-stata 'things)) s)))
;;                           (ask self 'say (list 'look (ask s 'name) 'the (ask teached-spell 'name) 'is 'cast 'like 'this))
;;                           (ask s 'say (list 'I 'now 'know 'how 'to 'cast (ask teached-spell 'name)))))
;;                       students)))))
;;       )
;;      auto-part)))

;; (run-clock 4)

;; Computer Exercise 9
;; (define me2 (create-avatar 'chenlin2 (ask me 'location)))
;; (define w1 (create-good-wand '11-inch-willow me))
;; (define w2 (create-good-wand '13-inch-steel me2))
;; (for-each (lambda (s)
;;             (clone-spell s me)
;;             (clone-spell s me2))
;;           (ask chamber-of-stata 'things))
;; (ask me 'look-around)
;; (ask me2 'look-around)
;; (ask w1 'cast 'boil-spell me2)
;; (ask w1 'cast 'slug-spell me2)
;; (ask w1 'cast 'wind-of-doom me2)
;; (ask w1 'cast 'transport me2)
;; (restart 1)

;; Computer Exercise 10
;; (define (create-the-chosen-one name birthplace)
;;   (create-instance the-chosen-one name birthplace))

;; (define (the-chosen-one self name birthplace)
;;   (let ((person-part (person self name birthplace)))
;;     (make-handler
;;      'the-chosen-one
;;      (make-methods
;;       'SUFFER
;;       (lambda (hits perp)
;;         (let ((after-hit-health (- (ask self 'health) hits)))
;;           (cond ((<= after-hit-health 0)
;;                  (ask self 'emit (list (ask self 'name)
;;                                        'scar 'suddernly 'flared 'brightly 'and
;;                                        'a 'flush 'light 'emited 'from 'the
;;                                        (ask self 'name)))

;;                  ;; avoid the infinite loop of the two chosen ones to kill each other
;;                  (cond ((not (ask perp 'is-a 'the-chosen-one))
;;                         (ask perp 'emit (list (ask perp 'name) 'was 'hitted 'by 'the 'flush 'of 'light))
;;                         (ask perp 'die self))
;;                        (else
;;                         (ask perp 'suffer (min hits
;;                                                (- (ask perp 'health) 1))
;;                              self))))
;;                 (else
;;                  (ask person-part 'suffer hits perp)))))
;;       )
;;      person-part)))

;; (define chosen-one (create-the-chosen-one 'harry (ask me 'location)))
;; (define chosen-one1 (create-the-chosen-one 'larry (ask me 'location)))
;; (define w1 (create-good-wand '11-inch-willow me))
;; (define w2 (create-good-wand '13-inch-wood chosen-one1))
;; (for-each (lambda (s)
;;             (clone-spell s me)
;;             (clone-spell s chosen-one1))
;;           (ask chamber-of-stata 'things))
;; (ask me 'look-around)
;; (ask w1 'cast 'wind-of-doom chosen-one)
;; (ask w2 'cast 'wind-of-doom chosen-one)
