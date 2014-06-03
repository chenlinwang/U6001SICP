;;two players' strategies
;; A sampler of strategies
(define (NASTY my-history other-history)
  "d")

(define (PATSY my-history other-history)
  "c")

(define (SPASTIC my-history other-history)
  (if (= (random 2) 0)
      "c"
      "d"))

(define (EGALITARIAN  my-history other-history)
  (define (count-instances-of test hist)
    (cond ((empty-history? hist) 0)
          ((string=? (most-recent-play hist) test)
           (+ (count-instances-of test (rest-of-plays hist)) 1))
          (else (count-instances-of test (rest-of-plays hist)))))
  (let ((ds (count-instances-of "d" other-history))
        (cs (count-instances-of "c" other-history)))
    (if (> ds cs) "d" "c")))

(define (EGALITARIAN2 my-history other-history)
  (define (major-count cs ds hist)
    (cond ((empty-history? hist)
           (if (> cs ds)
               "c"
               "d"))
          ((string=? (most-recent-play hist) "c")
           (major-count (+ cs 1) ds (rest-of-plays hist)))
          (else
           (major-count cs (+ ds 1) (rest-of-plays hist)))))
  (major-count 0 0 other-history))

(define (EYE-FOR-EYE my-history other-history)
  (if (empty-history? my-history)
      "c"
      (most-recent-play other-history)))

(define (EYE-FOR-TWO-EYES my-history other-history)
  (if (empty-history? my-history)
      "c"
      (if (and (string=? "d" (car other-history))
               (string=? "d" (car (cdr other-history))))
          "d"
          "c")))

(define (EYE-FOR-N-EYES num)
  (define (count-ds left hist)
    (cond ((= 0 left) "d")
          ((empty-history? hist) "c")
          ((string=? "d" (most-recent-play hist))
           (count-ds (- left 1) (rest-of-plays hist)))
          (else "c")))
  (define (neye my-history other-history)
    (count-ds num other-history))
  neye)

(define (make-rotating-stratey s0 s1 f0 f1)
  (define fs (+ f0 f1))
  (define (fst my-history other-history)
    (if (< (remainder (length other-history)) f0)
        (s0 my-history other-history)
        (s1 my-history other-history)))
  fst)

(define (make-higher-order-spastic sl)
  (define slnum (length sl))
  (define (mhs my-history other-history)
    (let ((order (remainder (length my-history) slnum)))
      ((list-ref sl order) my-history other-history)))
  mhs)

(define (gentle s gentleness)
  (define (gs my-history other-history)
    (let ((sr (s my-history other-history)))
      (if (string=? s "d")
          (if (< (random 1.0) gentleness)
              "c"
              "d")
          "c")))
  gs)

;;basics
(define (check-play play-a play-b)
  (and (string=? (car play-a) (car play-b))
       (string=? (cadr play-a)
                 (cadr play-b))))

(define make-play list)

(define the-empty-history '())

(define extend-history cons)
(define empty-history? null?)

(define most-recent-play car)
(define rest-of-plays cdr)


(define *game-association-list*
    (list (list (list "c" "c" "c") (list 4 4 4))
          (list (list "c" "c" "d") (list 2 2 5))
          (list (list "c" "d" "c") (list 2 5 2))
          (list (list "d" "c" "c") (list 5 2 2))
          (list (list "c" "d" "d") (list 0 3 3))
          (list (list "d" "c" "d") (list 3 0 3))
          (list (list "d" "d" "c") (list 3 3 0))
          (list (list "d" "d" "d") (list 1 1 1))))

;;extract-entry
(define (extract-entry play-a *game-association-list*)
    (define (ex-iter playlist number)
        (if (= number 0)
            (display "Invalid input, please enter again!\n")
            (let ((play (car (car playlist)))
                  (playvalue (car playlist)))
              (if (and (string=? (car play-a) (car play))
                       (string=? (car (cdr play-a)) (car (cdr play)))
                       (string=? (cadr (cdr play-a)) (cadr (cdr play))))
                  playvalue
                  (ex-iter (cdr playlist) (- number 1))))))
  (ex-iter *game-association-list* 8))

(define (get-point-list game)
    (cadr (extract-entry game *game-association-list*)))

(define (get-player-point num game)
    (list-ref (get-point-list game) num))


(define (get-score his0 his1 his2)
    (define (getiter h0 h1 h2 s0 s1 s2)
        (if (empty-history? h0)
            (list s0 s1 s2)
            (let ((play (make-play (most-recent-play h0)
                                   (most-recent-play h1)
                                   (most-recent-play h2))))
              (getiter (rest-of-plays h0)
                       (rest-of-plays h1)
                       (rest-of-plays h2)
                       (+ (get-player-point 0 play) s0)
                       (+ (get-player-point 1 play) s1)
                       (+ (get-player-point 2 play) s2)))))
  (getiter his0 his1 his2 0 0 0))

(define (print-out-result his0 his1 his2 num-of-game)
    (let ((scores (get-score his0 his1 his2)))
      (newline)
      (display "Total Game Number:")
      (display num-of-game)
      (newline)
      (display "Player 1:")
      (display (* 1.0 (/ (car scores) num-of-game)))
      (newline)
      (display "Player 2:")
      (display (* 1.0 (/ (car (cdr scores)) num-of-game)))
      (newline)
      (display "Player 3:")
      (display (* 1.0 (/ (cadr (cdr scores)) num-of-game)))
      (list his0 his1 his2)))

(define (play-loop s0 s1 s2)
    (define total (+ 100 (random 100)))
    (define (loopiter h0 h1 h2 num)
        (if (= 0 num)
            (print-out-result h0 h1 h2 total)
            (let ((a0 (s0 h0 h1 h2))
                  (a1 (s1 h1 h0 h2))
                  (a2 (s2 h2 h0 h1)))
              (loopiter (extend-history a0 h0)
                 (extend-history a1 h1)
                 (extend-history a2 h2)
                 (- num 1)))))
    (loopiter the-empty-history the-empty-history the-empty-history total))

;; Strategies
(define (p3 mh h0 h1) "c")
(define (n3 mh h0 h1) "d")
(define (s3 mh h0 h1)
    (if (= 0 (random 2))
        "c"
        "d"))

;;Tough eye for eye: if either plays "d", play "d"
(define (te3 mh h0 h1)
    (if (empty-history? mh)
        "c"
        (if (or (string=? "d" (most-recent-play h0))
                (string=? "d" (most-recent-play h1)))
            "d"
            "c")))

;;Soft eye for eye: if both play "d", play "d"
(define (se3 mh h0 h1)
    (if (empty-history? mh)
        "c"
        (if (and (string=? "d" (most-recent-play h0))
                 (string=? "d" (most-recent-play h1)))
            "d"
            "c")))

(define (make-combined-strategy s0 s1 combined)
    (define (news mh h0 h1)
        (combined (s0 mh h0)
                  (s1 mh h1)))
  news)

(define (c1 r1 r2)
    (if (and (string=? "d" r1)
             (string=? "d" r2))
        "d"
        "c"))

(define (c1 r1 r2)
    (if (or (string=? "d" r1)
            (string=? "d" r2))
        "d"
        "c"))

;;Analysis Module to analyse the strategy
;;Constructor, the constructor that takes in the history list and return the asked data
;;add data list
(define (add-data ol al)
    (list (+ (car ol) (car al))
          (+ (list-ref ol 1)
             (list-ref al 1))
          (+ (caddr ol) (caddr al))))

;;  ;;This parts help to tell the machine what the decisions are
;;  | my strategy | p0 strategy | p1 strategy | flag |
;;  |-------------+-------------+-------------+------|
;;  | c           | c           | c           |    0 |
;;  | d           | c           | c           |    1 |
;;  | c           | d           | c           |    2 |
;;  | d           | d           | c           |    3 |
;;  | c           | c           | d           |    4 |
;;  | d           | c           | d           |    5 |
;;  | c           | d           | d           |    6 |
;;  | d           | d           | d           |    7 |
;;  ;;Value Weight of each parameter
;; | items | my strategy | p0 strategy | p1 strategy |
;; |-------+-------------+-------------+-------------|
;; | score | 1           | 2           | 4           |
(define (tell-flag m s0 s1)
    (define flag0
        (if (string=? "d" m)
            1
            0))
  (define flag1
      (if (string=? "d" s0)
          2
          0))
  (define flag2
      (if (string=? "d" s1)
          4
          0))
  (+ flag0 flag1 flag2))

(define (test-flag)
    (display (tell-flag "c" "c" "c"))
  (newline)
  (display (tell-flag "d" "c" "c"))
  (newline)
  (display (tell-flag "c" "d" "c"))
  (newline)
    (display (tell-flag "d" "d" "c"))
  (newline)
    (display (tell-flag "c" "c" "d"))
  (newline)
    (display (tell-flag "d" "c" "d"))
  (newline)
    (display (tell-flag "c" "d" "d"))
  (newline)
    (display (tell-flag "d" "d" "d"))
  (newline)
    )

(define (make-summary historylist)
    (define (siter mh h0 h1 cc cd dd)
        (if (empty-history? h0)
            (list cc cd dd)
            (let ((flag (tell-flag (car mh)
                                   (car h0)
                                   (car h1)))
                  (ml (cdr mh))
                  (l0 (cdr h0))
                  (l1 (cdr h1)))
              (cond ((= flag 0) (siter ml l0 l1 (add-data cc (list 1 0 1)) cd dd))
                    ((= flag 1) (siter ml l0 l1 (add-data cc (list 0 1 1)) cd dd))
                    ((= flag 2) (siter ml l0 l1 cc (add-data cd (list 1 0 1)) dd))
                    ((= flag 3) (siter ml l0 l1 cc (add-data cd (list 0 1 1)) dd))
                    ((= flag 4) (siter ml l0 l1 cc (add-data cd (list 1 0 1)) dd))
                    ((= flag 5) (siter ml l0 l1 cc (add-data cd (list 0 1 1)) dd))
                    ((= flag 6) (siter ml l0 l1 cc cd (add-data dd (list 1 0 1))))
                    ((= flag 7) (siter ml l0 l1 cc cd (add-data dd (list 0 1 1))))))))
  (siter (car historylist) (cdr (cadr historylist)) (cdr (caddr historylist)) (list 0 0 0)
         (list 0 0 0) (list 0 0 0))
  )

(define (get-p summary)
    (let ((cc (car summary))
          (cd (cadr summary))
          (dd (caddr summary)))
      (list (if (> (caddr cc) 0)
                (* 1.0 (/ (car cc) (caddr cc)))
                (list))
            (if (> (caddr cd) 0)
                (* 1.0 (/ (car cd) (caddr cd)))
                (list))
            (if (> (caddr dd) 0)
                (* 1.0 (/ (car dd) (caddr dd)))
                 (list)))))

(define (test-entry index trial)
    (cond ((null? index)
           (null? trial))
          ((null? trial) #f)
          ((= (car index) (car trial))
           (test-entry (cdr index) (cdr trial)))
          (else #f)))

(define (test-maker pout name)
    (define (t0 s)
        (define p (get-p (make-summary (play-loop s s3 s3))))
      (if   (test-entry pout
                        (map (lambda (input) (cond ((null? input) 1)
                                                   ((= 1 input) 1)
                                                   (else input)))
                             p))
            (begin (display "\nThis is the ")
                   (display name)
                   (display " strategy!\n")
                   #t)
            (begin (display "\nThis is not the ")
                   (display name)
                   (display " strategy!\n")
                   #f)))
  t0)

(define test-for-fool (test-maker (list 1 1 1) "fool"))

(define (print-sample sample)
    (define (printiter mh h0 h1)
        (if (empty-history? h0)
            #t
            (begin (newline)
                   (display (car mh))
                   (display (car h0))
                   (display (car h1))
                   (newline)
                   (printiter (cdr mh)
                              (cdr h0)
                              (cdr h1)))))
  (printiter (car sample) (cdr (cadr sample)) (cdr (caddr sample))))

(define test-for-se3 (test-maker (list 1 1 0) "soft eye-for-eye"))
(define test-for-te3 (test-maker (list 1 0 0) "tough eye-for-eye"))
;;in expected-values: #f = don't care
;;X = actual-value needs to be #f or X
(define (test-entry expected-values actual-values)
  (cond ((null? expected-values) (null? actual-values))
        ((null? actual-values) #f)
        ((or (not (car expected-values))
             (not (car actual-values))
             (= (car expected-values) (car actual-values)))
         (test-entry (cdr expected-values) (cdr actual-values)))
        (else #f)))

(define (is-he-a-fool? hist0 hist1 hist2)
  (test-entry (list 1 1 1)
              (get-probability-of-c
               (make-history-summary hist0 hist1 hist2))))

(define (could-he-be-a-fool? hist0 hist1 hist2)
 (test-entry (list 1 1 1)
             (map (lambda (elt)
                     (cond ((null? elt) 1)
                           ((= elt 1) 1)
                           (else 0)))
                  (get-probability-of-c (make-history-summary hist0
                                                              hist1
                                                              hist2)))))
