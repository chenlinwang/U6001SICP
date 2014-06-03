;;
;;  The play-loop procedure takes as its  arguments two prisoner's
;;  dilemma strategies, and plays an iterated game of approximately
;;  one hundred rounds.  A strategy is a procedure that takes
;;  two arguments: a history of the player's previous plays and 
;;  a history of the other player's previous plays.  The procedure
;;  returns either a "c" for cooperate or a "d" for defect.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (record s0 s1)
  (define start (get-universal-time))
  (play-loop s0 s1)
  (define end (get-universal-time))
  (display "Time consumed:")
  (display (- end start))
  (newline))

(define (play-loop strat0 strat1)
  (define (play-loop-iter strat0 strat1 count history0 history1 limit)
    (cond ((= count limit) (print-out-results history0 history1 limit))
	  (else (let ((result0 (strat0 history0 history1))
		      (result1 (strat1 history1 history0)))
		  (play-loop-iter strat0 strat1 (+ count 1)
				  (extend-history result0 history0)
				  (extend-history result1 history1)
				  limit)))))
  (play-loop-iter strat0 strat1 0 the-empty-history the-empty-history
		  (+ 500 (random 500))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  The following procedures are used to compute and print
;;  out the players' scores at the end of an iterated game
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (print-out-results history0 history1 number-of-games)
  (let ((scores (get-scores history0 history1)))
    (newline)
    (display "Player 1 Score:  ")
    (display (* 1.0 (/ (car scores) number-of-games)))
    (newline)
    (display "Player 2 Score:  ")
    (display (* 1.0 (/ (cadr scores) number-of-games)))
    (newline)))

(define (get-scores history0 history1)
  (define (get-scores-helper history0 history1 score0 score1)
    (cond ((empty-history? history0)
	   (list score0 score1))
	  (else (let ((game (make-play (most-recent-play history0)
				       (most-recent-play history1))))
		  (get-scores-helper (rest-of-plays history0)
				     (rest-of-plays history1)
				     (+ (get-player-points 0 game) score0)
				     (+ (get-player-points 1 game) score1))))))
  (get-scores-helper history0 history1 0 0))

(define (get-player-points num game)
  (list-ref (get-point-list game) num))

(define *game-association-list*
  ;; format is that first sublist identifies the players' choices 
  ;; with "c" for cooperate and "d" for defect; and that second sublist 
  ;; specifies payout for each player
  '((("c" "c") (3 3))
    (("c" "d") (0 5))
    (("d" "c") (5 0))
    (("d" "d") (1 1))))

(define (get-point-list game)
  (cadr (extract-entry game *game-association-list*)))

;; note that you will need to write extract-entry
(define (extract-entry a-play *game-association-list*)
  (define (ex-iter game-list num)
    (if (= 0 num)
        (display "Illegal Play, Please Try Again!\n")
        (if (check-play a-play (car (car game-list)))
            (car game-list)
            (ex-iter (cdr game-list) (- num 1)))))
  (ex-iter *game-association-list* 4))


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
