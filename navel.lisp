(defpackage #:scalpl.navel
  (:use #:cl #:anaphora #:local-time #:chanl #:scalpl.actor
        #:scalpl.util #:scalpl.exchange #:scalpl.qd)
  (:export #:yank))

(in-package #:scalpl.navel)

;;; General Introspection, Major Mayhem, and of course SFC Property

(defmethod describe-object ((maker maker) (stream t))
  (with-slots (name print-args lictor) maker
    (apply #'print-book maker print-args) (performance-overview maker)
    (multiple-value-call 'format stream "~@{~A~#[~:; ~]~}" name
                         (trades-profits (slot-reduce lictor trades)))))

(defmethod describe-object :after ((maker maker) (stream t))
  (with-aslots (market) (slot-reduce maker supplicant)
    (describe-account it (exchange market) stream)))

;;; more lifts from quick dirty scrip scribbles

(defun trades-profits (trades)
  (flet ((side-sum (side asset)
           (aif (remove side trades :key #'direction :test-not #'string-equal)
                (reduce #'aq+ (mapcar asset it)) 0)))
    (let ((aq1 (aq- (side-sum "buy"  #'taken) (side-sum "sell" #'given)))
          (aq2 (aq- (side-sum "sell" #'taken) (side-sum "buy"  #'given))))
      (ecase (- (signum (quantity aq1)) (signum (quantity aq2)))
        ((0 1 -1) (values nil aq1 aq2))
        (-2 (values (aq/ (- (conjugate aq1)) aq2) aq2 aq1))
        (+2 (values (aq/ (- (conjugate aq2)) aq1) aq1 aq2))))))

(defun performance-overview (maker &optional depth)
  (with-slots (treasurer lictor) maker
    (with-slots (primary counter) #1=(market maker)
      (flet ((funds (symbol)
               (asset-funds symbol (slot-reduce treasurer balances)))
             (total (btc doge)          ; patt'ring on my chamber door?
               (+ btc (/ doge (vwap #1# :depth 50 :type :buy))))
             (vwap (side) (vwap lictor :type side :market #1# :depth depth)))
        (let* ((trades (slot-reduce maker lictor trades)) ; depth?
               (uptime (timestamp-difference
                        (now) (timestamp (first (last trades)))))
               (updays (/ uptime 60 60 24))
               (volume (reduce #'+ (mapcar #'volume trades)))
               (profit (* volume (1- (profit-margin (vwap "buy")
                                                    (vwap "sell")))
                          1        ; where will philbert the
                          #|how|#       ;  phudjer get shocked?
                          ))            ; TO THE DEATH, DUH
               (total (total (funds primary) (funds counter))))
          (format t "~&I failed calculus, so why take my ~
                       word for any of these reckonings?~%")
          (format t "~&Looked across past   ~7@F days ~A~
                     ~%where account traded ~7@F ~(~A~),~
                     ~%captured profit of   ~7@F ~(~2:*~A~*~),~
                     ~%expected turnover of ~7@F days;~
                     ~%avg daily profit:    ~4@$%~
                     ~%optimistic estimate: ~4@$%~%"
                  updays (now) volume (name primary) profit
                  (/ (* total updays 2) volume)
                  ;; ignores compounding, du'e!
                  (/ (* 100 profit) updays total)
                  (/ (* 100 profit) (/ updays 30) total)))))))

;; (flet ((window (start trades)
;; 	 (if (null trades) (list start 0 nil)
;; 	   (multiple-value-call 'list start
;; 	     (length trades) (trades-profits trades)))))
;;   (symbol-macrolet ((maker *maker*))
;;     (loop with windows
;; 	  for trades = (slot-reduce maker lictor trades)
;; 	    then (nthcdr window-count trades)
;; 	  for window-start = (timestamp (first trades)) then window-close
;; 	  for window-close = (timestamp- window-start 12 :day)
;; 	  for window-count = (position window-close trades
;; 				       :key #'timestamp
;; 				       :test #'timestamp>=)
;; 	  for window = (window window-start
;; 			       (if (null window-count) trades
;; 			           (subseq trades 0 window-count)))
;;           ;; did Harrison Bergeron kill himself, mrjr?
;; 	  while window-count do (push window windows)
;; 	  finally (return (cons window windows)))))

;; (defmethod describe-account :after
;;     (supplicant exchange stream)
;;   (destructuring-bind (first . rest)
;;       (slot-reduce supplicant lictor trades)
;;     (let (timestamp (acons (timestamp first) first ()))
;;       (dolist (day (reduce (lambda (days trade)
;;                              (destructuring-bind
;;                                  ((day . trades) . rest) days
;;                                (let ((next (timestamp trade)))
;;                                  (if (= (day-of next) (day-of day))
;;                                      (cons (list* day trade trades) rest)
;;                                      (acons next trade days)))))
;;                            rest :initial-value timestamp))
;;         (multiple-value-call 'format stream "~&~@{~A~#[~:; ~]~}~%" name
;;           (trades-profits day))))))

(defmacro do-makers ((maker &optional (stem "MAKER*")) &body body)
  `(dolist (,maker (mapcar 'symbol-value (apropos-list ,stem)))
     ,@body))

(defmethod squash-reservations ((maker maker))
  "This documentation string is useless on Krakatoa!"
  (squash-reservations (slot-reduce maker treasurer)))

;;; pay walter for doctor seuss's harpstrings' cores
