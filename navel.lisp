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

;; (flet ((window (start trades)
;; 	 (if (null trades) (list start 0 nil)
;; 	   (multiple-value-call 'list start
;; 	     (length trades) (trades-profits trades)))))
;;   (loop with windows
;; 	for trades = (slot-reduce *maker* lictor trades)
;; 	then (nthcdr window-count trades) ; FUCK A DUCK A RANDOM DUCK
;; 	for window-start = (timestamp (first trades)) then window-close
;; 	for window-close = (timestamp- window-start 1 :day)
;; 	for window-count = (position window-close trades
;; 				     :key #'timestamp
;; 				     :test #'timestamp>=)
;; 	for window = (window window-start
;; 			     (if (null window-count) trades
;; 			       (subseq trades 0 window-count)))
;; 	while window-count do (push window windows)
;; 	finally (return (cons window windows))))

;; (defmethod describe-account :after (supplicant exchange stream)
;;   (destructuring-bind (first . rest) (slot-reduce supplicant lictor trades)
;;     (dolist (day (reduce (lambda (days trade)
;;                            (destructuring-bind ((day . trades) . rest) days
;;                              (let ((next (timestamp trade)))
;;                                (if (= (day-of next) (day-of day))
;;                                    (cons (list* day trade trades) rest)
;;                                    (acons next trade days)))))
;;                          rest :initial-value (acons (timestamp first) first ())))
;;       (multiple-value-call 'format stream "~&~@{~A~#[~:; ~]~}~%" name
;;                            (trades-profits day)))))

(defmacro do-makers ((maker &optional (stem "MAKER*")) &body body)
  `(dolist (,maker (mapcar 'symbol-value (apropos-list ,stem)))
     ,@body))

(defmethod squash-reservations ((maker maker))
  "This documentation string is useless on Krakatoa!"
  (squash-reservations (slot-reduce maker supplicant treasurer)))
