;;;; exchange.lisp

(defpackage #:scalpl.exchange
  (:use #:cl #:anaphora #:local-time #:scalpl.util)
  (:export #:exchange #:assets #:markets
           #:asset #:find-asset #:name #:bids #:asks
           #:market #:find-market #:decimals #:base #:quote
           #:offer #:placed #:bid #:ask #:offer #:offer
           #:volume #:price #:uid #:consumed-asset #:trade
           #:parse-timestamp #:gate #:gate-post #:gate-request
           #:thread #:control #:updater #:worker #:output ; UGH
           #:get-book #:trades-since #:trades-tracker #:book-tracker
           #:placed-offers #:market-fee #:execution-since #:vwap
           #:post-offer #:cancel-offer #:execution #:execution-tracker
           #:direction #:cost #:fee))

(in-package #:scalpl.exchange)

;;; TODO
;;; This file should lay out the interface that each exchange client needs to
;;; implement. Each exchange client should instantiate the exchange class and
;;; specialize methods on generic functions, which should be defined here.

;;;
;;; Exchanges
;;;

(defclass exchange ()
  ((name    :initarg :name    :reader name)
   (assets  :initarg :assets  :reader assets)
   (markets :initarg :markets :reader markets)
   (market-timestamp-sensitivity :initarg :sensitivity)))

(defmethod shared-initialize :after ((exchange exchange) names &key)
  (with-slots (assets markets) exchange
    (dolist (asset assets) (setf (slot-value asset 'exchange) exchange))
    (dolist (market markets) (setf (slot-value market 'exchange) exchange))))

(defmethod print-object ((exchange exchange) stream)
  (print-unreadable-object (exchange stream :type nil :identity nil)
    (princ (name exchange) stream)))

(defgeneric parse-timestamp (exchange timestamp)
  ;; most common is a unix timestamp
  (:method ((exchange exchange) (timestamp real))
    (multiple-value-bind (sec nsec) (floor timestamp)
      (unix-to-timestamp sec :nsec (round (* (expt 10 9) nsec)))))
  (:method ((exchange exchange) (timestamp string))
    ;; asdfjkshanueodaoseitcgaeosundantheoudvwe;ercgu
    (parse-timestamp exchange (parse-float timestamp :type 'double-float))))

;;;
;;; Assets
;;;

(defclass asset ()
  ((name     :initarg :name     :reader name)
   (decimals :initarg :decimals :reader decimals)
   (exchange :initarg :exchange :reader exchange)))

(defmethod print-object ((asset asset) stream)
  (print-unreadable-object (asset stream :type nil :identity nil)
    (format stream "~A on ~A" (name asset) (exchange asset))))

(defgeneric find-asset (designator exchange)
  (:method (designator (assets list))
    (find designator assets :key 'name :test 'string-equal))
  (:method (designator (exchange exchange))
    (with-slots (assets) exchange
      (find-market designator assets))))

;;;
;;; Markets
;;;

(defclass market ()
  ((name     :initarg :name     :reader name)
   (decimals :initarg :decimals :reader decimals)
   (exchange :initarg :exchange :reader exchange)
   (quote    :initarg :quote    :reader quote-asset)
   (base     :initarg :base     :reader base-asset)))

(defmethod print-object ((market market) stream)
  (print-unreadable-object (market stream :type nil :identity nil)
    (format stream "~A on ~A" (name market) (exchange market))))

(defgeneric find-market (designator exchange)
  (:method (designator (markets list))
    (find designator markets :key 'name :test 'string-equal))
  (:method (designator (exchange exchange))
    (with-slots (markets) exchange
      (find-market designator markets))))

;;;
;;; Offers
;;;

(defclass offer ()
  ((market :initarg :market :reader market)
   (volume :initarg :volume :accessor volume)
   (price  :initarg :price  :reader price)))

(defclass placed (offer)
  ((uid    :initarg :uid    :reader uid)))

(defclass bid (offer) ())
(defclass ask (offer) ())
(defmethod initialize-instance :after ((bid bid) &key)
  (with-slots (price) bid (setf price (- price))))

(defmethod print-object ((offer offer) stream)
  (print-unreadable-object (offer stream :type t)
    (with-slots (volume price market) offer
      (with-slots (name decimals) (consumed-asset offer)
        (let ((market-decimals (slot-value market 'decimals)))
          (format stream "~V$ ~A @ ~V$" decimals volume name market-decimals
                  (/ (abs price) (expt 10 market-decimals))))))))

(defgeneric consumed-asset (offer)
  (:method ((bid bid)) (slot-value (slot-value bid 'market) 'quote))
  (:method ((ask ask)) (slot-value (slot-value ask 'market) 'base))
  (:method ((offer offer))
    (with-slots (market price) offer
      (slot-value market (if (> price 0) 'base 'quote)))))

;; (defmethod update-instance-for-different-class :after ((offer offer) (placed placed) &key)
;;   (format t "~&@~A ~A" (format-timestring nil (now) :format '((:sec 2))) placed))

;;;
;;; Rate Gate
;;;

(defclass gate ()
  ((pubkey :initarg :pubkey :initform (error "gate requires API pubkey"))
   (secret :initarg :secret :initform (error "gate requires API secret"))
   (in     :initarg :in     :initform (make-instance 'channel))
   (thread :initarg :thread)))

(defgeneric gate-post (gate pubkey secret request))

(defun gate-loop (gate)
  (with-slots (pubkey secret in) gate
    (destructuring-bind (ret . request) (recv in)
      (send ret (gate-post gate pubkey secret request)))))

(defmethod shared-initialize :after ((gate gate) names &key)
  (with-slots (thread) gate
    (when (or (not (slot-boundp gate 'thread))
              (eq :terminated (task-status thread)))
      (setf thread (pexec (:name "qdm-preα gate") (loop (gate-loop gate)))))))

(defun gate-request (gate path &optional options)
  (let ((out (make-instance 'channel)))
    (send (slot-value gate 'in) (list* out path options))
    (values-list (recv out))))

;;;
;;; Public Data API
;;;

;;; should trade direction be represented as:
;;; a boolean slot?
;;; slots for (consumed|earned)×(volume|asset)?
;;; distinct buy and sell mixin classes/objects?

(defclass trade ()
  ((market    :initarg :market    :reader market)
   (volume    :initarg :volume    :reader volume)
   (price     :initarg :price     :reader price)
   (cost      :initarg :cost      :reader cost)
   (timestamp :initarg :timestamp :reader timestamp)
   (direction :initarg :direction :reader direction)))

(defmethod print-object ((trade trade) stream)
  (print-unreadable-object (trade stream :identity nil)
    (with-slots (market volume price timestamp direction) trade
      (with-slots (base decimals) market
        (format stream "~A ~A at ~V$: ~V$" timestamp
                direction decimals price (decimals base) volume)))))

(defgeneric trades-since (market &optional since))

(defclass trades-tracker ()
  ((market  :initarg :market :reader market)
   (delay   :initarg :delay :initform 27)
   (control :initform (make-instance 'channel))
   (buffer  :initform (make-instance 'channel))
   (output  :initform (make-instance 'channel))
   (trades  :initform nil)
   updater worker))

(defgeneric vwap (tracker &key since type depth &allow-other-keys)
  (:method ((tracker trades-tracker) &key since depth type)
    (let ((trades (slot-value tracker 'trades)))
      (when since (setf trades (remove since trades
                                       :key #'timestamp :test #'timestamp>=)))
      (when type
        (setf trades (remove (ccase type (:buy #\b) (:sell #\s)) trades
                             :key (lambda (trade) (char (direction trade) 0))
                             :test-not #'char=)))
      (when depth
        (setf trades (loop for trade in trades collect trade
                           sum (volume trade) into sum
                           until (>= sum depth))))
      (handler-case
          (/ (reduce #'+ (mapcar #'cost trades))
             (reduce #'+ (mapcar #'volume trades)))
        (division-by-zero () 0)))))

;;; look at us, thinking of the future already
;;; we also dispatch on trades-tracker so that exchange-specific methods can
;;; dispatch on just the tracker argument, rather than both prev and next
;;; face it, sometimes you just gotta (eval '(expt +evil+ 1/2))

(defgeneric trades-mergeable? (trades-tracker prev next)
  (:method-combination and)
  (:method and (tracker (prev null) next))
  (:method and ((tracker trades-tracker) (prev trade) (next trade))
    (with-slots (market-timestamp-sensitivity)
        (slot-reduce tracker market exchange) ; !
      (and (string= (direction prev) (direction next))
           (> market-timestamp-sensitivity
              (timestamp-difference (timestamp next) (timestamp prev)))))))

;; (defgeneric same-trades? (trades-tracker prev next)
;;   (:method-combination and)
;;   (:method and ()))

(defgeneric merge-trades (trades-tracker prev next)
  (:method ((tracker trades-tracker) (prev trade) (next trade))
    (let* ((volume (+ (volume prev) (volume next)))
           (cost   (+ (cost   prev) (cost   next)))
           (price (/ cost volume)))
      (make-instance 'trade :market (market prev)
                     :timestamp (timestamp next) :cost cost
                     :volume volume :price price
                     :direction (direction prev)))))

(defmethod timestamp ((object null)))

(defun trades-worker-loop (tracker)
  (with-slots (control buffer output trades market) tracker
    (let ((last (car trades)))
      (select
        ((recv control command)
         ;; commands are (cons command args)
         (case (car command)
           ;; should this really be a 'command' and not just an operation
           ;; calculated by another actor on (slot-value tracker 'trades)?
           ;; max - find max seen trade size
           (max (send output (reduce #'max (mapcar #'volume trades)
                                     :initial-value 0)))
           ;; set symbol value
           (set (setf (symbol-value (second command)) (third command)))
           ;; get symbol value
           (get (send (third command) (symbol-value (second command))))
           ;; pause - wait for any other command to restart
           (pause (recv control))))
        ((send buffer last))
        ((recv buffer next)
         (if (trades-mergeable? tracker last next)
             (setf (car trades) (merge-trades tracker last next))
             (push next trades)))
        (t (sleep 0.2))))))

(defun trades-updater-loop (tracker)
  (with-slots (market buffer delay) tracker
    (dolist (trade (trades-since market (recv buffer))) (send buffer trade))
    (sleep delay)))

(defmethod shared-initialize :after ((tracker trades-tracker) (slots t) &key)
  (with-slots (updater worker market) tracker
    (when (or (not (slot-boundp tracker 'updater))
              (eq :terminated (task-status updater)))
      (setf updater
            (pexec
                (:name (concatenate 'string "qdm-preα trades updater for " (name market))
                 :initial-bindings `((*read-default-float-format* double-float)))
              (loop (trades-updater-loop tracker)))))
    (when (or (not (slot-boundp tracker 'worker))
              (eq :terminated (task-status worker)))
      (setf worker
            (pexec (:name (concatenate 'string "qdm-preα trades worker for " (name market))
                          :initial-bindings `((*read-default-float-format* double-float)))
              ;; TODO: just pexec anew each time...
              ;; you'll understand what you meant someday, right?
              (loop (trades-worker-loop tracker)))))))

;;;
;;; ORDER BOOK
;;;

(defclass book-tracker ()
  ((market :initarg :market)
   (control :initform (make-instance 'channel))
   (output :initform (make-instance 'channel))
   (delay :initarg :delay :initform 8)
   bids asks updater worker))

(defun book-worker-loop (tracker)
  (with-slots (control bids asks output) tracker
    (handler-case
        (select
          ((recv control command)
           ;; commands are (cons command args)
           (case (car command)
             ;; pause - wait for any other command to restart
             (pause (recv control))))
          ((send output (cons bids asks)))
          (t (sleep 0.2)))
      (unbound-slot ()))))

(defgeneric get-book (market))

(defun book-updater-loop (tracker)
  (with-slots (bids asks delay market offers) tracker
    (setf (values asks bids) (get-book market))
    (sleep delay)))

(defmethod shared-initialize :after ((tracker book-tracker) (names t) &key)
  (with-slots (updater worker market) tracker
    (when (or (not (slot-boundp tracker 'updater))
              (eq :terminated (task-status updater)))
      (setf updater
            (pexec
                (:name (concatenate 'string "qdm-preα book updater for " (name market))
                       :initial-bindings `((*read-default-float-format* double-float)))
              (loop (book-updater-loop tracker)))))
    (when (or (not (slot-boundp tracker 'worker))
              (eq :terminated (task-status worker)))
      (setf worker
            (pexec (:name (concatenate 'string "qdm-preα book worker for " (name market)))
              ;; TODO: just pexec anew each time...
              ;; you'll understand what you meant someday, right?
              (loop (book-worker-loop tracker)))))))

;;;
;;; Private Data API
;;;

(defgeneric placed-offers (gate))
(defgeneric market-fee (gate market))

;;;
;;; EXECUTION TRACKING
;;;

(defclass execution (trade)
  ((uid :initarg :uid :reader uid)
   (fee :initarg :fee :reader fee)
   (net-cost :initarg :net :reader net-cost)))

(defclass execution-tracker ()
  ((gate :initarg :gate)
   ;; TODO: is this the right model for a general execution tracking API?
   ;; bitfinex requires tracking distinct markets, kraken doesn't
   ;; keep your eyes open, for now we'll specify a single market per tracker
   (market :initarg :market)
   (delay :initform 30 :initarg :delay)
   (trades :initform nil)
   (control :initform (make-instance 'channel))
   (buffer :initform (make-instance 'channel))
   worker updater))

(defgeneric execution-since (gate market since))

(defun execution-worker-loop (tracker)
  (with-slots (control buffer trades) tracker
    (let ((last (car trades)))
      (select
        ((recv control command)
         ;; commands are (cons command args)
         (case (car command)
           ;; pause - wait for any other command to restart
           (pause (recv control))
           ;; set symbol value
           (set (setf (symbol-value (second command)) (third command)))
           ;; get symbol value
           (get (send (third command) (symbol-value (second command))))))
        ((send buffer last))
        ((recv buffer next) (push next trades))
        (t (sleep 0.2))))))

(defun execution-updater-loop (tracker)
  (with-slots (gate market buffer delay) tracker
    (dolist (trade (execution-since gate market (recv buffer)))
      (send buffer trade))
    (sleep delay)))

(defmethod shared-initialize :after ((tracker execution-tracker) (slots t) &key)
  (with-slots (updater worker market) tracker
    (when (or (not (slot-boundp tracker 'updater))
              (eq :terminated (task-status updater)))
      (setf updater
            (pexec
                (:name (concatenate 'string "qdm-preα execution updater for " (name market))
                 :initial-bindings `((*read-default-float-format* double-float)))
              (loop (execution-updater-loop tracker)))))
    (when (or (not (slot-boundp tracker 'worker))
              (eq :terminated (task-status worker)))
      (setf worker
            (pexec (:name (concatenate 'string "qdm-preα execution worker for " (name market))
                          :initial-bindings `((*read-default-float-format* double-float)))
              ;; TODO: just pexec anew each time...
              ;; you'll understand what you meant someday, right?
              (loop (execution-worker-loop tracker)))))))

;;; TODO: We have the fees paid for each order in the data from the exchange,
;;; so we should be able to calculate the _net_ price for each trade, and use
;;; that for profitability calculations, rather than fee at time of calculation.

(defmethod vwap ((tracker execution-tracker) &key type depth net)
  (let ((trades (remove type (slot-value tracker 'trades)
                        :key #'direction :test #'string/=)))
    (when depth
      (setf trades (loop for trade in trades collect trade
                      sum (volume trade) into sum
                      until (>= sum depth))))
    (/ (reduce '+ (mapcar (if net #'net-cost #'cost) trades))
       (reduce '+ (mapcar #'volume trades)))))

;;;
;;; Action API
;;;

(defgeneric post-offer (gate offer))
(defgeneric cancel-offer (gate offer))
