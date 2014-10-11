;;;; exchange.lisp

(defpackage #:scalpl.exchange
  (:use #:cl #:anaphora #:st-json #:local-time #:scalpl.util)
  (:export #:exchange #:assets #:markets
           #:asset #:find-asset #:name #:bids #:asks
           #:market #:find-market #:decimals #:base #:quote
           #:offer #:placed #:bid #:ask #:offer #:offer
           #:volume #:price #:uid #:consumed-asset #:trade
           #:parse-timestamp #:gate #:gate-post #:gate-request
           #:thread #:control #:updater #:worker #:output ; UGH
           #:get-book #:trades-since #:trades-tracker #:book-tracker
           #:placed-offers #:market-fee #:execution-history #:vwap
           #:post-offer #:cancel-offer
           ))

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
    (parse-timestamp exchange (read-from-string timestamp))))

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
   (in     :initarg :in     :initform (make-instance 'chanl:channel))
   (thread :initarg :thread)))

(defgeneric gate-post (gate pubkey secret request))

(defun gate-loop (gate)
  (with-slots (pubkey secret in) gate
    (destructuring-bind (ret . request) (chanl:recv in)
      (chanl:send ret (gate-post gate pubkey secret request)))))

(defmethod shared-initialize :after ((gate gate) names &key)
  (when (or (not (slot-boundp gate 'thread))
            (eq :terminated (chanl:task-status (slot-value gate 'thread))))
    (setf (slot-value gate 'thread)
          (chanl:pexec (:name "qdm-preα gate")
            (loop (gate-loop gate))))))

(defun gate-request (gate path &optional options)
  (let ((out (make-instance 'chanl:channel)))
    (chanl:send (slot-value gate 'in) (list* out path options))
    (values-list (chanl:recv out))))

;;;
;;; Public Data API
;;;

(defgeneric get-book (market))

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

(defgeneric trades-since (market &optional since))

(defclass trades-tracker ()
  ((market  :initarg :market :reader market)
   (delay   :initarg :delay :initform 27)
   (control :initform (make-instance 'chanl:channel))
   (buffer  :initform (make-instance 'chanl:channel))
   (output  :initform (make-instance 'chanl:channel))
   (trades  :initform nil)
   last updater worker))

(defgeneric vwap (tracker &key since type depth &allow-other-keys)
  (:method ((tracker trades-tracker) &key since depth type)
    (let ((trades (slot-value tracker 'trades)))
      (when since (setf trades (remove since trades
                                       :key #'timestamp :test #'timestamp>=)))
      (when type
        (setf trades (remove (ccase type (buy #\b) (sell #\s)) trades
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

(defgeneric trades-mergeable? (trades-tracker prev next)
  (:method-combination and)
  (:method and ((tracker trades-tracker) prev next)
    (with-slots (market-timestamp-sensitivity)
        (slot-reduce tracker market exchange)
      (> market-timestamp-sensitivity
         (timestamp-difference (timestamp next) (timestamp prev))))))

(defgeneric merge-trades (trades-tracker prev next)
  (:method ((tracker trades-tracker) prev next)
    (let* ((volume (+ (volume prev) (volume next)))
           (cost   (+ (cost   prev) (cost   next)))
           (price (/ cost volume)))
      (make-instance 'trade :market (market prev)
                     :timestamp (timestamp prev) :cost cost
                     :volume volume :price price))))

(defun trades-worker-loop (tracker)
  (with-slots (control buffer output trades market) tracker
    (chanl:select
      ((chanl:recv control command)
       ;; commands are (cons command args)
       (case (car command)
         ;; max - find max seen trade size
         (max (chanl:send output (reduce #'max (mapcar #'volume trades)
                                         :initial-value 0)))
         ;; vwap - find vwap over recent trades
         (vwap (chanl:send output (apply #'vwap tracker (cdr command))))
         ;; pause - wait for any other command to restart
         (pause (chanl:recv control))))
      ((chanl:recv buffer raw-trades)
       (unless trades (push (pop raw-trades) trades))
       (setf trades
             (reduce (lambda (acc next &aux (prev (first acc)))
                       (if (trades-mergeable? tracker prev next)
                           (cons (merge-trades tracker prev next) (cdr acc))
                           (cons next acc)))
                     raw-trades :initial-value trades)))
      (t (sleep 0.2)))))

(defun trades-updater-loop (tracker)
  (with-slots (market buffer delay last) tracker
    (multiple-value-bind (trades until)
        (handler-case (trades-since market last)
          (unbound-slot () (trades-since market)))
      (when trades (setf last until) (chanl:send buffer trades)))
    (sleep delay)))

(defmethod shared-initialize :after ((tracker trades-tracker) (slots t) &key)
  (with-slots (updater worker market) tracker
    (when (or (not (slot-boundp tracker 'updater))
              (eq :terminated (chanl:task-status updater)))
      (setf updater
            (chanl:pexec
                (:name (concatenate 'string "qdm-preα trades updater for " (name market))
                       :initial-bindings `((*read-default-float-format* double-float)))
              (loop (trades-updater-loop tracker)))))
    (when (or (not (slot-boundp tracker 'worker))
              (eq :terminated (chanl:task-status worker)))
      (setf worker
            (chanl:pexec (:name (concatenate 'string "qdm-preα trades worker for " (name market)))
              ;; TODO: just pexec anew each time...
              ;; you'll understand what you meant someday, right?
              (loop (trades-worker-loop tracker)))))))

;;;
;;; ORDER BOOK
;;;

(defclass book-tracker ()
  ((market :initarg :market)
   (control :initform (make-instance 'chanl:channel))
   (output :initform (make-instance 'chanl:channel))
   (delay :initarg :delay :initform 8)
   bids asks updater worker))

(defun book-worker-loop (tracker)
  (with-slots (control bids asks output) tracker
    (handler-case
        (chanl:select
          ((chanl:recv control command)
           ;; commands are (cons command args)
           (case (car command)
             ;; pause - wait for any other command to restart
             (pause (chanl:recv control))))
          ((chanl:send output (cons bids asks)))
          (t (sleep 0.2)))
      (unbound-slot ()))))

(defun book-updater-loop (tracker)
  (with-slots (bids asks delay market offers) tracker
    (setf (values asks bids) (get-book market))
    (sleep delay)))

(defmethod shared-initialize :after ((tracker book-tracker) (names t) &key)
  (with-slots (updater worker market) tracker
    (when (or (not (slot-boundp tracker 'updater))
              (eq :terminated (chanl:task-status updater)))
      (setf updater
            (chanl:pexec
                (:name (concatenate 'string "qdm-preα book updater for " (name market))
                       :initial-bindings `((*read-default-float-format* double-float)))
              (loop (book-updater-loop tracker)))))
    (when (or (not (slot-boundp tracker 'worker))
              (eq :terminated (chanl:task-status worker)))
      (setf worker
            (chanl:pexec (:name (concatenate 'string "qdm-preα book worker for " (name market)))
              ;; TODO: just pexec anew each time...
              ;; you'll understand what you meant someday, right?
              (loop (book-worker-loop tracker)))))))

;;;
;;; Private Data API
;;;

(defgeneric placed-offers (gate))
(defgeneric market-fee (gate market))
(defgeneric execution-history (gate &key))

;;;
;;; Action API
;;;

(defgeneric post-offer (gate offer))
(defgeneric cancel-offer (gate offer))
