;;;; exchange.lisp

(defpackage #:scalpl.exchange
  (:use #:cl #:anaphora #:st-json #:local-time #:scalpl.util)
  (:export #:exchange #:assets #:markets
           #:asset #:find-asset #:name-of
           #:market #:find-market #:decimals #:base #:quote
           #:offer #:placed #:bid #:ask #:offer-price #:offer-volume
           #:volume #:price #:offer-id #:offer-text #:consumed-asset
           #:parse-timestamp #:gate #:gate-post #:gate-request
           #:thread ; UGH
           #:get-book #:trades-since
           #:placed-offers #:market-fee
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
  ((name :initarg :name)
   (assets :initarg :assets)
   (markets :initarg :markets)))

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
  ((name :initarg :name :reader name-of)
   (decimals :initarg :decimals)))

(defgeneric find-asset (designator exchange)
  (:method (designator (assets list))
    (find designator assets :key 'name-of :test 'string-equal))
  (:method (designator (exchange exchange))
    (with-slots (assets) exchange
      (find-market designator assets))))

;;;
;;; Markets
;;;

(defclass market ()
  ((name :initarg :name :reader name-of)
   (decimals :initarg :decimals)
   (quote :initarg :quote :reader quote-asset)
   (base :initarg :base :reader base-asset)))

(defgeneric find-market (designator exchange)
  (:method (designator (markets list))
    (find designator markets :key 'name-of :test 'string-equal))
  (:method (designator (exchange exchange))
    (with-slots (markets) exchange
      (find-market designator markets))))

;;;
;;; Offers
;;;

(defclass offer ()
  ((market :initarg :market)
   (volume :initarg :volume :accessor offer-volume)
   (price :initarg :price :reader offer-price)))

(defclass placed (offer)
  ((id :initarg :id :reader offer-id)
   (text :initarg :text :reader offer-text)))

(defclass bid (offer) ())
(defclass ask (offer) ())
(defmethod initialize-instance :after ((bid bid) &key)
  (with-slots (price) bid (setf price (- price))))

(defmethod shared-initialize :after ((offer placed) names &key)
  (unless (slot-boundp offer 'text)
    (setf (slot-value offer 'text)
          (with-output-to-string (s) (describe offer s)))))

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
(defgeneric trades-since (market &optional since))

;;;
;;; Private Data API
;;;

(defgeneric placed-offers (gate))
(defgeneric market-fee (gate market))

;;;
;;; Action API
;;;

(defgeneric post-offer (gate offer))
(defgeneric cancel-offer (gate offer))
