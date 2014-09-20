;;;; exchange.lisp

(defpackage #:scalpl.exchange
  (:use #:cl #:anaphora #:st-json #:local-time #:scalpl.util)
  (:export #:exchange #:assets #:markets
           #:asset #:find-asset #:name-of
           #:market #:find-market #:decimals #:base #:quote
           #:offer #:placed #:bid #:ask #:offer-price #:offer-volume
           #:volume #:price #:offer-id #:offer-text #:consumed-asset
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
  (print-unreadable-object (offer stream :type t :identity t)
    (with-slots (volume price) offer
      (format stream "~A @ ~A" volume price))))

(defgeneric consumed-asset (offer)
  (:method ((bid bid)) (slot-value (slot-value bid 'market) 'quote))
  (:method ((ask ask)) (slot-value (slot-value ask 'market) 'base))
  (:method ((offer offer))
    (with-slots (market price) offer
      (slot-value market (if (> price 0) 'base 'quote)))))
