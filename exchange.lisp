;;;; exchange.lisp

(defpackage #:scalpl.exchange
  (:use #:cl #:anaphora #:st-json #:local-time #:scalpl.util)
  (:export #:exchange #:assets #:markets
           #:asset #:find-asset #:name-of
           #:market #:find-market #:decimals #:base #:quote
           #:offer #:placed #:bid #:ask #:offer-price #:offer-volume
           #:volume #:price #:offer-id #:offer-text
           ))

(in-package #:scalpl.exchange)

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

(defgeneric asset-consumed-by (offer)
  (:method ((bid bid)) (slot-value (slot-value bid 'market) 'quote))
  (:method ((ask ask)) (slot-value (slot-value ask 'market) 'base))
  (:method ((offer offer))
    (with-slots (market price) offer
      (slot-value market (if (> price 0) 'base 'quote)))))
