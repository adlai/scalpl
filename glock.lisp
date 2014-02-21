;;;; glock.lisp

(defpackage #:glock
  (:use #:cl #:glock.util #:glock.connection #:glock.orders #:glock.book)
  (:export
   ;; from glock.connection
   #:mtgox-api-error
   #:mtgox-connection
   #:request

   ;; from here
   #:ticker
   ))

;;; For your REPL convenience
(defpackage #:glock-user
  (:use #:cl #:st-json #:glock #:glock.util #:glock.book))

(in-package #:glock)

;;; Ticker request
(defclass ticker (get-request) ()
  (:default-initargs :fast T))

(defmethod initialize-instance :around ((ticker ticker) &key fast)
  (call-next-method ticker :path (format nil "money/ticker~@[_fast~]" fast)))

;;; Order Manipulation
(defclass orders (post-request) ()
  (:default-initargs :path "money/orders"))
