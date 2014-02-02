;;;; orders.lisp

(defpackage #:glock.orders
  (:use #:cl #:st-json #:glock.util #:glock.connection))

(in-package #:glock.orders)

(defclass orders (post-request)
  ()
  (:default-initargs :path "money/orders"))

(defmethod request :around ((connection mtgox-connection) (method orders))
  (let ((raw-orders (call-next-method)))
    (flet ((process-raw-order (order)
             (with-json-slots (priority oid amount price status) order
               (jso "oid" oid
                    "amount" (/ (parse-integer (getjso "value_int" amount)) (expt 10 8))
                    "price" (/ (parse-integer (getjso "value_int" price)) (expt 10 5))
                    "now" priority
                    "status" status))))
      (jso "bids" (mapcar #'process-raw-order
                          (remove "ask" raw-orders
                                  :test #'string=
                                  :key (lambda (o) (getjso "type" o))))
           "asks" (mapcar #'process-raw-order
                          (remove "bid" raw-orders
                                  :test #'string=
                                  :key (lambda (o) (getjso "type" o))))))))

(defclass cancel-order (post-request)
  ((id :initarg :id))
  (:default-initargs :path "money/order/cancel"))

(defmethod initialize-instance :around ((op cancel-order) &rest all &key id)
  (apply #'call-next-method op :data `(("oid" . ,id)) all))
