;;;; utils.lisp

(defpackage #:glock.util
  (:use #:cl)
  (:import-from #:json #:map-slots)
  (:export #:mapcar-slots
           #:bound-slot-names
           #:as-alist
           #:currency-pair
           #:with-json-slots))

(in-package #:glock.util)

;;; Exploratory nonsense

(defun mapcar-slots (function object)
  (let (list)
    (map-slots (lambda (name value)
                 (push (funcall function name value) list))
               object)
    list))

(defun bound-slot-names (object)
  (mapcar-slots (lambda (name value) (declare (ignore value)) name) object))

(defun as-alist (object)
  (mapcar-slots #'cons object))

;;; Actually useful

(defun currency-pair (c1 c2)
  "c1 and c2 should be three-letter keywords such as :btc or :usd"
  (format nil "~A~A" c1 c2))

(defmacro with-json-slots (slot-vars object &body body)
  `(with-slots
         ,(mapcar (lambda (slot)
                    `(,slot ,(intern (symbol-name slot) :keyword)))
                  slot-vars)
       ,object
     ,@body))
