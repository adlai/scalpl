;;;; utils.lisp

(in-package #:glock)

;;; Exploratory nonsense

(import 'json::map-slots)

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
