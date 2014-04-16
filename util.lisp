;;;; utils.lisp

(defpackage #:glock.util
  (:use #:cl #:st-json)
  (:export #:mapcar-slots
           #:bound-slot-names
           #:as-alist
           #:currency-pair
           #:with-json-slots
           #:goxstamp
           #:jso-keys
           ))

(in-package #:glock.util)

;;; Exploratory nonsense

(defun mapcar-slots (function object)
  (st-json:mapjso function object))

(defun bound-slot-names (object)
  (mapcar-slots (lambda (name value) (declare (ignore value)) name) object))

(defun as-alist (object)
  (mapcar-slots #'cons object))

;;; Actually useful

(defun goxstamp (stamp)
  (multiple-value-bind (sec nsec) (floor stamp 1000000)
    (local-time:unix-to-timestamp sec :nsec nsec)))

(defun currency-pair (c1 c2)
  "c1 and c2 should be three-letter keywords such as :btc or :usd"
  (format nil "~A~A" c1 c2))

(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym (symbol-name n)))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
      `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
        ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
           ,@body)))))

(defmacro with-json-slots ((&rest slot-bindings) object &body body)
  (once-only (object)
    `(let ,(mapcar (lambda (binding)
                     (if (consp binding)
                         (destructuring-bind (var slot) binding
                           `(,var (getjso ,slot ,object)))
                         `(,binding (getjso ,(string-downcase (symbol-name binding)) ,object))))
                   slot-bindings)
       ,@body)))

(defun jso-keys (jso &aux keys)
  (mapjso (lambda (key val)
            (declare (ignore val))
            (push key keys))
          jso)
  keys)
