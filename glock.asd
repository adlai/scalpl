;;;; glock.asd

(cl:defpackage #:glock.asd
  (:use #:cl #:asdf #:asdf/run-program #:asdf/component))

(cl:in-package #:glock.asd)

(defclass package.json (static-file)
  ())

(defmethod perform ((op compile-op) (component package.json))
  (with-slots ((path absolute-pathname)) (slot-value component 'parent)
    (run-program (format nil "cd ~a && npm install" path))))

(defsystem #:glock
  :serial t
  :description "MtGox API"
  :author "Adlai Chandrasekhar <munchking@gmail.com>"
  :license "CC0"
  :depends-on (#:drakma
               #:st-json
               #:cl-base64
               #:ironclad
               #:local-time
               #:chanl
               #:external-program)
  :serial t
  :components ((:file "util")
               (:file "connection")
               (:file "orders")
               (:file "calc")
               (:package.json "package.json")
               (:file "book")
               (:file "glock")))
