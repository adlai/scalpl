;;;; glock.asd

(cl:defpackage #:glock.asd
  (:use #:cl #:asdf #:asdf/run-program #:asdf/component))

(cl:in-package #:glock.asd)

(defclass package.json (static-file) ())

;; (defmethod component-relative-pathname ((component package.json)) "package.json")

(defmethod perform ((op compile-op) (component package.json))
  (with-slots ((path absolute-pathname)) (slot-value component 'parent)
    (run-program (format nil "cd ~a && npm install" path))))

(defsystem #:glock
  :serial t
  :description "MtGox API"
  :author "Adlai Chandrasekhar <munchking@gmail.com>"
  :license "CC0"
  :depends-on (#:anaphora
               #:drakma
               #:st-json
               #:cl-base64
               #:ironclad
               #:local-time
               #:chanl
               #:external-program)
  :serial t
  :components ((:file "util")
               (:file "connection")
               (:file "qd")
               ;; (:file "orders")
               ;; (:file "calc")
               ;; (:package.json "package.json") ; why is ASDF so insistant
               ;; (:file "book")
               ;; (:file "glock")
               ))
