(cl:defpackage #:scalpl.asd
  (:use #:cl #:asdf #:asdf/run-program #:asdf/component))

(cl:in-package #:scalpl.asd)

(defsystem #:scalpl
  :serial t
  :author "Adlai Chandrasekhar <munchking@gmail.com>"
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
               (:file "exchange")
               (:file "kraken")
               (:file "qd")
               ))
