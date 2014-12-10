(cl:defpackage #:scalpl.asd
  (:use #:cl #:asdf #:asdf/run-program #:asdf/component))

(cl:in-package #:scalpl.asd)

(defsystem #:scalpl
  :serial t
  :license "public domain"
  :description "Bitcoin exchange API & market maker"
  :author "Adlai Chandrasekhar <munchking@gmail.com>"
  :depends-on (#:anaphora
               #:string-case
               #:parse-float
               #:drakma
               #:st-json
               #:cl-base64
               #:ironclad
               #:local-time
               #:closer-mop
               #:rucksack
               #:chanl #:chanl.examples
               #:external-program)
  :serial t
  :components ((:file "util")
               (:file "exchange")
               (:file "kraken")
               (:file "bitfinex")
               (:file "qd")
               ))
