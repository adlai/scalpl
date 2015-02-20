(cl:defpackage #:scalpl.asd
  (:use #:cl #:asdf #:asdf/run-program #:asdf/component))

(cl:in-package #:scalpl.asd)

(defsystem #:scalpl
  :license "public domain"
  :description "Bitcoin exchange API & market maker"
  :author "Adlai Chandrasekhar <adlai.chandrasekhar@gmail.com>"
  :depends-on (#:anaphora #:string-case #:parse-float #:drakma
               #:cl-base64 #:ironclad #:local-time #:chanl #:dbi #:cl-json)
  :components ((:file "util")
               (:file "actor"    :depends-on ("util"))
               (:file "exchange" :depends-on ("actor"))
               (:file "kraken"   :depends-on ("exchange"))
               (:file "bitfinex" :depends-on ("exchange"))
               (:file "btce"     :depends-on ("exchange"))
               (:file "qd"       :depends-on ("exchange"))
               (:file "db"       :depends-on ("exchange"))))
