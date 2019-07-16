(cl:defpackage #:scalpl.asd (:use #:cl #:asdf))

(cl:in-package #:scalpl.asd)

(defsystem #:scalpl
  :license "public domain"
  :description "market maker + APIs to several Bitcoin exchanges"
  :author "Adlai Chandrasekhar <adlai.chandrasekhar@gmail.com>"
  :depends-on (#:anaphora
               #:string-case
               #:parse-float
               #:cl-irc
               #:drakma
               #:method-combination-utilities
               #:local-time
               #:rss
               #:chanl
               #:split-sequence
               #:cl-base64
               #:ironclad
               #:cl-json
               #:dbi
               #:decimals)
  :components ((:file "util") (:file "sasl")
               (:file "actor"    :depends-on ("util"))
               (:file "exchange" :depends-on ("actor"))
               ;; (:file "bitmex"   :depends-on ("exchange"))
               ;; (:file "poloniex" :depends-on ("exchange"))
               ;; (:file "mpex"     :depends-on ("exchange"))
               ;; (:file "kraken"   :depends-on ("exchange"))
               ;; (:file "bitfinex" :depends-on ("exchange"))
               ;; (:file "btce"     :depends-on ("exchange"))
               (:file "qd"       :depends-on ("exchange"))
               (:file "db"       :depends-on ("exchange"))))
