(cl:defpackage #:scalpl.asd (:use #:cl #:asdf))

(cl:in-package #:scalpl.asd)

(defsystem #:scalpl
  :license "public domain"
  :description "Bitcoin exchange API & market maker"
  :author "Adlai Chandrasekhar <adlai.chandrasekhar@gmail.com>"
  :depends-on (#:anaphora #:string-case #:parse-float #:drakma
               #:method-combination-utilities #:dbi #:cl-json
               #:cl-base64 #:ironclad #:local-time #:chanl)
  :components ((:file "util")
               (:file "exchange" :depends-on ("util"))
               (:file "kraken"   :depends-on ("exchange")) ; TODO:      :∃MXIꟻ
               (:file "bitfinex" :depends-on ("exchange")) ; factor these into
               (:file "btce"     :depends-on ("exchange")) ; sub-systems for
               (:file "qd"       :depends-on ("exchange")) ; modularitizations
               (:file "db"       :depends-on ("exchange"))))

(defsystem #:scalpl.mpex
  :license "all rights reserved"
  :description "scalpl mpex api"
  :author "Adlai Chandrasekhar <adlai.chandrasekhar@gmail.com>"
  :depends-on (#:scalpl #:split-sequence #:rss)
  :components ((:file "mpex")))
