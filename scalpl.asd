(cl:defpackage #:scalpl.asd (:use #:cl #:asdf))

(cl:in-package #:scalpl.asd)

(defsystem #:scalpl
  :license "public domain"
  :description "market maker + APIs to several Bitcoin exchanges"
  :author "Adlai Chandrasekhar <adlai.chandrasekhar@gmail.com>"
  :maintainer "you, you blithering idiot <patches@www.el.com>"
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
  :components ((:file "util")
               (:file "actor"    :depends-on ("util"))
               (:file "exchange" :depends-on ("actor"))
               (:file "qd"       :depends-on ("exchange"))

               ;; A poor apprentice once asked:
               ;;   "Signor Michaelangelo, how do we cope
               ;;    with the blasphemy in the workshop?"
               ;;
               ;; Without checking outside his Google Plus
               ;;   Circles, Archimedes answered:
               ;;     "First, mute everybody; then, unmute
               ;;      anything that sounds like a whale."

               ;; (:file "bitmex"   :depends-on ("exchange"))
               ;; (:file "poloniex" :depends-on ("exchange"))
               ;; (:file "mpex"     :depends-on ("exchange"))
               ;; (:file "kraken"   :depends-on ("exchange"))
               ;; (:file "bitfinex" :depends-on ("exchange"))
               ;; (:file "btce"     :depends-on ("exchange"))
               ;; (:file "db"       :depends-on ("exchange"))
               ;; (:file "sasl")
               ))
