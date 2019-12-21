(cl:defpackage #:scalpl.asd (:use #:cl #:asdf))

(cl:in-package #:scalpl.asd)

(defsystem #:scalpl
  :license "public domain"
  :description "market maker + APIs to several Bitcoin exchanges"
  :author "Adlai Chandrasekhar <adlai.chandrasekhar@gmail.com>"
  :depends-on (#:anaphora #:string-case #:parse-float #:decimals #:ironclad
               #:chanl #:cl-json #:cl-base64 #:split-sequence #:local-time
               #:method-combination-utilities #:drakma

               ;; these are only needed for certain exchanges
               #:aserve #:rss

               ;; these were used for experiments in the past
               ;; #:cl-irc #:dbi
               )
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
               ))

(defsystem #:scalpl.dbi
  :license "public domain"
  :description "database interaction"
  :author "Adlai Chandrasekhar <adlai.chandrasekhar@gmail.com>"
  :depends-on (#:scalpl #:dbi)
  :components ((:file "db")))

(defsystem #:scalpl.irc
  :license "public domain"
  :description "sasl authentication module"
  :author "Adlai Chandrasekhar <adlai.chandrasekhar@gmail.com>"
  :depends-on (#:scalpl #:cl-irc)
  :components ((:file "sasl")))
