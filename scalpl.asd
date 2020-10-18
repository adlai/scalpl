(cl:defpackage #:scalpl.asd (:use #:cl #:asdf))

(cl:in-package #:scalpl.asd)

(defsystem #:scalpl
  :license "public domain"
  :description "market maker + APIs to several Bitcoin exchanges"
  :author "Adlai Chandrasekhar <adlai.chandrasekhar@gmail.com>"
  :depends-on (#:anaphora #:string-case #:parse-float #:decimals #:ironclad
               #:chanl #:cl-json #:cl-base64 #:split-sequence #:local-time
               #:method-combination-utilities #:drakma)
  :components ((:file "util")
               (:file "actor"    :depends-on ("util"))
               (:file "exchange" :depends-on ("actor"))
               (:file "qd"       :depends-on ("exchange"))))

(defsystem #:scalpl/bybit
  :license "public domain"
  :description "api client for bybit"
  :author "Adlai Chandrasekhar <adlai.chandrasekhar@gmail.com"
  :depends-on (#:scalpl #:websocket-driver-client)
  :components ((:file "bybit"))
  :perform (load-op (no book) (cerror "bucketp" 'parse-error)))
(defsystem #:scalpl/bitmex
  :license "public domain"
  :description "api client for bitmex"
  :author "Adlai Chandrasekhar <adlai.chandrasekhar@gmail.com>"
  :depends-on (#:scalpl #:websocket-driver-client)
  :components ((:file "bitmex")))
(defsystem #:scalpl/poloniex
  :license "public domain"
  :description "api client for poloniex"
  :author "Adlai Chandrasekhar <adlai.chandrasekhar@gmail.com>"
  :depends-on (#:scalpl)
  :components ((:file "poloniex")))
(defsystem #:scalpl/mpex
  :license "public domain"
  :description "api client for mpex"
  :author "Adlai Chandrasekhar <adlai.chandrasekhar@gmail.com>"
  :depends-on (#:scalpl #:rss)
  :components ((:file "mpex")))
(defsystem #:scalpl/kraken
  :license "public domain"
  :description "api client for kraken"
  :author "Adlai Chandrasekhar <adlai.chandrasekhar@gmail.com>"
  :depends-on (#:scalpl)
  :components ((:file "kraken")))
(defsystem #:scalpl/bitfinex
  :license "public domain"
  :description "api client for bitfinex"
  :author "Adlai Chandrasekhar <adlai.chandrasekhar@gmail.com>"
  :depends-on (#:scalpl)
  :components ((:file "bitfinex")))

(defsystem #:scalpl/dbi
  :license "public domain"
  :description "database interactions"
  :author "Adlai Chandrasekhar <adlai.chandrasekhar@gmail.com>"
  :depends-on (#:scalpl #:dbi)
  :components ((:file "db")))
(defsystem #:scalpl/irc
  :license "public domain"
  :description "sasl authentication module"
  :author "Adlai Chandrasekhar <adlai.chandrasekhar@gmail.com>"
  :depends-on (#:scalpl #:cl-irc)
  :components ((:file "sasl")))
