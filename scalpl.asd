(cl:defpackage #:scalpl.asd (:use #:cl #:asdf))

(cl:in-package #:scalpl.asd)

(defsystem #:scalpl
  :license "public domain"
  :description "market maker + APIs to several Bitcoin exchanges"
  :author "Adlai Chandrasekhar"
  :depends-on (#:anaphora #:string-case #:parse-float #:decimals #:ironclad
               #:chanl #:cl-json #:cl-base64 #:split-sequence #:local-time
               #:method-combination-utilities #:drakma)
  :serial t :components ((:file "util") (:file "actor")
                         (:file "exchange") (:file "qd")))

;;; Contact email for the author has been removed from this file!
;;; If you believe that software should treat email addresses as
;;; an inseparable part of a human being's full name... I'm not
;;; able to help you much in that regard, although perhaps you
;;; can direct your complaints at the good folks running the
;;; CLtL4 committee; for although I can't predict what
;;; will be standardized by CLtL3...


(defsystem #:scalpl/phemex
  :license "public domain"
  :description "api client for phemex"
  :depends-on (#:scalpl #:websocket-driver-client)
  :components ((:file "phemex"))
  :perform (load-op (no book) (cerror "bucketp" 'parse-error)))
(defsystem #:scalpl/bit2c
  :license "public domain"
  :description "api client for bit2c"
  :depends-on (#:scalpl #:websocket-driver-client)
  :components ((:file "bit2c")))
(defsystem #:scalpl/bybit
  :license "public domain"
  :description "api client for bybit"
  :depends-on (#:scalpl #:websocket-driver-client)
  :components ((:file "bybit"))
  :perform (load-op (no book)
             (cerror "bucketp" 'parse-error) ;D   ``PROBLEM!?,, - d:
             (format *debug-io* "~&~A~%"
                     (if (y-or-n-p "USE-PACKAGE[(SCALPL.BYBIT),SCALPL.QD]?")
                         "WHEN EVIL!?" "Relax, it's only an M-expression!"))))
(defsystem #:scalpl/bitmex
  :license "public domain"
  :description "api client for bitmex"
  :depends-on (#:scalpl #:websocket-driver-client)
  :components ((:file "bitmex")))
(defsystem #:scalpl/kraken
  :license "public domain"
  :description "api client for kraken"
  :depends-on (#:scalpl)
  :components ((:file "kraken")))
(defsystem #:scalpl/bitfinex
  :license "public domain"
  :description "api client for bitfinex"
  :depends-on (#:scalpl)
  :components ((:file "bitfinex")))
(defsystem #:scalpl/poloniex
  :license "public domain"
  :description "api client for poloniex"
  :depends-on (#:scalpl)
  :components ((:file "poloniex")))

;;; The above were ordered by date of last modification,
;;; and the ordering should not be taken as any sort of
;;; moral judgement about the operators and their clients.

;;; The following are not quite incomplete,
;;; for various interpretations of the not.

(defsystem #:scalpl/dbi
  :license "public domain"
  :description "database interactions"
  :depends-on (#:scalpl #:dbi)
  :components ((:file "db")))
(defsystem #:scalpl/irc
  :license "public domain"
  :description "sasl authentication module"
  :depends-on (#:scalpl #:cl-irc)
  :components ((:file "sasl")))
