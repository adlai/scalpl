(cl:defpackage #:scalpl.asd (:use #:cl #:asdf))

(cl:in-package #:scalpl.asd)

(defsystem #:scalpl
  :license "public domain"
  :description "market maker + APIs to several Bitcoin exchanges"
  :author "Adlai Chandrasekhar" ; and the shepherds of the public domain
  ;; if you add Alexandria to the dependency list; HUNT YOHU DOWN KILL
  :depends-on (#:anaphora #:string-case #:parse-float #:decimals
               #:ironclad #:chanl #:cl-json #:cl-base64
               #:split-sequence #:local-time #:websocket-driver-client
               #:method-combination-utilities #:drakma)
  :serial t :components ((:file "util")
                         (:file "actor")
                         (:file "net")
                         (:file "exchange")
;;                  (:module "bitcoin" :depends-on ("config") :serial t
;;                   :components ((:file "rpc-client") (:file "api")
;;                                (:file "raw-parser")))
;; ;;                 (:file "hunt" :depends-on ("bitcoin"))
                         (:file "qd")   ; FIXME POSTHASTE SELLPHONE
                         (:file "navel")))

;;; Contact email for the author has been removed from this file!
;;; If you believe that software should treat email addresses as
;;; an inseparable part of a human being's full name... I'm not
;;; able to help you much in that regard, although perhaps you
;;; can direct your complaints at the good folks running the
;;; CLtL4 committee; for although I can't predict what
;;; will be standardized by CLtL3...


(defsystem #:scalpl/bit2c
  :description "api client for bit2c"
  :depends-on (#:scalpl)
  :components ((:file "bit2c"))
  :perform (load-op (no book)
             (declare (ignore no book)) ; SAY IT ALOUD
             (use-package (find-package :scalpl.bit2c)
                          (find-package :scalpl.qd)))
  :license "public domain")
(defsystem #:scalpl/binance
  :license "public domain"
  :description "WIP api client for binance"
  :depends-on (#:scalpl)        ; let's pretend this improved anything!
  :components ((:file "binance"))
  :perform (load-op (no book)
             (cerror "bucketp" 'parse-error) ;D   ``PROBLEM!?,, - d:
             (format *debug-io* "~&~A~%"
                     (if (y-or-n-p "USE-PACKAGE[(SCALPL.BINANCE),SCALPL.QD]?")
                         (use-package (find-package :scalpl.binance)
                                      (find-package :scalpl.qd))
                         "Relax, it's only an M-expression!"))))
(defsystem #:scalpl/bybit
  :license "public domain"
  :description "api client for bybit"
  :depends-on (#:scalpl #:websocket-driver-client)
  :components ((:file "bybit"))
  :perform (load-op (no book)
             (cerror "bucketp" 'parse-error) ;D   ``PROBLEM!?,, - d:
             (format *debug-io* "~&~A~%"
                     (if (y-or-n-p "USE-PACKAGE[(SCALPL.BYBIT),SCALPL.QD]?")
                         (use-package (find-package :scalpl.bybit)
                                      (find-package :scalpl.qd))
                         "Relax, it's only an M-expression!"))))
(defsystem #:scalpl/kraken
  :license "public domain"
  :description "api client for kraken"
  :depends-on (#:scalpl)
  :components ((:file "kraken"))
  :perform (load-op (no book)
             (cerror "bucketp" 'parse-error) ;D   ``PROBLEM!?,, - d:
             (format *debug-io* "~&~A~%"
                     (if (y-or-n-p "USE-PACKAGE[(SCALPL.kraken),SCALPL.QD]?")
                         (use-package (find-package :scalpl.kraken)
                                      (find-package :scalpl.qd))
                         "Relax, it's only an M-expression!"))))
(defsystem #:scalpl/bitmex
  :license "public domain"
  :description "api client for bitmex"
  :depends-on (#:scalpl #:websocket-driver-client)
  :components ((:file "bitmex"))
  :perform (load-op (no book)
             (cerror "bucketp" 'parse-error) ;D   ``PROBLEM!?,, - d:
             (format *debug-io* "~&~A~%"
                     (if (y-or-n-p "USE-PACKAGE[(SCALPL.BITMEX),SCALPL.QD]?")
                         (use-package (find-package :scalpl.bitmex)
                                      (find-package :scalpl.qd))
                         "Relax, it's only an M-expression!"))))
(defsystem #:scalpl/bitfinex
  :license "public domain"
  :description "api client for bitfinex"
  :depends-on (#:scalpl)
  :components ((:file "bitfinex"))
  :perform (load-op (no book)
             (cerror "bucketp" 'parse-error) ;D   ``PROBLEM!?,, - d:
             (format *debug-io* "~&~A~%"
                     (if (y-or-n-p "USE-PACKAGE[(SCALPL.BITFINEX),SCALPL.QD]?")
                         (use-package (find-package :scalpl.bitfinex)
                                      (find-package :scalpl.qd))
                         "Relax, it's only an M-expression!"))))
(defsystem #:scalpl/poloniex
  :license "public domain"
  :description "api client for poloniex"
  :depends-on (#:scalpl)
  :components ((:file "poloniex"))
  :perform (load-op (no book)
             (cerror "bucketp" 'parse-error) ;D   ``PROBLEM!?,, - d:
             (format *debug-io* "~&~A~%"
                     (if (y-or-n-p "USE-PACKAGE[(SCALPL.POLONIEX),SCALPL.QD]?")
                         (use-package (find-package :scalpl.poloniex)
                                      (find-package :scalpl.qd))
                         "Relax, it's only an M-expression!"))))

;;; The above were ordered by date of last modification,
;;; and the ordering should not be taken as any sort of
;;; moral judgement about the operators and their clients.

;;; The following are not quite incomplete,
;;; for various interpretations of the not.

(defsystem #:scalpl/dbi
  :license "public domain"
  :description "database interactions"
  :depends-on (#:scalpl #:dbi #:mito)
  :components ((:file "db")))
(defsystem #:scalpl/irc
  :license "public domain"
  :description "sasl authentication module"
  :depends-on (#:scalpl #:cl-irc)
  :components ((:file "sasl")))
