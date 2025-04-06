(mapc 'ql:quickload '("swank" "scalpl/kraken"))

(in-package :scalpl.qd) (use-package :scalpl.kraken)

(swank:create-server :dont-close t :port 54321)

(when (y-or-n-p ";; #?(ENABLE-PRETTY-PRINTER-ABUSE)")
  (enable-pretty-printer-abuse))

(destructuring-bind (pubkey secret)
    (directory "secrets/accounts/kraken.*")
  (defvar *gate* (make-instance 'kraken-gate :pubkey pubkey :secret secret)))

(defparameter *supplicant*
  (make-instance 'supplicant :order-slots 13 :gate *gate*
		             :market (find-market "XBTUSD" :kraken)))

(define-maker *wbtcr* :supplicant *supplicant* :targeting 1)

;;; use amend orders when possible to improve API efficiency
(change-class (slot-reduce *wbtcr* ope prioritizer)
              'kraken::kraken-prioritizer)
#/bin/it ;-- must be someone's work
