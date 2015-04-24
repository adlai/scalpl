(defpackage #:scalpl.poloniex
  (:nicknames #:poloniex)
  (:export #:*poloniex* #:poloniex-gate)
  (:use #:cl #:chanl #:anaphora #:local-time #:scalpl.util #:scalpl.exchange))

(in-package #:scalpl.poloniex)

;;; General Parameters
(defparameter +base-path+ "https://poloniex.com/")
(defparameter +public-stub+ "public?command=")
(defparameter +private-stub+ "tradingApi")
