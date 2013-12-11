;;;; glock.lisp

(in-package #:glock)

;;; General Parameters
(defparameter +base-path+ "https://data.mtgox.com/api/2/")

;;; First frobs
(defun ticker (&optional (pair "BTCUSD"))
  (let ((uri (concatenate 'string +base-path+ pair "/money/ticker_fast")))
    (json:decode-json (drakma:http-request uri :want-stream t))))
