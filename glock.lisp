;;;; glock.lisp

(defpackage #:glock
  (:use #:cl #:glock.connection #:glock.orders)
  (:export
   ;; from glock.connection
   #:mtgox-api-error
   #:mtgox-connection
   #:request

   ;; from here
   #:ticker
   ))

;;; For your REPL convenience
(defpackage #:glock-user
  (:use #:cl #:glock))

(in-package #:glock)

;; ;;; First frobs
;; (defun ticker (&key (pair "BTCUSD") fast)
;;   (let ((uri (concatenate 'string +base-path+ pair
;;                           (if fast
;;                               "/money/ticker_fast"
;;                               "/money/ticker"))))
;;     (json:decode-json (drakma:http-request uri :want-stream t))))

(defclass ticker (get-request) ()
  (:default-initargs :fast T))

(defmethod initialize-instance :around ((ticker ticker) &key fast)
  (call-next-method ticker :path (format nil "money/ticker~@[_fast~]" fast)))
