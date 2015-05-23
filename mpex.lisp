(defpackage #:scalpl.mpex
  (:nicknames #:mpex)
  (:export #:*mpex* #:mpex-gate)
  (:use #:cl #:chanl #:anaphora #:local-time #:scalpl.util #:scalpl.exchange))

(in-package #:scalpl.mpex)

;;; General Parameters
(defparameter +base-path+ "http://mpex.co/")  ; TODO: auto-fallback to proxies
(defparameter +public-stub+ "mpex-")

(defun raw-request (path &rest keys)
  (multiple-value-bind (body status)
      (apply #'http-request (concatenate 'string +base-path+ path) keys)
    (if (= status 200) (read-json body)
        (values nil (format nil "HTTP Error ~D~%~A" status body)))))

(defun get-request (path)
  (raw-request (concatenate 'string +public-stub+ path ".php")))

(defvar *mpex* (make-instance 'exchange :name :mpex))

;; (defun post-request (method key signer &optional params &aux (nonce (nonce)))
;;   (push (cons "method" method) params)
;;   (push (cons "nonce" nonce) params)
;;   (let ((data (urlencode-params params)))
;;     (raw-request (concatenate 'string +private-stub+)
;;                  :method :post :content data
;;                  :additional-headers `(("Key"  . ,key)
;;                                        ("Sign" . ,(funcall signer data))))))

(defun get-info ()
  (flet ((make-asset (name &optional (d 0))
           (make-instance 'asset :name name :decimals d :exchange *mpex*)))
    (let* ((bitcoin (make-asset "CxBTC" 8)) (assets (list bitcoin)))
      (values (mapcar (lambda (name &aux (asset (make-asset name)))
                        (push asset assets)
                        (make-instance 'market :primary asset :counter bitcoin
                                       :exchange *mpex* :decimals 8 :name name))
                      (mapcar #'car (get-request "mktdepth")))
              assets))))

(defmethod fetch-exchange-data ((exchange (eql *mpex*)))
  (with-slots (markets assets) exchange
    (setf (values markets assets) (get-info))))

(defclass mpex-gate (gate)
  ((exchange :allocation :class :initform *mpex*)
   (agent-url :initarg :agent-url :type string)))

(defmethod gate-post ((gate (eql *mpex*)) key secret request))

;;;
;;; Public Data API
;;;

(defmethod get-book ((market mpex-market) &key (count 200)))

(defun trade-parser (market))
(defmethod trades-since ((market mpex-market) &optional since))

;;;
;;; Private Data API
;;;

(defmethod placed-offers ((gate mpex-gate)))

(defmethod account-balances ((gate mpex-gate)))

;;; All sellers are assesed a 0.2% fee at the moment the sale completes (so if
;;; you sell 500 stocks for 100 satoshi each you get 49`900 satoshi or
;;; 0.000499 BTC). All MKOPT and MKFUT orders are assesed a 2% fee
(defmethod market-fee ((mpex-gate t) market) (fee market))

(defun parse-execution (txid json))
(defun raw-executions (gate &key pair from end))
(defmethod execution-since ((gate mpex-gate) market since))

(defun post-raw-limit (gate type market price volume))
(defmethod post-offer ((gate mpex-gate) offer))

(defun cancel-raw-order (gate oid))
(defmethod cancel-offer ((gate mpex-gate) offer))
