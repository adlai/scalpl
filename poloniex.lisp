(defpackage #:scalpl.poloniex
  (:nicknames #:poloniex)
  (:export #:*poloniex* #:poloniex-gate)
  (:use #:cl #:chanl #:anaphora #:local-time #:scalpl.util #:scalpl.exchange))

(in-package #:scalpl.poloniex)

;; Please note that there is a default limit of 6 calls per second.
;; All calls to the trading API are sent via HTTP POST to
;; https://poloniex.com/tradingApi and must contain the following headers:

;; Key - Your API key.
;; Sign - The query's POST data signed by your key's "secret" with HMAC-SHA512
;; Additionally, all queries must include an increasing "nonce" POST parameter.

(defun hmac-sha512 (message secret)
  (let ((hmac (ironclad:make-hmac (string-octets secret) 'ironclad:sha512)))
    (ironclad:update-hmac hmac (string-octets message))
    (ironclad:octets-to-integer (ironclad:hmac-digest hmac))))

;; All responses from the trading API are in JSON format.
;; In the event of an error, the response will always be of the following format:
;; {"error":"<error message>"}

;; There are several methods accepted by the trading API,
;; each of which is specified by the "command" POST parameter:

(defgeneric make-signer (secret)
  (:method ((signer function)) signer)
  (:method ((string string))
    (lambda (message) (format () "~(~128,'0X~)" (hmac-sha512 message string))))
  (:method ((stream stream)) (make-signer (read-line stream)))
  (:method ((path pathname)) (with-open-file (data path) (make-signer data))))

(defgeneric make-key (key)
  (:method ((key string)) key)
  (:method ((stream stream)) (read-line stream))
  (:method ((path pathname))
    (with-open-file (stream path)
      (make-key stream))))

;;; General Parameters
(defparameter +base-path+ "https://poloniex.com/")
(defparameter +public-stub+ "public?command=")
(defparameter +private-path+ (concatenate 'string +base-path+ "tradingApi"))

(defun public-request (method &rest parameters)
  (decode-json
   (http-request (apply 'concatenate
                        'string +base-path+ +public-stub+ method
                        (when parameters
                          `("&" ,(urlencode-params parameters)))))))

(defun auth-request (method key signer &optional parameters)
  (push (cons "command" method) parameters)
  (let* ((nonce (format () "~D" (+ (timestamp-millisecond (now))
                                   (* 1000 (timestamp-to-unix (now))))))
         (data (urlencode-params (acons "nonce" nonce parameters)))
         (sig (funcall signer data)))
    (decode-json
     (http-request +private-path+ :method :post :content data
                   :additional-headers `(("Key" . ,key) ("Sign" . ,sig))))))
