(defpackage #:scalpl.bitmex
  (:nicknames #:bitmex) (:export #:*bitmex* #:bitmex-gate)
  (:use #:cl #:base64 #:chanl #:anaphora #:local-time #:scalpl.util
        #:scalpl.actor #:scalpl.exchange))

(in-package #:scalpl.bitmex)

;;; General Parameters
(defparameter *base-url* "https://www.bitmex.com")
(defparameter *base-path* "/api/v1/")

(defvar *bitmex* (make-instance 'exchange :name :bitmex))

(defclass bitmex-market (market)
  ((exchange :initform *bitmex*)
   (fundings :initarg :fundings)))      ; https://www.bitmex.com/app/swapsGuide

(defun hmac-sha256 (message secret)
  (let ((hmac (ironclad:make-hmac (string-octets secret) 'ironclad:sha256)))
    (ironclad:update-hmac hmac (string-octets message))
    (ironclad:octets-to-integer (ironclad:hmac-digest hmac))))

(defgeneric make-signer (secret)
  (:method ((signer function)) signer)
  (:method ((string string))
    (lambda (message) (format () "~(~64,'0X~)" (hmac-sha256 message string))))
  (:method ((stream stream)) (make-signer (read-line stream)))
  (:method ((path pathname)) (with-open-file (data path) (make-signer data))))

(defgeneric make-key (key)
  (:method ((key string)) key)
  (:method ((stream stream)) (read-line stream))
  (:method ((path pathname))
    (with-open-file (stream path)
      (make-key stream))))

(defun public-request (method parameters)
  (let* ((data (urlencode-params parameters))
         (path (concatenate 'string *base-path* method "?" data)))
    (decode-json (http-request (concatenate 'string *base-url* (print path))))))

(defun auth-request (verb method key signer &optional parameters)
  (let* ((data (urlencode-params parameters))
         (path (apply #'concatenate 'string *base-path*
                      method (and (not verb) parameters `("?" ,data))))
         (nonce (format () "~D" (+ (timestamp-millisecond (now))
                                   (* 1000 (timestamp-to-unix (now))))))
         (sig (funcall signer (print (concatenate
                                'string (or verb "GET") path nonce data)))))
    (decode-json (apply #'http-request (concatenate 'string *base-url* path)
                        :method (intern (or verb "GET") :keyword)
                        :additional-headers `(("api-key" . ,key)
                                              ("api-nonce" . ,nonce)
                                              ("api-signature" . ,sig))
                        (and verb `(:content ,data))))))

(defun get-info (&aux assets)
  (awhen (public-request "instrument" '(("symbol" . "XBTUSD"))) ; start small
    (let ((xbt (make-instance 'asset :name "BTC" :decimals 4))  ;   shameful!
          (usd (make-instance 'asset :name "USD" :decimals 0))) ; such hard code
      (values (mapcar (lambda (pair)    ; this is less hardcoded
                        (with-json-slots ((name "symbol")) ; etc
                            pair
                          (make-instance 'bitmex-market :name name
                                         :fee (error "To be continued")
                                         ;; :decimals decimals :fee fee
                                         ;; :minimum minimum :maximum maximum
                                         ;; :epsilon epsilon :hidden hidden
                                         ;; :primary (ensure-asset (subseq pair 0 3))
                                         ;; :counter (ensure-asset (subseq pair 4))
                                         )))
                      it)
              assets))))

(defmethod fetch-exchange-data ((exchange (eql *bitmex*)))
  (with-slots (markets assets) exchange
    (setf (values markets assets) (get-info))))

(defun swagger ()                       ; TODO: swagger metaclient!
  (decode-json (http-request (concatenate 'string *base-url*
                                          "/api/explorer/swagger.json"))))

(defclass bitmex-gate (gate) ((exchange :initform *bitmex*)))
