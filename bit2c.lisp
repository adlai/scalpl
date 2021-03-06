(defpackage #:scalpl.bit2c
  (:nicknames #:bit2c) (:export #:*bit2c* #:bit2c-gate)
  (:use #:cl #:base64 #:chanl #:anaphora #:local-time #:scalpl.util
        #:scalpl.actor #:scalpl.exchange))

(in-package #:scalpl.bit2c)

;;; General Parameters
(defparameter *base-url* "https://Bit2C.co.il/")

(setf cl+ssl:*make-ssl-client-stream-verify-default*
      (and (yes-or-no-p "Did you personally verify their certificate?")
           (yes-or-no-p "Are you using this exchange as a bank?")
           (and (not (yes-or-no-p "... do you ever read source code?"))
                (cerror "Continue, having traded warranty for sympathy."
                        "A terrible habit, indeed! More lies than files."))))

(defvar *bit2c* (make-instance 'exchange :name :bit2c))

(defclass bit2c-market (market)
  ((exchange :initform *bit2c*) (fee :initarg :fee :reader fee)))

(defun hmac-sha512 (message secret)
  (let ((hmac (ironclad:make-hmac (string-octets secret) 'ironclad:sha512)))
    (ironclad:update-hmac hmac (string-octets message))
    (ironclad:octets-to-integer (ironclad:hmac-digest hmac))))

(defgeneric make-signer (secret)
  (:method ((signer function)) signer)
  (:method ((string string))
    (lambda (message) (integer-to-base64-string (hmac-sha512 message string))))
  (:method ((stream stream)) (make-signer (read-line stream)))
  (:method ((path pathname)) (with-open-file (data path) (make-signer data))))

(defgeneric make-key (key)
  (:method ((key string)) key)
  (:method ((stream stream)) (read-line stream))
  (:method ((path pathname))
    (with-open-file (stream path)
      (make-key stream))))

(defun bit2c-request (path &rest args)
  (multiple-value-bind (body status headers)
      (apply #'http-request (concatenate 'string *base-url* path) args)
    (if (= status 200) (values (decode-json body) 200)
        (values () status (if (member status '(502 504)) body
                              (getjso "error" (decode-json body)))))))

(defun public-request (method &optional parameters)
  (bit2c-request (format () "Exchanges/~A~:[~;?~A~]" method
                         parameters (concatenate-url-parameters parameters))))

(defun auth-request (verb method key signer &optional params)
  (let* ((nonce (format () "~D" (+ (timestamp-millisecond (now))
                                   (* 1000 (timestamp-to-unix (now))))))
         (data (urlencode-params (acons "nonce" nonce params)))
         (sig (funcall signer data)))
    (bit2c-request method :method verb :content data
                   :additional-headers `(("Sign" . ,sig) ("Key" . ,key)
                                         ("nonce" . ,nonce)
                                         ("Content-Type"
                                          . "application/x-www-form-urlencoded")))))

(defun get-info (&aux assets)
  ;; ideally, this info should be read out of the <script>
  ;; tag at the end of https://www.bit2c.co.il/trade ,
  ;; although that footer includes delisted coins!
  (flet ((asset (name decimals)
           (or (find name assets :key #'name :test #'string=)
               (aprog1 (make-instance 'asset :name name :decimals decimals)
                 (push it assets)))))
    (flet ((make-market (primary)
             (make-instance 'bit2c-market
                            :name (format () "~ANis" primary) :fee 0.5 :decimals 2
                            :primary (asset primary 8) :counter (asset "Nis" 2))))
      (values (mapcar #'make-market '("Btc" "Eth" "Bch" "Grin")) assets))))

(defmethod fetch-exchange-data ((exchange (eql *bit2c*)))
  (with-slots (markets assets) exchange
    (setf (values markets assets) (get-info))))

(defclass bit2c-gate (gate) ((exchange :initform *bit2c*)))

(defmethod gate-post ((gate (eql *bit2c*)) key secret request)
  (destructuring-bind ((verb method) . parameters) request
    (multiple-value-bind (ret status error)
        (auth-request verb method key secret parameters)
      `(,ret ,(aprog1 (if (/= 502 504 status) (getjso "message" error) error)
                (when it (warn it)))))))

(defmethod shared-initialize ((gate bit2c-gate) names &key pubkey secret)
  (multiple-value-call #'call-next-method gate names
                       (mvwrap pubkey make-key) (mvwrap secret make-signer)))

;;;
;;; Public Data API
;;;

(defmethod get-book ((market bit2c-market) &key count)
  (assert (null count) (count)
          "The exchange only provides a complete order book!
For bucket shops, please request referral links from
the good folks at your local Gambler's Anonymous.")
  (flet ((offer-maker (kind)
           (lambda (row)
             (destructuring-bind (price volume) row
               (make-instance kind :market market
                              :price (* 100 price) :volume volume)))))
    (awhen (public-request (format () "~A/orderbook.json" (name market)))
      (with-json-slots (asks bids) it
        (values (mapcar (offer-maker 'ask) asks)
                (mapcar (offer-maker 'bid) bids))))))

(defmethod trades-since ((market bit2c-market) &optional since)
  (awhen (public-request (format () "~A/trades.json" (name market))
                         ;; currently refers to trade ID numbers
                         ;; NB: the parameter is an inclusive bound!
                         (when since `(("since" . ,since))))
    (mapcar (lambda (json)
              (with-json-slots (date price amount tid (bid "isBid")) json
                (make-instance 'trade :direction (if bid "buy" "sell")
                               :price (* 100 price) :volume amount
                               :market market :txid tid :timestamp
                               (unix-to-timestamp date))))
            it)))

;;;
;;; Private Data API
;;;

(defmethod placed-offers ((gate bit2c-gate)))

(defmethod account-balances ((gate bit2c-gate))
  (awhen (gate-request gate '(:get "Account/Balance") ())
    ;; although the AVAILABLE_ and LOCKED_ amounts could be convenient
    ;; in future code, currently only keep the most relevant amounts
    (loop for (name . amount) in it until (string= name "Fees")
       for asset = (find-asset name :bit2c)
       when asset collect (cons-aq asset amount))))

(defmethod market-fee ((gate bit2c-gate) market) (fee market))

(defmethod execution-since ((gate bit2c-gate) market since))

(defmethod post-offer ((gate bit2c-gate) offer))

(defmethod cancel-offer ((gate bit2c-gate) (offer offered)))
