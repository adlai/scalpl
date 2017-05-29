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
  ((exchange :initform *bitmex*) (fee :initarg :fee :reader fee)
   (epsilon :initarg :epsilon :reader epsilon)
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
  (let* ((data (net.aserve:uridecode-string (urlencode-params parameters)))
         (path (concatenate 'string *base-path* method "?" data)))
    (multiple-value-bind (json status)
        (http-request (concatenate 'string *base-url* path))
      (case status (200 (decode-json json))
            (t (values () (cdr (assoc :|error| (decode-json json)))))))))

(defun auth-request (verb method key signer &optional parameters)
  (let* ((data (urlencode-params parameters))
         (path (apply #'concatenate 'string *base-path*
                      method (and (or (not verb) (string= verb "GET"))
                                  parameters `("?" ,data))))
         (nonce (format () "~D" (+ (timestamp-millisecond (now))
                                   (* 1000 (timestamp-to-unix (now))))))
         (sig (funcall signer (concatenate 'string (or (string verb) "GET")
                                           path nonce data))))
    (multiple-value-bind (json status)
        (apply #'http-request (concatenate 'string *base-url* path)
                        :method (intern (or (string verb) "GET") :keyword)
                        :additional-headers `(("api-key" . ,key)
                                              ("api-nonce" . ,nonce)
                                              ("api-signature" . ,sig))
                        (and verb `(:content ,data)))
      (values (decode-json json) status))))

(defun get-info (&aux assets)
  (awhen (public-request "instrument" ()) ; start small
    (flet ((ensure-asset (name)
             (or (find name assets :key #'name :test #'string=)
                 (aprog1 (make-instance 'asset :name name :decimals 0)
                   (push it assets)))))
      (values (mapcar (lambda (pair)
                        (with-json-slots ((fee "takerFee") symbol underlying
                                          (eps "tickSize") (counter "quoteCurrency"))
                            pair
                          (make-instance
                           'bitmex-market :name symbol :fee fee :epsilon eps
                           :decimals 0
                           :primary (ensure-asset (format () "L~A" symbol))
                           :counter (ensure-asset (format () "S~A" symbol)))))
                      it)
              assets))))

(defmethod fetch-exchange-data ((exchange (eql *bitmex*)))
  (with-slots (markets assets) exchange
    (setf (values markets assets) (get-info))))

(defun swagger ()                       ; TODO: swagger metaclient!
  (decode-json (http-request (concatenate 'string *base-url*
                                          "/api/explorer/swagger.json"))))

(defclass bitmex-gate (gate) ((exchange :initform *bitmex*)))

(defmethod gate-post ((gate (eql *bitmex*)) key secret request)
  (destructuring-bind ((verb method) . parameters) request
    (multiple-value-bind (ret status)
        (auth-request verb method key secret parameters)
      (case status (200 (list ret ()))
            (t (list () (cdr (assoc :|error| ret))))))))

(defmethod shared-initialize ((gate bitmex-gate) names &key pubkey secret)
  (multiple-value-call #'call-next-method gate names
                       (mvwrap pubkey make-key) (mvwrap secret make-signer)))

;;;
;;; Public Data API
;;;

(defmethod get-book ((market bitmex-market) &key (count 200)
                     &aux (pair (name market)))
  (loop for raw in
       (public-request "orderBook/L2" `(("symbol" . ,pair)
                                        ("depth" . ,(prin1-to-string count))))
     for type = (string-case ((cdr (assoc :|side| raw)))
                  ("Sell" 'ask) ("Buy" 'bid))
     for offer = (make-instance type :market market
                                :price (cdr (assoc :|price| raw))
                                :volume (cdr (assoc :|size| raw)))
     if (eq type 'ask) collect offer into asks
     if (eq type 'bid) collect offer into bids
     finally (return (values asks bids))))

(defmethod trades-since ((market bitmex-market) &optional since
                         &aux (pair (name market)))
  (awhen (public-request
          "trade" `(("symbol" . ,pair) ("count" . "200")
                    ("startTime"
                     . ,(format-timestring
                         () (or (timestamp since) (timestamp- (now) 1 :minute))
                         :format (butlast +iso-8601-format+ 1)))))
    (mapcar (lambda (trade)
              (flet ((val (key) (cdr (assoc key trade))))
                (make-instance 'trade :market market :direction (val :|side|)
                               :timestamp (parse-timestring (val :|timestamp|))
                               :volume (val :|size|) :price (val :|price|)
                               :cost (* (val :|size|) (val :|price|)))))
            (butlast it))))             ; startTime is inclusive :(

