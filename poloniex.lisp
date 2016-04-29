(defpackage #:scalpl.poloniex
  (:nicknames #:poloniex)
  (:export #:*poloniex* #:poloniex-gate)
  (:use #:cl #:chanl #:anaphora #:local-time #:scalpl.util #:scalpl.exchange))

(in-package #:scalpl.poloniex)

(defun hmac-sha512 (message secret)
  (let ((hmac (ironclad:make-hmac (string-octets secret) 'ironclad:sha512)))
    (ironclad:update-hmac hmac (string-octets message))
    (ironclad:octets-to-integer (ironclad:hmac-digest hmac))))

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

(defclass poloniex-market (market)
  ((fee :allocation :class :initform '(0.25 . 0.25))))

(defun get-info (&aux assets)
  (awhen (public-request "returnTicker")
    (flet ((ensure-asset (name)
             (or (find name assets :key #'name :test #'string=)
                 (aprog1 (make-instance 'asset :name name :decimals 8)
                   (push it assets)))))
      (values (mapcar (lambda (data &aux (pair (car data)))
                        (make-instance
                         'poloniex-market :name pair :decimals 8
                         :primary (ensure-asset (subseq (string pair) 4))
                         :counter (ensure-asset (subseq (string pair) 0 3))))
                      (remove "0" it :test-not #'string=
                              :key (lambda (x) (cdr (assoc :|isFrozen| (cdr x))))))
              assets))))

(defvar *poloniex* (make-instance 'exchange :name :poloniex :sensitivity 1))

(defmethod fetch-exchange-data ((exchange (eql *poloniex*)))
  (with-slots (markets assets) exchange
    (setf (values markets assets) (get-info))))

(defclass poloniex-gate (gate)
  ((exchange :allocation :class :initform *poloniex*)))

(defmethod shared-initialize ((gate poloniex-gate) names &key pubkey secret)
  (multiple-value-call #'call-next-method gate names
                       (mvwrap pubkey make-key) (mvwrap secret make-signer)))

(defmethod gate-post ((gate (eql *poloniex*)) key secret request)
  (destructuring-bind (command . options) request
    (alet (auth-request command key secret options)
      (if (eq (caar it) :|error|) (list (warn (cdar it)) (cdar it)) (list it)))))

;;; public data API

(defmethod get-book ((market poloniex-market) &key (count 200)
                     &aux (pair (name market)))
  (let ((decimals (slot-value market 'decimals)))
    (with-json-slots (bids asks)
        (public-request "returnOrderBook"
                        `(:|currencyPair| . ,pair)
                        `(:|depth| . ,count))
      (flet ((parser (class)
               (lambda (raw-order)
                 (destructuring-bind (price amount) raw-order
                   (make-instance
                    class :market market :volume amount
                    :price (parse-price price decimals))))))
        (values (mapcar (parser 'ask) asks)
                (mapcar (parser 'bid) bids))))))

(defmethod parse-timestamp ((exchange (eql *poloniex*)) (timestring string))
  (parse-timestring (replace timestring "T" :start1 10)))

(defmethod trades-since ((market poloniex-market) &optional since)
  (mapcar (lambda (trade)
            (with-json-slots (rate amount date type total) trade
              (let ((price  (parse-float rate))
                    (volume (parse-float amount :type 'rational))
                    (cost (parse-float total)))
                (make-instance 'trade :market market :direction type
                               :timestamp (parse-timestamp *poloniex* date)
                               :volume volume :price price :cost cost))))
          (reverse (apply 'public-request
                          "returnTradeHistory"
                          `(:|currencyPair| . ,(name market))
                          (when since
                            `(("start" . ,(1+ (timestamp-to-unix
                                               (timestamp since))))))))))

;;; private data API

(defmethod account-balances ((gate poloniex-gate))
  (aif (gate-request gate "returnCompleteBalances")
       (mapcan (lambda (pair &aux (asset (find-asset (car pair) :poloniex)))
                 (let* ((balances (cdr pair))
                        (available (parse-float (getjso "available" balances)
                                                :type 'number))
                        (on-orders (parse-float (getjso "onOrders" balances)
                                                :type 'number))
                        (amount (+ available on-orders)))
                   (unless (zerop amount)
                     `(,(cons-aq* asset amount)))))
               it)
       (error "communication breakdown!")))

(defmethod placed-offers ((gate poloniex-gate))
  (mapcan (lambda (pair)
            (let ((market (find-market (car pair) :poloniex)))
              (mapcar (lambda (order)
                        (flet ((key (key) (getjso key order)))
                          (let ((bidp (string= "buy" (key "type")))
                                (rate (* (expt 10 (decimals market))
                                         (parse-float (key "rate")
                                                      :type 'number)))
                                (amount (parse-float (key "amount")
                                                     :type 'number)))
                            (make-instance
                             'placed :market market :volume amount
                             :given (cons-aq (funcall (if bidp #'counter
                                                          #'primary) market)
                                             (* rate amount))
                             :oid (parse-integer (key "orderNumber"))
                             :price (* rate (if bidp -1 1))))))
                      (cdr pair))))
          (remove () (gate-request gate "returnOpenOrders"
                                   '(("currencyPair" . :all)))  ; kludge
                  :key #'cdr)))

(defmethod market-fee ((gate poloniex-gate) (market market)) ; kludge!!!!
  ;; to avoid fireplay, we assume that we always pay the taker fee
  (* 100 (parse-float (getjso "takerFee" (gate-request gate "returnFeeInfo")))))

(defun execution-parser (market)
  (lambda (jso)
    (with-json-slots ((oid "orderNumber") (txid "globalTradeID")
                      date rate amount total type category fee) jso
      (assert (string= category "exchange")) ; for now
      (flet ((parse (float) (parse-float float :type 'number)))
        (let ((volume (parse amount)) (price (parse rate)) (cost (parse total))
              (after-fee (- 1 (parse fee)))) ; fees deducted from taken asset
          (make-instance 'execution :market market :oid (parse oid) :txid txid
                         :timestamp (parse-timestamp *poloniex* date)
                         :direction type :volume volume :cost cost :price price
                         :net-cost (string-case (type)
                                     ("buy" cost) ("sell" (* cost after-fee)))
                         :net-volume (string-case (type)
                                       ("buy" (* volume after-fee))
                                       ("sell" volume))))))))

(defun raw-executions-since (gate market &key start end)
  (reverse (gate-request gate "returnTradeHistory"
                         `(("currencyPair" . ,(name market))
                           ,@(when start `(("start" . ,(1+ start))))
                           ,@(when end `(("end" . ,end)))))))

(defmethod execution-since ((gate poloniex-gate) (market market) since)
  (mapcar (execution-parser market)
          (remove "exchange"            ; one day, you'll see... that i am gone!
                  (apply #'raw-executions-since gate market
                         (when since
                           `(:start ,(timestamp-to-unix (timestamp since)))))
                  :test-not #'string= :key (getjso "category"))))

;;; actions

(defmethod cancel-offer ((gate poloniex-gate) (offer placed))
  (with-json-slots (success error)
      (gate-request gate "cancelOrder" `(("orderNumber" . ,(oid offer))))
    (if (not (zerop success)) offer
        (if (gate-request gate "returnOrderTrades"
                          `(("orderNumber" . ,(oid offer))))
            offer (warn error)))))      ; generalized boolean

(defmethod post-offer ((gate poloniex-gate) (offer offer)) ; if only, if only...
  (with-slots (market volume price) offer
    (flet ((str (amt) (format () "~8$" (float amt 0d0))))
      (let ((rate (/ (abs price) (expt 10 8))))
        (with-json-slots ((oid "orderNumber"))
            (gate-request
             gate (if (plusp price) "sell" "buy")
             `(("currencyPair" . ,(name (market offer)))
               ("rate" . ,(str rate))
               ("amount" . ,(str (if (plusp price) volume
                                     (/ volume rate))))))
          (if oid (aprog1 offer (change-class it 'placed :oid oid))
              (values () ())))))))
