(defpackage #:scalpl.bit2c
  (:nicknames #:bit2c) (:export #:*bit2c* #:bit2c-gate)
  (:use #:cl #:base64 #:chanl #:anaphora #:local-time #:scalpl.util
        ;; #:|| #+(and) #-(or) #:\\require-X3J13 #:deterministic-gc
	#:scalpl.actor #:scalpl.exchange))

(in-package #:scalpl.bit2c)

;;; General Parameters
(defparameter *base-url* "https://Bit2C.co.il/")

(setf cl+ssl:*make-ssl-client-stream-verify-default*
      (and (and (yes-or-no-p "Did you personally verify its certificate?")
		(string= #1="~&Type the fingerprint: "
			 (progn (format *query-io* #1#) (read-line))))
           (string= #2="~&Explain why you use it as a bank:~%"
		    (progn (format *query-io* #2#) (read-line)))
           (and (not (yes-or-no-p "... do you ever read source code?"))
                (cerror "Continue, having traded warranty for sympathy."
                        "A terrible habit, indeed! More lies than files."))))
;;; ... was that contract smart?

(defvar *bit2c* (make-instance 'exchange :name :bit2c))

(defclass bit2c-market (market)
  ((exchange :initform *bit2c*) (fee :initarg :fee :reader fee)))

(defun hmac-sha512 (message secret)
  (let ((hmac (ironclad:make-hmac (string-octets secret) 'ironclad:sha512)))
    (ironclad:update-hmac hmac (string-octets message))
    (usb8-array-to-base64-string (ironclad:hmac-digest hmac))))

(defgeneric make-signer (secret)
  (:method ((signer function)) signer)
  (:method ((string string))
    (lambda (message) (hmac-sha512 message string)))
  (:method ((stream stream)) (make-signer (read-line stream)))
  (:method ((path pathname)) (with-open-file (data path) (make-signer data))))

(defgeneric make-key (key)
  (:method ((key string)) key)
  (:method ((stream stream)) (read-line stream))
  (:method ((path pathname))
    (with-open-file (stream path)
      (make-key stream))))

(defun bit2c-request (path &rest args)
  (declare (optimize debug)) (sleep 1/7)
  (multiple-value-bind (body status headers)
      (apply #'http-request (concatenate 'string *base-url* path) args)
    (case status
      (200 (values (decode-json body) 200 body headers))
      ;; the rate limit should never get hit!
      ((409) (sleep 11/13) (values () status body headers))
      ;; why special-case these, here?
      ((404 500 502 504) (values () status body headers))
      ;; TODO: how to best print hebrew without unicode fonts?
      (t (values () status
		 (if (stringp body) body
		     (flexi-streams:octets-to-string
		      body :external-format :utf8)))))))

(defun public-request (method &optional parameters)
  (bit2c-request (format () "Exchanges/~A~:[~;?~A~]" method
                         parameters (concatenate-url-parameters parameters))))

(defun auth-request (verb method key signer &optional params)
  ;; (sleep 5/7)
  (let* ((now (now))
	 (nonce (format () "~D" (+ (multiple-value-bind (mils mics)
				       (timestamp-millisecond now)
				     (+ (isqrt mics) (* 31 (isqrt mils))))
                                   (* 1000 (timestamp-to-unix now)))))
         (data (concatenate-url-parameters (acons "nonce" nonce params)))
         (sig (funcall signer data)))
    (bit2c-request method :method verb :content data :backoff 0
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
                            :name (format () "~ANis" primary)
			    :fee 3 :decimals 2
                            :primary (asset primary 8)
			    :counter (asset "Nis" 2))))
      (values (mapcar #'make-market '("Btc" "Eth" "Bch" "Grin")) assets))))

(defmethod fetch-exchange-data ((exchange (eql *bit2c*)))
  (with-slots (markets assets) exchange
    (setf (values markets assets) (get-info))))

(defclass bit2c-gate (gate) ((exchange :initform *bit2c*)))

(defmethod gate-post ((gate (eql *bit2c*)) key secret request)
  (declare (optimize debug))
  (destructuring-bind ((verb method) . parameters) request
    (prog () :loop
      (multiple-value-bind (ret status error headers)
          (auth-request verb method key secret parameters)
	(return
	  `(,ret ,(aprog1 (case status
			    ((nil 404) "Not found")
			    (409 (warn "Rate limited at ~A"
				       (getjso :date headers))
			     (sleep 1) (go :loop))
			    ((500 502 504 524) error)
			    (t (awhen (ignore-errors (read-json error))
				 (or (ignore-errors (getjso "message" it))
				     (ignore-errors (getjso "error" it))
				     it error))))
		    (when it
		      (warn (if (zerop (count #\Newline it)) it
				(format () "HTTP ~D Error~%~A"
					status headers)))))))))))

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
    (loop for book = (public-request (format () "~A/orderbook.json"
					     (name market)))
	  for delay = 0.1 then (* 2 delay)
	  when book return
	    (with-json-slots (asks bids) book
              (values (mapcar (offer-maker 'ask) asks)
                      (mapcar (offer-maker 'bid) bids)))
	  do (sleep delay))))

(defmethod trades-since ((market bit2c-market) &optional since)
  (awhen (public-request (format () "~A/trades.json" (name market))
                         ;; currently refers to trade ID numbers
                         ;; NB: the parameter is an inclusive bound!
                         (when since `(("since" . ,(txid since)))))
    (mapcar (lambda (json)
              (with-json-slots (date price amount tid (bid "isBid")) json
                (make-instance 'trade :direction (if bid "buy" "sell")
                                      :price price :volume amount
                                      :cost (* amount price)
                                      :market market :txid tid :timestamp
                                      (unix-to-timestamp date))))
            (if since (remove (txid since) it :key (getjso "tid")) it))))

;;; Fees advertised in https://bit2c.co.il/home/Fees
(defmethod market-fee ((gate bit2c-gate) (market bit2c-market))
  (aif (gate-request gate '(:get "Account/Balance") ())
       (reduce #'getjso `("FeeMaker" ,(name market) "Fees")
	       :initial-value it :from-end t)
       (fee market)))

;;; This function has not been published in their documentation yet,
;;; so I suppose it could change unexpectedly... thus, avoid using
;;; this function programmatically.
(defun fee-tier-progress (gate)
  (with-json-slots
      ((fee "Fee") (left "Volume") (target "AmountFrom"))
      (gate-request gate '(:get "Account/NextTradeFees") ())
    (format t "~&You're ~2$ NIS away from the fee drop:~%~
                 Next Tier: ~$% taker and ~$% maker~%"
	    (- target left) fee (- fee 1/2))))

;;;
;;; Private Data API
;;;

(defun parse-offer (json &aux (market (find-market "BtcNis" :bit2c)))
  (with-json-slots
      ((status "status_type") type ; order_type should ALWAYS = 0, i.e. limit
       created amount price id) json
    (make-instance 'offered :oid (prin1-to-string id) :volume amount
                   :market market
                   :price (* price 2 (- type 1/2)
                             (expt 10 (decimals market))))))

(defmethod placed-offers ((gate bit2c-gate) &optional market)
  (awhen (gate-request gate '(:get "Order/MyOrders") '(("pair" . "BtcNis")))
    (with-json-slots (ask bid) (getjso "BtcNis" it)
      (nconc (mapcar #'parse-offer ask) (mapcar #'parse-offer bid)))))

(defmethod account-balances ((gate bit2c-gate))
  (awhen (gate-request gate '(:get "Account/Balance") ())
    ;; although the AVAILABLE_ and LOCKED_ amounts could be convenient
    ;; in future code, currently only keep the most relevant amounts
    (loop for (name . amount) in it until (string= name "Fees")
       for asset = (find-asset name :bit2c)
       when asset collect (cons-aq* asset amount))))

(defparameter *action-enum*
  '((0 . :Buy) (1 . :Sell)
    (2 . :Deposit) (3 . :Withdrawal)
    (4 . :FeeWithdrawal) (10 . :Unknown)
    (11 . :SendPayment) (12 . :ReceivedPayment)
    (21 . :DepositVIACredit)
    (23 . :RefundWithdrawal) (24 . :RefundFeeWithdrawal)
    (26 . :DepositFee) (27 . :RefundDepositFee)))

(defun account-history (gate)
  (gate-request gate '(:get "Order/AccountHistory") ()))

(defparameter *bit2c-timestamp*
  '((:day 2) "/" (:month 2) "/" :year " "
    (:hour 2) ":" (:min 2) ":" (:sec 2) ".000"))

(defun parse-execution (json &optional (market (find-market :btcnis :bit2c)))
  (with-json-slots (ticks action price reference) json
    (flet ((adjust (field)
	     (abs (parse-float (remove #\, (getjso field json))))))
      (make-instance 'execution :market market
                     :oid (subseq reference
                                  (1+ (position #\| reference :from-end t)))
                     :timestamp (unix-to-timestamp ticks)
                     :direction (case action (0 "buy") (1 "sell"))
                     :price (parse-float (remove #\, price))
                     :cost (+ (adjust "secondAmount")
                              (case action
				(1 (adjust "feeAmount")) (0 0) (t 0)))
                     :net-cost (adjust "secondAmount")
                     :volume (adjust "firstAmount")
                     :net-volume (adjust "firstAmount")))))

;; (defmethod execution-until ...)

(defmethod execution-since ((gate bit2c-gate) market since)
  (awhen (gate-request gate '(:get "Order/OrderHistory")
                       `(("pair" . ,(name market))
                         ,@(when since
                             `(("fromTime" . ,(format-timestring
                                               () (timestamp since)
                                               :format *bit2c-timestamp*))))))
    (when (eq :|error| (caar it)) (return-from execution-since ()))
    (let ((executions (remove 2 (reverse it) :key (getjso "action") :test #'<=)))
      (mapcar #'parse-execution
              (if (null since) executions
                  (remove (timestamp-to-unix (timestamp since)) executions
                          :key (getjso "ticks") :test #'=))))))

(defun post-raw-limit (gate buyp market price size)
  (check-type price string "Must supply decimals as strings")
  (check-type size string "Must supply decimals as strings")
  (gate-request gate '(:post "Order/AddOrder")
                `(("pair" . ,(name market))
                  ("IsBid" . ,(if buyp "true" "false"))
                  ("Price" . ,price) ("Amount" . ,size))))

(defmethod post-offer ((gate bit2c-gate) (offer offer))
  ;; (when *features* (break))
  (with-slots (market volume price) offer
    (let ((factor (expt 10 (decimals market))))
      (multiple-value-bind (json complaint)
          (post-raw-limit gate (not (plusp price)) market
                          (multiple-value-bind (int dec)
                              (floor (abs price) factor)
                            (format () "~D.~V,'0D" int (decimals market) dec))
                          (format () "~8$"
				  (if (plusp price) volume
                                      (/ volume (/ (abs price) factor)))))
	(with-json-slots
	    ((response "OrderResponse") (echo "NewOrder") error) json
	  ;; (warn "~S" (map 'list 'char-name (getjso "Message" response)))
	  (sleep 1/2)
	  (or (unless complaint
		(awhen (getjso "id" echo)
		  (change-class offer 'offered :oid (prin1-to-string it))))
	      (unless (let ((length (length error)))
			(awhen (search "nonce" error :from-end t)
			  (or ;(warn "~&Is your accellerator unsolicited?~%")
			   (string= (subseq error (+ it 6) (- length 2))
				    (subseq error (position #\( error)
					    (position #\) error))))))
		(warn "~&Failed placing ~A:~%~A~&" offer
		      (or complaint error))
		)))))))

(defmethod cancel-offer ((gate bit2c-gate) (offer offered))
  (with-slots (oid) offer
    (multiple-value-bind (ret err)
        (gate-request gate '(:post "Order/CancelOrder") `(("id" . ,oid)))
      (if (null err)
          (with-json-slots ((errorp "HasError") (message "Error"))
              (getjso "OrderResponse" ret)
            (or (not errorp)
                (aand (search oid message)
                      (string= (subseq message (+ it (length oid)))
                               " not found."))
                (values () message)))
          (values () err)))))

(defmethod describe-account
    ((supplicant supplicant) (exchange (eql *bit2c*)) (stream t)))

;;;
;;; "Even musicians have to eat." - Consider The Source
;;;

(defun withdraw-shekels (gate amount comment)
  ;; (check-type amount integer)		; request will fail if <= zero
  (check-type comment string)
  (gate-request gate '(:post "Funds/AddFund")
		`(("Total" . ,(format () "~D" amount))
		  ("Reference" . ,(format () "~S" comment))
		  ("IsDeposit" . "false"))))

