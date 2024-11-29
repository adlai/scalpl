(defpackage #:scalpl.bit2c
  ;; (:nicknames #:BITTWOC) ; DID THAT COST YOU MONEY ? L1 L2 L3 L4
  (:export #:*bit2c* #:bit2c-gate #|#:+action-enum+|#)
  (:use #:cl #:base64 #:chanl #:anaphora #:local-time #:scalpl.util
        ;; #:|| #+(and) #-(or) #:\\require-X3J13 #:deterministic-gc
        #:scalpl.net #:scalpl.actor #:scalpl.exchange))

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
  (:method ((string string)) (lambda (message) (hmac-sha512 message string)))
  (:method ((stream stream)) (make-signer (string-upcase (read-line stream))))
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
    (sleep (random (exp 1/2)))          ; WHY HIT RATE LIMIT, FOOL !?
    (let ((json (ignore-errors (decode-json body)))
          (raw (if (stringp body) body
                   (flexi-streams:octets-to-string
                    body :external-format :utf8))))
      (case status
        (200 (values json 200 raw headers))
        ;; the rate limit should never get hit!
        ;; however, it is also undocumented... too much work?
        ((409) (sleep (random pi)) (values () status raw headers))
        ;; why special-case these, here?
        ((404 500 502 504) (values () status raw headers))
        ;; TODO: how to best print hebrew without unicode fonts?
        (t (values () status raw))))))

(defun public-request (method &optional parameters)
  (bit2c-request (format () "Exchanges/~A~:[~;?~A~]" method parameters
                         (concatenate-url-parameters parameters))))

(defun auth-request (verb method key signer &optional params)
  (let* ((now (now))
         (nonce (format () "~D" (+ (multiple-value-bind (mils mics)
                                       (timestamp-millisecond now)
                                     (+ (isqrt mics) (* 31 (isqrt mils))))
                                   (* 1000 (timestamp-to-unix now)))))
         (data (concatenate-url-parameters (acons "nonce" nonce params)))
         (sig (funcall signer data)))
    (bit2c-request method :method verb :content data :backoff 1
                   :additional-headers `(("Sign" . ,sig) ("Key" . ,key)
                                         ("nonce" . ,nonce)
                                         ("Content-Type"
                                          . "application/x-www-form-urlencoded")))))

(defun get-info (&aux assets)
  ;; theoretically, the info could be taken from one of the <script>
  ;; tags in https://www.bit2c.co.il/trade ; although it is probably
  ;; some form of sibling evil to premature optimisation, and might
  ;; also be considered worsening of slippery slopes to altcoins.
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
      (values (mapcar #'make-market '("Btc" "Eth" "Bch" "Grin" "Usdc")) assets))))

(defmethod fetch-exchange-data ((exchange (eql *bit2c*)))
  (with-slots (markets assets) exchange
    (setf (values markets assets) (get-info))))

(defclass bit2c-gate (gate) ((exchange :initform *bit2c*)))

(defmethod gate-post ((gate (eql *bit2c*)) key secret request)
  (declare (optimize debug))            ; WHO HAS BEEN KILLED
  (destructuring-bind ((verb method) . parameters) request
    (prog () (sleep (exp -1))
     :loop (sleep (random (sqrt 13)))   ; WHO DIED
       (multiple-value-bind (ret status error headers)
           (auth-request verb method key secret parameters)
         (return
           `(,ret ,(aprog1 (case status
                             (200 nil)
                             ((nil 404) (concatenate 'string method
                                                     " [ \\equiv 404 ]"))
                             (409 (warn "Rate limited at ~A"
                                        (getjso :date headers))
                              (sleep (random (sqrt pi))) (go :loop))
                             ((500 502 504 524) error) ; could be #()
                             (t (awhen (ignore-errors (read-json error))
                                  (or (ignore-errors (getjso "message" it))
                                      (ignore-errors (getjso "error" it))
                                      it error))))
                     (when (stringp it)	; ARE FAILURES INTERESTING?
                       (warn (if (zerop (count #\Newline it)) it
                                 (format () "HTTP ~D Error~%~A"
                                         status headers)))))))))))

(defmethod shared-initialize ((gate bit2c-gate) names &key pubkey secret)
  (multiple-value-call #'call-next-method gate names
                       (mvwrap pubkey make-key) (mvwrap secret make-signer)))

;;;
;;; Public Data API
;;;

;;; "https://bit2c.co.il/home/api#geto"
;;; 03:Param Description
;;; 04:ll    last price
;;; 05:av    last 24 hours price avarage
;;; 06:a     last 24 hours volume
;;; 07:h     highest buy order
;;; 08:l     lowest sell order
;;; 09:c     last 24 hours average change - decimal(nullable)
;;; 10:up    last change - true = up , false - down , null - no change
;; (multiple-value-bind
;;       (octets http-status response-headers locator
;;        stream null string)
;;     (drakma:http-request
;;      "https://www.bit2c.co.il/Exchanges/BtcNis/Ticker.json"
;;      :want-stream nil :user-agent (load-time-value (gensym "") t))
;;   (values (map 'string 'code-char octets) (decode-json octets)
;;           (now) response-headers
;;           (list null locator stream http-status string)))

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
    ;; when do you use the partial and quietly-released endpoint?
    ;; (scalpl.bit2c::public-request "BtcNis/orderbook-top.json")
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

;;; Fee structure advertised in https://bit2c.co.il/home/Fees
(defmethod market-fee ((gate bit2c-gate) (market bit2c-market))
  (aif (handler-bind
           ((simple-warning             ; currently, I'm useless and
              (lambda (warning)         ; writing to myself ... plus
                (when (slot-boundp *bit2c* 'markets)
                  (muffle-warning warning)))))
         (gate-request gate '(:GET "Funds/GetUsersFees")
                       (warn 'simple-warning :format-control
                             "undocumented Funds/GetUsersFees")))
       (with-json-slots ((current-maker "feeMaker")) it
         current-maker)
       (fee market)))
;;; publishing it is +EV thus git push

;;; The endpoint Funds/GetUsersFees is not part of the official API
(defun fee-tier-progress (gate)
  (aprog1 (now)
    (with-json-slots
        ((current-maker "feeMaker") (current-taker "feeTaker")
         (next-maker "nextfeeMaker") (next-taker "nextfeeTaker")
         (users-fee "usersFee") (volume "totalbalance"))
        (gate-request gate '(:GET "Funds/GetUsersFees") ())
      (format t "~&~A~%You've traded ~2$ NIS within the window:~%~
                 Current fees: ~$% taker [and ~$% maker]~%~
                 Next Tier: ~$% taker [and ~$% maker]~%~
                 Mystery 'Users Fee': ~A~%(NOW) > ,it = ~A~% //\\"
              it volume current-taker current-maker
              next-taker next-maker users-fee (now)))))
;;; consider arguing (format nil "~/signum/" EV), professionally ;)

;;;
;;; Private Data API
;;;

(defun offer-parser (market)
  (lambda (json)
    (with-json-slots
        ((status "status_type") type    ; type = 1 for ask, 0 for bid
         ;; order_type should ALWAYS = 0, i.e. limit
         created amount price id) json
      (make-instance 'offered :oid (prin1-to-string id) :volume amount
                              :market market
                              :price (* price 2 (- type 1/2)
                                        (expt 10 (decimals market)))))))

(defmethod placed-offers ((gate bit2c-gate) &optional market)
  (if market
      (awhen (gate-request gate '(:get "Order/MyOrders")
                           `(("pair" . ,(name market))))
        (with-json-slots (ask bid) (getjso (name market) it)
          (nconc (mapcar (offer-parser market) ask)
                 (mapcar (offer-parser market) bid))))
      (awhen (gate-request gate '(:get "Order/MyOrders"))
        (loop for (name . json) in it
              and market = (find-market name :bit2c)
              nconc (with-json-slots (ask bid) json
                      (nconc (mapcar (offer-parser market) ask)
                             (mapcar (offer-parser market) bid)))))))

(defmethod account-balances ((gate bit2c-gate))
  (awhen (gate-request gate '(:GET "Account/Balance") ())
    ;; although the AVAILABLE_ and LOCKED_ amounts could be convenient
    ;; in future code, currently only keep the most relevant amounts
    (loop for (name . amount) in it until (string= name "Fees")
          for asset = (find-asset name :bit2c)
          when asset collect (cons-aq* asset amount))))

;;; Could scrape from "https://bit2c.co.il/home/api#orderh" ...
(defparameter +action-enum+
  '((0 . :Buy) (1 . :Sell)
    (2 . :Deposit) (3 . :Withdrawal)
    (4 . :FeeWithdrawal) (10 . :Unknown)
    (11 . :SendPayment) (12 . :ReceivedPayment)
    (21 . :DepositVIACredit)
    (23 . :RefundWithdrawal)
    (24 . :RefundFeeWithdrawal)
    (26 . :DepositFee)
    (27 . :RefundDepositFee)            ; OM NOM NOMM MLEM MLEM FF
    (31 . :DepositInterest)))           ; RIBA RIBA RIBA RIBA RIBA

(defparameter *bit2c-timestamp*
  '((:day 2) "/" (:month 2) "/" :year " "
    (:hour 2) ":" (:min 2) ":" (:sec 2) ".000"))

(defun parse-execution (json market)
  (with-json-slots (ticks action price reference) json
    (flet ((adjust (field)
             (abs (parse-float (remove #\, (getjso field json))))))
      (make-instance 'execution :market market ; consider using TXID
                     :oid reference            ; field of superclass
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
  (awhen (gate-request gate '(:GET "Order/OrderHistory")
                       `(("pair" . ,(name market))
                         ,@(when since
                             `(("fromTime"
                                . ,(format-timestring
                                    () (timestamp since)
                                    :format *bit2c-timestamp*))))))
    (when (eq :|error| (caar it)) (return-from execution-since ()))
    (let ((executions (remove 2 (reverse it)
                              :key (getjso "action") :test #'<=)))
      (mapcar (lambda (json) (parse-execution json market))
              (if (null since) executions
                  (remove (timestamp-to-unix (timestamp since)) executions
                          :key (getjso "ticks") :test #'=))))))

(defun post-raw-limit (gate buyp market price size)
  (check-type price string "Must supply decimals as strings")
  (check-type size string "Must supply decimals as strings")
  (gate-request gate '(:POST "Order/AddOrder")
                `(("pair" . ,(name market))
                  ("IsBid" . ,(if buyp "true" "false"))
                  ("Price" . ,price) ("Amount" . ,size))))

(defmethod post-offer ((gate bit2c-gate) (offer offer))
  (with-slots (market volume price) offer
    (let ((dec-al (expt 10 (decimals market))))
      (multiple-value-bind (json complaint)
          (post-raw-limit gate (not (plusp price)) market
                          (multiple-value-bind (int dec)
                              (floor (abs price) dec-al)
                            (format () "~D.~V,'0D" int
                                    (decimals market) dec))
                          (format () "~8$"
                                  (/ volume
                                     (if (plusp price) 1
                                         (/ (abs price) dec-al)))))
        (with-json-slots
            ((response "OrderResponse") (echo "NewOrder")) json
          (let ((now (now)) (message (getjso "Error" response)))
            (cond
              ((search " 30% " message)
               (warn "~&Exceeded 30% price range ~A~%~A" now message))
              ((null complaint)
               (atypecase (getjso "id" echo)
                 ((integer 1)
                  (change-class offer 'offered
                                :oid (prin1-to-string it)))
                 ((eql 0) (cerror "Proceed" "Break-point one")
                  (warn "FIXME! Balance guard possibly failed..."))))
              (t (cerror "Proceed" "Break-point two")
                 (warn "~S" (or (getjso "Message" response) message))))))))))

(defmethod cancel-offer ((gate bit2c-gate) (offer offered))
  (with-slots (oid) offer
    (multiple-value-bind (ret err)
        (gate-request gate '(:POST "Order/CancelOrder") `(("id" . ,oid)))
      (if (null err)
          (with-json-slots ((errorp "HasError") (message "Error"))
              (getjso "OrderResponse" ret)
            (or (not errorp)
                (aand (search oid message)
                      (string= (subseq message (+ it (length oid)))
                               " not found."))
                (values () message)))
          (values () err)))))

;;; EXPLAME YOURSELF, BOOKISH BOOKIE ! BOOKWORM BOOGER BUGGER BAGGER stripmine, lol
;;; "#<2088550125 1963.09 Nis @ 219155.69>       #<ASK  0.16721539 Btc @ 225594.57>
;;; Totals:
;;;                3670899.68 Nis > 0.10             17.22194805 Btc < 2000000.00 
;;; I failed calculus, so why take my word for any of these reckonings?
;;; Looked across past   +30.885 days 2024-10-10T21:29:12.625675+03:00
;;; where account traded +.27067 btc,
;;; captured profit of   -.00746 btc,
;;; expected turnover of  +169.8 days;
;;; chudloadic exkrmnt:  -0.031%
;;; mean daily profit:   -0.03247%
;;; "

;;; The following code belongs, probably, after some page break...
;; (defun account-history (gate &optional stream)
;;   (aprog1 (gate-request gate '(:GET "Order/AccountHistory") ())
;;     ;; (format stream "~A" it)
;;     ))

;;; The following code was only reasonable for a window where the
;;; encountered accountAction values were {0,1,3,4,31}; a smarter
;;; grouping would be helpful for the complete account overview.
;; (loop with groupings for record in *
;;       for action = (getjso "accountAction" record)
;;       for index = (floor (log (1+ action) 3))
;;       do (push record (getf groupings index))
;;       finally (return (length groupings)))

(defmethod describe-account
    ((supplicant supplicant) (exchange (eql *bit2c*)) (stream t)))

;;;
;;; "Even musicians have to eat." - Consider The Source
;;;

(defun withdraw-shekels (gate amount comment)
  (check-type amount (integer 1))	; restart-case arity?
  (check-type comment (string))		; MUCH less than half kB
  (gate-request gate '(:POST "Funds/AddFund")
                `(("Total" . ,(format () "~D" amount))
                  ("Reference" . ,(format () "~S" comment))
                  ("IsDeposit" . "false"))))

