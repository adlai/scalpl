(defpackage #:scalpl.phemex
  (:nicknames #:phemex) (:export #:*phemex* #:phemex-gate)
  (:use #:cl #:base64 #:chanl #:anaphora #:local-time #:scalpl.util
        #:scalpl.actor #:scalpl.exchange #:scalpl.qd))

(in-package #:scalpl.phemex)

;;; General Parameters
(defparameter *base-domain* "api.phemex.com")
(defparameter *base-url* (format () "https://~A" *base-domain*))
(setf cl+ssl:*make-ssl-client-stream-verify-default* ())

(defvar *phemex* (make-instance 'exchange :name :phemex :sensitivity 1))

(defclass phemex-market (market)
  ((exchange :initform *phemex*) (fee :initarg :fee :reader fee)))

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

(defun phemex-request (path &rest args)
  (multiple-value-bind (body status headers)
      (apply #'http-request (concatenate 'string *base-url* path) args)
    (awhen (or (getjso :x-ratelimit-remaining headers)
               (getjso :x-ratelimit-remaining-contract headers))
      (sleep (+ (random 1.0) (dbz-guard (/ (1- (parse-integer it)))))))
    (if (= status 200) (values (decode-json body) 200 ())
        (values () status (getjso "msg" (decode-json body))))))

(defun auth-request (verb method key signer &optional params)
  (let* ((query (case verb ((:delete :get) (urlencode-params params))))
         (data (case verb
                 ((:put :post) (json:encode-json-alist-to-string params))))
         (path (apply #'concatenate 'string method (and query `("?" ,query))))
         (expiry (format () "~D" (+ (timestamp-to-unix (now)) 60)))
         (sig (funcall signer (concatenate 'string method query expiry data))))
    (apply #'phemex-request path
           :url-encoder (lambda (url format) (declare (ignore format)) url)
           :additional-headers `(("x-phemex-access-token" . ,key)
                                 ("x-phemex-request-expiry" . ,expiry)
                                 ("x-phemex-request-signature" . ,sig))
           :method verb :content-type "application/json"
           (case verb ((:put :post) `(:content ,data))))))

(defun get-info (&aux assets)
  (awhen (phemex-request "/exchange/public/cfg/v2/products")
    (flet ((make-market (product)
             (with-json-slots
                 ((name "symbol") (long "indexSymbol") (short "quoteCurrency")
                  (decimals "pricePrecision") (lot "lotSize") state)
                 product
               (flet ((make-asset (fake decimals)
                        (let ((name (concatenate 'string fake "-" name)))
                          (aprog1 (make-instance 'asset :name name
                                                 :decimals decimals)
                            (push it assets)))))
                 (make-instance
                  'phemex-market :name name :fee 0.00075 :decimals decimals
                  :primary (make-asset (subseq long 1) 8)
                  :counter (make-asset short 2))))))
      (values (mapcar #'make-market
                      (remove "Spot" (getjso "products" (getjso "data" it))
                              :key (getjso "type") :test #'string=))
              assets))))

(defmethod fetch-exchange-data ((exchange (eql *phemex*)))
  (with-slots (markets assets) exchange
    (setf (values markets assets) (get-info))))

(defclass phemex-gate (gate) ((exchange :initform *phemex*)))

(defmethod gate-post ((gate (eql *phemex*)) key secret request)
  (destructuring-bind ((verb method) . parameters) request
    (multiple-value-bind (ret status error)
        (auth-request verb method key secret parameters)
      `(,ret ,(cons status error)))))

(defmethod shared-initialize ((gate phemex-gate) names &key pubkey secret)
  (multiple-value-call #'call-next-method gate names
                       (mvwrap pubkey make-key) (mvwrap secret make-signer)))

;;;
;;; Public Data API
;;;

(defmethod get-book ((market phemex-market) &key count &aux (pair (name market)))
  (awhen (phemex-request (format () "/md/orderbook?symbol=~A" pair))
    (flet ((offer-maker (kind)
             (lambda (row)
               (destructuring-bind (price volume) row
                 (make-instance kind :market market :price (/ price 1000)
                                :volume (/ volume price 1/10000))))))
      (with-json-slots (asks bids) (getjso "book" (getjso "result" it))
        (values (mapcar (offer-maker 'ask) asks)
                (mapcar (offer-maker 'bid) bids))))))

(defmethod trades-since ((market phemex-market)
                         &optional (since (timestamp- (now) 1 :minute))
                         &aux (pair (name market)))
  (flet ((parse (trade)
           (destructuring-bind (timestamp side price size) trade
             (make-instance 'trade :market market :direction side
                            :timestamp (parse-timestamp *phemex* timestamp)
                            :price (/ price 1000) :cost size
                            :volume (/ size price 1/10000)))))
    (awhen (phemex-request (format () "/md/trade?symbol=~A" pair))
      (remove (etypecase since (timestamp since) (trade (timestamp since)))
              (mapcar #'parse (reverse (getjso "trades" (getjso "result" it))))
              :test #'timestamp>= :key #'timestamp))))

;;;
;;; Private Data API
;;;

(defmethod placed-offers ((gate phemex-gate) ; FIXME: hardcoded market
                          &aux (market (find-market "BTCUSD" :phemex)))
  (awhen (gate-request gate '(:get "/orders/activeList")
                       `(("symbol" . ,(name market))))
    (with-json-slots (code msg data) it
      (if (zerop code)
          (mapcar (lambda (data)
                    (with-json-slots
                        (side price (oid "orderID") (size "orderQty")) data
                      (let ((aksp (string-equal side "Sell")))
                        (make-instance 'placed :oid oid :market market
                                       :volume (/ size price)
                                       :price (* price (if aksp 1 -1)
                                                 (expt 10 (decimals market)))))))
                  (getjso "rows" data))
          (warn msg)))))

(defmethod account-positions ((gate phemex-gate) &aux positions)
  (awhen (gate-request gate '(:get "/accounts/accountPositions")
                       '(("currency" . "BTC")))
    (with-json-slots (code msg data) it
      (if (/= code 0) (warn msg)
          (dolist (position (getjso "positions" data) (values positions data))
            (with-json-slots ((entry "avgEntryPrice") symbol size (cost "posCost"))
                position
              (with-aslots (primary counter) (find-market symbol :phemex)
                (push (list it (cons-mp* it (* entry (- (signum size))))
                            (cons-aq* primary (- cost))
                            (cons-aq* counter (- size)))
                      positions))))))))

(defmethod account-balances ((gate phemex-gate) &aux balances)
  ;; tl;dr - transubstantiates position into 'balances' of long + short
  (multiple-value-bind (positions data) (account-positions gate)
    (with-json-slots ((deposit "accountBalanceEv")) (getjso "account" data)
      (unless (zerop deposit)
        (dolist (position (getjso "positions" data) balances)
          (with-json-slots (symbol leverage (mark "markPrice")) position
            (with-aslots (primary counter) (find-market symbol :phemex)
              (let* ((fund (* deposit leverage))
                     (fiat (* fund mark (expt 10 -8))))
                (flet ((collect (a b) (push a balances) (push b balances)))
                  (aif (find it positions :key #'car)
                       (collect (aq+ (cons-aq primary fund) (third it))
                         (aq+ (cons-aq* counter fiat) (fourth it)))
                       (collect (cons-aq primary fund)
                         (cons-aq* counter fiat))))))))))))

;;; This horror can be avoided via the actor-delegate mechanism.
(defmethod market-fee ((gate phemex-gate) (market phemex-market)) (fee market))
(defmethod market-fee ((gate phemex-gate) market)
  (fee (slot-reduce market scalpl.exchange::%market)))

(defun parse-execution (raw)
  (with-json-slots ((oid "orderID") (txid "execID") (amt "execQty")
                    symbol side (price "execPriceEp") (type "tradeType")
                    (timestamp "transactTimeNs") (volume "execValueEv")
                    (fee "execFeeEv")) raw
    (when (string= type "Trade")
      (let ((market (find-market symbol :phemex)))
        (flet ((adjust (value)
                 (/ value (expt 10 (decimals (primary market))))))
          (let ((volume (adjust volume)) (fee (adjust fee)))
            (list (make-instance 'execution :direction side :market market
                                 :oid oid :txid txid :cost amt :net-cost amt
                                 :price (/ price 10000) :volume (abs volume)
                                 :timestamp (parse-timestamp *phemex* timestamp)
                                 :net-volume (abs (+ volume fee))))))))))

(defun raw-executions (gate &key (pair "BTCUSD") start end count)
  (macrolet ((params (&body body)
               `(append ,@(loop for (val key exp) in body
                             collect `(when ,val `((,,key . ,,exp)))))))
    (gate-request gate '(:get "/exchange/order/trade")
                  (params (pair "symbol" pair) (count "count" count)
                          (start "start" (timestamp-to-unix start))
                          (end "end" (timestamp-to-unix end))
                          (count "withCount" count)))))

(defmethod parse-timestamp ((exchange (eql *phemex*)) (timestamp integer))
  (multiple-value-bind (unix nsec) (floor timestamp (expt 10 9))
    (unix-to-timestamp unix :nsec nsec)))

(defmethod execution-since ((gate phemex-gate) market since)
  (awhen (raw-executions gate :pair (name market)
                         :start (if since (timestamp since)
                                    (timestamp- (now) 5 :day)))
    (with-json-slots (code msg data) it
      (let ((rows (reverse (getjso "rows" data))))
        (when (zerop code)
          (mapcan #'parse-execution
                  (if (null since) rows
                      (subseq rows (1+ (position (txid since) rows
                                                 :test #'string=
                                                 :key (getjso "execID")))))))))))

(defun post-raw-limit (gate buyp market price size)
  (gate-request gate '(:post "/orders")
                `(("clOrdID" . ,(format () "scalpl~D" (timestamp-to-unix (now))))
                  ("ordType" . "Limit") ("orderQty" . ,size)
                  ("priceEp" . ,(* price 1000))
                  ("side" . ,(if buyp "Buy" "Sell"))
                  ("symbol" . ,market)
                  ("timeInForce" . "PostOnly"))))

(defmethod post-offer ((gate phemex-gate) (offer offer))
  (with-slots (market volume price) offer
    (let ((factor (expt 10 (decimals market))))
      (awhen (post-raw-limit gate (not (plusp price)) (name market) (abs price)
                             (floor (* volume (if (minusp price) 1
                                                  (/ price factor)))))
        (with-json-slots (code msg data) it
          (if (zerop code)
              (with-json-slots ((oid "orderID") (status "ordStatus")) data
                (if (string= status "Created")
                    (change-class offer 'placed :oid oid)
                    (warn "Failed placing: ~S~%~A" offer status)))
              (warn "Failed placing: ~S~%~A" offer msg)))))))

(defmethod cancel-offer ((gate phemex-gate) (offer placed))
  (with-json-slots (code msg)
      (gate-request gate '(:delete "/orders/cancel")
                    `(("orderID" . ,(oid offer))
                      ("symbol" . ,(name (market offer)))))
    (case code ((0 10002) t) (t (warn "~A ~D ~A" offer code msg)))))

;;;
;;; Comte Monte Carte
;;;

(defmethod bases-for ((supplicant supplicant) (market phemex-market))
  (with-slots (gate) supplicant         ; FIXME: XBTUSD-specific
    (awhen (assoc (name market) (account-positions gate)
                  :test #'string= :key #'name)
      (let ((entry (realpart (second it))) (size (abs (quantity (fourth it)))))
        (flet ((foolish (basis &aux (price (realpart (car basis))))
                 (if (= (signum price) (signum entry)) (> price entry)
                     (and (< (isqrt size) (quantity (second basis)))
                          (< (isqrt size) (quantity (third basis)))))))
          (multiple-value-bind (primary counter) (call-next-method)
            (values (remove-if #'foolish primary)
                    (remove-if #'foolish counter))))))))

;;;
;;; Websocket
;;;

(defparameter *websocket-url* (format () "wss://phemex.com/ws"))

(defun connect-websocket-client ()
  (wsd:start-connection (wsd:make-client *websocket-url*)))

;;; At this point, further implementation of the Phemex client has been placed
;;; on hiatus due to the fact that they do not publish orderbook depth beyond
;;; $15 on either side, neither in the REST API nor the Websocket, and this is
;;; considered insufficient for running the default market maker.

;;; It is possible to complete the API for use in other trading strategies,
;;; although this is discouraged and currently at a low priority.
