(defpackage #:scalpl.bybit
  (:nicknames #:bybit) (:export #:*bybit* #:bybit-gate #:swagger)
  (:use #:cl #:base64 #:chanl #:anaphora #:local-time #:scalpl.util
        #:scalpl.actor #:scalpl.exchange))

(in-package #:scalpl.bybit)

;;; General Parameters
(defparameter *base-domain* "api.bybit.com")
(defparameter *base-url* (format () "https://~A" *base-domain*))

(defvar *bybit* (make-instance 'exchange :name :bybit :sensitivity 1))

;;; HALT THOU
;;;
;;; ... oops, too late, defvar is some sorta rewritable run-oncely
;;;
;;; You may also want to consult a
;;; github.com/bybit-exchange/api-connectors/blob/?#,swagger.json
;;; thanks to @dexter-2 for the pointer from their Issues flytrap

(defclass bybit-market (market)
  ((exchange :initform *bybit*) (fee :initarg :fee :reader fee)))

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

(defun bybit-request (path &rest args)
  (multiple-value-bind (body status)
      (apply #'http-request (concatenate 'string *base-url* path) args)
    (case status
      (200 (with-json-slots (result ret_code ret_msg) (decode-json body)
             (values result ret_code ret_msg)))
      ((403 404) (warn "ByBit Error ~A" body) (values () status body))
      (t (warn "Bybit Error ~%~A" status) (values () status body)))))

(defun bybit-path (&rest paths) (format () "~{~A~}" paths))

(defun public-request (method parameters)
  (bybit-request (bybit-path "/v2/public/" method) :parameters parameters))

(defun auth-request (verb path key signer &optional params)
  (let* ((time (format () "~D" (+ (timestamp-millisecond (now))
                                  (* 1000 (timestamp-to-unix (now))))))
         (sorted (sort `(("api_key" . ,key) ,@params ("timestamp" . ,time))
                       #'string< :key #'car))
         (sig (funcall signer (urlencode-params sorted)))
         (parameters `(,@sorted ("sign" . ,sig))))
    (bybit-request path :method verb :parameters parameters)))

(defun get-info (&aux assets)
  (flet ((make-market (instrument)
           (with-json-slots ((long "base_currency") (short "quote_currency")
                             name (fee "maker_fee") (filter "price_filter"))
               instrument
             (flet ((asset (fake)
                      (let ((name (concatenate 'string fake "-" name)))
                        (or (find name assets :key #'name :test #'string=)
                            (aprog1 (make-instance 'asset :name name :decimals
                                                   (if (string= fake "BTC")
                                                       8 0))
                              (push it assets)))))
                    (ilog (i) (floor (log (abs i) 10))))
               (make-instance
                'bybit-market :name name :fee (parse-float fee)
                :decimals (- (ilog (parse-float (getjso "tick_size" filter))))
                :primary (asset long)
                :counter (asset short))))))
    (awhen (public-request "symbols" ())
      (values (mapcar #'make-market it) assets))))

(defmethod fetch-exchange-data ((exchange (eql *bybit*)))
  (with-slots (markets assets) exchange
    (setf (values markets assets) (get-info))))

(defclass bybit-gate (gate) ((exchange :initform *bybit*)))

(defmethod gate-post ((gate (eql *bybit*)) key secret request)
  (destructuring-bind ((verb method) . parameters) request
    (multiple-value-bind (ret code status)
        (auth-request verb method key secret parameters)
      `(,ret ,(aprog1 (unless (zerop code) status) (when it (warn it)))))))

(defmethod shared-initialize ((gate bybit-gate) names &key pubkey secret)
  (multiple-value-call #'call-next-method gate names
                       (mvwrap pubkey make-key) (mvwrap secret make-signer)))

;;;
;;; Public Data API
;;;

(defmethod get-book ((market bybit-market) &key count &aux (pair (name market)))
  (loop for raw in
       (public-request "orderBook/L2" `(("symbol" . ,pair)))
     for price = (parse-float (getjso "price" raw))
     for type = (string-case ((getjso "side" raw)) ("Sell" 'ask) ("Buy" 'bid))
     for offer = (make-instance type :market market
                                :price (* (expt 10 (decimals market)) price)
                                :volume (/ (getjso "size" raw) price))
     if (eq type 'ask) collect offer into asks
     if (eq type 'bid) collect offer into bids
     finally (return (values asks bids))))

(defmethod trades-since ((market bybit-market) &optional from
                         &aux (pair (name market)))
  (flet ((parse (trade)
           (with-json-slots (id side time qty price) trade
             (make-instance 'trade :market market :direction side :txid id
                            :timestamp (parse-timestring time)
                            :volume (/ qty price) :price price :cost qty))))
    (awhen (public-request "trading-records"
                           `(("symbol" . ,pair)
                             ,@(when from
                                 `(("from"
                                    .,(princ-to-string (txid from)))))))
      (if from (cdr (mapcar #'parse it)) (nreverse (mapcar #'parse it))))))

;;;
;;; Private Data API
;;;

(defmethod placed-offers ((gate bybit-gate) &aux offers (page 1))
  (flet ((fetch-page ()
           (gate-request gate '(:get "/open-api/order/list")
                         `(("order_status" . "New")
                           ("limit" . "50")
                           ("page" .,(format () "~D" page)))))
         (parse-offer (json)
           (with-json-slots
               (symbol side price (oid "order_id") qty) json
             (let ((market (find-market symbol :bybit))
                   (aksp (string-equal side "Sell")))
               (make-instance 'placed :oid oid :market market
                              :volume (/ qty price)
                              :price (* price (if aksp 1 -1)
                                        (expt 10 (decimals market))))))))
    (loop
       (awhen (fetch-page)
         (with-json-slots ((current "current_page") (count "last_page") data) it
           (dolist (json data) (push (parse-offer json) offers))
           (if (= current count) (return offers) (incf page)))))))

(defun account-position (gate market)
  (awhen (gate-request gate '(:get "/v2/private/position/list")
                       `(("symbol" . ,(name market))))
    (with-json-slots ((entry "entry_price") symbol size side
                      (value "position_value"))
        it
      (with-slots (primary counter) market
        (list (cons-mp* market (* (parse-float entry)
                                  (if (string= side "Buy") -1 1)))
              (cons-aq* primary (* (parse-float value)
                                   (if (string= side "Buy") 1 -1)))
              (cons-aq counter (* size (if (string= side "Buy") -1 1))))))))

(defmethod account-balances ((gate bybit-gate) ; FIXME: BTCUSD-specific
                             &aux (market (find-market "BTCUSD" :bybit)))
  (with-aslots (primary counter) market
    (awhen (gate-request gate '(:get "/v2/private/wallet/balance"))
      (with-json-slots ((deposit "BTC")) it
        (with-json-slots ((mark "mark_price"))
            (car (public-request "tickers" `(("symbol" . "BTCUSD"))))
          (let ((fund (* 5 (getjso "equity" deposit))))
            (aif (account-position gate market)
                 (list (aq+ (cons-aq* primary fund) (second it))
                       (aq+ (cons-aq* counter (* fund (parse-float mark)))
                            (third it)))
                 (list (cons-aq* primary fund)
                       (cons-aq* counter (* fund (parse-float  mark)))))))))))

;;; This horror can be avoided via the actor-delegate mechanism.
(defmethod market-fee ((gate bybit-gate) (market bybit-market)) (fee market))
(defmethod market-fee ((gate bybit-gate) market)
  (fee (slot-reduce market scalpl.exchange::%market)))

(defmethod parse-timestamp ((exchange (eql *bybit*)) (timestamp integer))
  (multiple-value-bind (unix milliseconds) (floor timestamp 1000)
    (unix-to-timestamp unix :nsec (* 1000000 milliseconds))))

(defun parse-execution (raw)
  (with-json-slots ((oid "order_id") (txid "exec_id") (amt "exec_qty")
                    symbol side (price "exec_price") (time "trade_time_ms")
                    (execost "exec_value") (exefee "exec_fee"))
      raw
    (unless (zerop (length side))
      (let ((market (find-market symbol :bybit)))
        (flet ((adjust (value) (parse-float value)))
          (let ((volume (adjust execost)) (fee (adjust exefee)))
            (make-instance 'execution :direction side :market market
                           :oid oid :txid txid :cost amt :net-cost amt
                           :price (adjust price) :volume (abs volume)
                           :timestamp (parse-timestamp *bybit* time)
                           :net-volume (abs (+ volume fee)))))))))

(defun raw-executions (gate &key pair from count page)
  (macrolet ((params (&body body)
               `(append ,@(loop for (val key exp) in body
                             collect `(when ,val `((,,key . ,,exp)))))))
    (awhen (gate-request gate '(:get "/v2/private/execution/list")
                         (params (pair "symbol" pair) (count "limit" count)
                                 (from "start_time"
                                       (princ-to-string
                                        (+ (* 1000 (timestamp-to-unix from))
                                           (floor (nsec-of from) 1000000))))
                                 (page "page" page)))
      (getjso "trade_list" it))))

(defmethod execution-since ((gate bybit-gate) market since)
  (awhen (raw-executions gate :pair (name market)
                                 :from (if since (timestamp since)
                                           (timestamp- (now) 5 :day)))
    (mapcar #'parse-execution
            (if (null since) it
                (subseq it (1+ (position (txid since) it :test #'string=
                                         :key (getjso "exec_id"))))))))

(defun post-raw-limit (gate buyp market price size)
  (gate-request gate '(:post "/v2/private/order/create")
                `(("symbol" . ,market) ("side" .,(if buyp "Buy" "Sell"))
                  ("order_type" . "Limit") ("price" . ,price)
                  ("qty" . ,(princ-to-string (floor size)))
                  ("time_in_force" . "PostOnly"))))

(defmethod post-offer ((gate bybit-gate) offer)
  (with-slots (market volume price) offer
    (let ((factor (expt 10 (decimals market))))
      (with-json-slots ((oid "order_id") (status "order_status") text)
          (post-raw-limit gate (not (plusp price)) (name market)
                          (multiple-value-bind (int dec)
                              (floor (abs (/ (floor price 1/2) 2)) factor)
                            (format nil "~D.~V,'0D"
                                    int (max 1 (decimals market)) (* 10 dec)))
                          (floor (* volume (if (minusp price) 1
                                               (/ price factor)))))
        (if (equal status "Created") (change-class offer 'placed :oid oid)
            (warn "Failed placing: ~S~%~A" offer text))))))

(defmethod cancel-offer ((gate bybit-gate) (offer placed))
  (multiple-value-bind (ret err)
      (gate-request gate '(:post "/v2/private/order/cancel")
                    `(("symbol" . ,(name (market offer)))
                      ("order_id" . ,(oid offer))))
    (when (or (null err) (string= err "Order already cancelled"))
      (string= (oid offer) (getjso "order_id" ret)))))

;;;
;;; Liquidity Contribution Metrics
;;;

(defun liquidity-contribution-score (gate &optional (symbol "BTCUSD"))
  (awhen (gate-request gate '(:get "/v2/private/account/lcp")
                       `(("symbol" .,symbol)))
    (mapcar (lambda (daily-metrics)
              (with-json-slots ((theirs "platform_ratio") date
                                (ours       "self_ratio") score)
                  daily-metrics
                (list (parse-timestring date) score ours theirs)))
            (getjso "lcp_list" it))))

;;;
;;; Comte Monte Carte
;;;

(defmethod bases-for ((supplicant supplicant) (market bybit-market))
  (with-slots (gate) supplicant         ; FIXME: bitmex copypasta
    (awhen (account-position gate market)
      (let ((entry (realpart (first it))) (size (abs (quantity (third it)))))
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

(defparameter *websocket-url* "wss://stream.bybit.com/realtime")

(defun make-websocket-handler (client table market book
                               &aux (next-expected :subscribe)
                                 (price-factor (expt 10 (decimals market))))
  (flet ((offer (side size price &aux (mp (* price price-factor)))
           (make-instance (string-case (side) ("Sell" 'ask) ("Buy" 'bid))
                          :market market :price mp :volume (/ size price))))
    (lambda (raw &aux (message (read-json raw)))
      (case next-expected
        (:subscribe
         (with-json-slots (success request) message
           (if (with-json-slots (op args) request
                 (and success (string= op "subscribe")
                      (string= (first args) table)))
               (setf next-expected :table)
               (wsd:close-connection client))))
        (:table
         (with-json-slots (topic type data) message
           (macrolet ((do-data ((&rest slots) data &body body)
                        `(dolist (item ,data)
                           (with-json-slots ,slots item ,@body))))
             (flet ((build ()
                      (do-data (id side size price) data
                               (let ((price (parse-float price)))
                                 (setf (gethash id book)
                                       (cons price (offer side size price)))))))
               (if (string= table topic)
                   (string-case (type)
                     ("delta"
                      (with-json-slots (delete update insert) data
                        (do-data (id side size) update
                           (let ((price (/ id 10000)))
                             (aif (gethash id book)
                                  (rplacd it (offer side size price))
                                  (setf (gethash id book)
                                        (cons price (offer side size price))))))
                        (do-data (id side size price) insert
                           (let ((price (parse-float price)))
                             (setf (gethash id book)
                                   (cons price (offer side size price)))))
                        (do-data (id) delete (remhash id book))))
                     ("snapshot" (clrhash book) (build))
                     (t (wsd:close-connection client)
                        (error "unknown orderbook action: ~s" type)))
                   (wsd:close-connection client))))))))))

(defun make-orderbook-socket (market &optional (depth 200) (frequency 100))
  (let* ((book (make-hash-table :test #'eq))
         (topic (format () "orderBook~@[_~D~].~Dms.~A" depth frequency
                        (name market)))
         (client (wsd:make-client *websocket-url*)))
    (wsd:on :message client (make-websocket-handler client topic market book))
    (wsd:start-connection client)
    (wsd:send client (format () "{\"op\":\"subscribe\",\"args\":[~S]}" topic))
    (values book client)))

(defclass streaming-market (bybit-market) (socket book-table))
(defmethod shared-initialize :after ((market streaming-market) slot-names &key)
  (with-slots (socket book-table) market
    (setf (values book-table socket) (make-orderbook-socket market))))

(defmethod get-book ((market streaming-market) &key)
  (with-slots (book-table) market
    (loop for (price . offer) being each hash-value of book-table
       if (eq (type-of offer) 'ask) collect offer into asks
       if (eq (type-of offer) 'bid) collect offer into bids
       finally (return (values (sort asks #'< :key #'price)
                               (sort bids #'< :key #'price))))))

;; ;;;
;; ;;; Bulk Placement and Cancellation
;; ;;;

;; (defun offer-alist (offer)
;;   (with-slots (market price given taken) offer
;;     (let ((buyp (minusp price)) (factor (expt 10 (decimals market))))
;;       (multiple-value-bind (int dec)
;;           (floor (abs (/ (floor price 1/2) 2)) factor)
;;         `(("symbol" . ,(name market))
;;           ("price" . ,(format () "~D.~V,'0D" int
;;                               (max 1 (decimals market)) dec))
;;           ("orderQty" . ,(princ-to-string
;;                           (* (- (signum price))
;;                              (scaled-quantity (if buyp given taken)))))
;;           ("execInst" . "ParticipateDoNotInitiate"))))))

;; (defun post-bulk (gate &rest offers)
;;   (let ((orders (format () "[~{~A~^,~}]"
;;                         (reduce 'mapcar '(json:encode-json-alist-to-string
;;                                           offer-alist)
;;                                 :from-end t :initial-value offers))))
;;     (awhen (gate-request gate '(:post "order/bulk") `(("orders" . ,orders)))
;;       (loop for data in it for status = (getjso "ordStatus" data)
;;          when (equal status "New") collect
;;            (with-json-slots
;;                (symbol side price (oid "orderID") (size "orderQty")) data
;;              (let ((market (find-market symbol :bybit))
;;                    (aksp (string-equal side "Sell")))
;;                (make-instance 'placed :oid oid :market market
;;                               :volume (/ size price)
;;                               :price (* price (if aksp 1 -1)
;;                                         (expt 10 (decimals market))))))))))

;; (defun cancel-bulk (gate &rest offers)
;;   (gate-request gate '(:delete "order")
;;                 `(("orderID" . ,(mapcar #'oid offers)))))
