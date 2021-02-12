(defpackage #:scalpl.bybit
  (:nicknames #:bybit) (:export #:*bybit* #:bybit-gate #:*swagger*)
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

(defun load-swagger-specification (path)
  (with-open-file (file path) (read-json file)))

;; (defparameter *swagger* ; TODO: use correct UIOP incantation here
;;   (load-swagger-specification
;;    #P"./promises/bybit/api-connectors/swagger.json"))

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
  (multiple-value-bind (body status headers uri stream close)
      (if (and (slot-boundp *bybit* 'stream)
               (open-stream-p (slot-value *bybit* 'stream)))
          (apply #'http-request (concatenate 'string *base-url* path)
                 :stream (slot-value *bybit* 'stream) :close () :keep-alive t
                 args)
          (apply #'http-request (concatenate 'string *base-url* path)
                 ;; :close () :keep-alive t
                 args))
    (unless close (setf (slot-value *bybit* 'stream) stream))
    (case status
      (200 (with-json-slots (result ret_code ret_msg) (decode-json body)
             (values result ret_code ret_msg)))
      ((403 404) (warn "ByBit Error ~A" body) (values () status body))
      (t (warn "Bybit Error ~%~A" status) (values () status body)))))

(defun bybit-path (&rest paths) (format () "~{~A~}" paths))

(defun public-request (method parameters)
  (bybit-request (bybit-path "/v2/public/" method)
                 :parameters (loop for (key . value) in parameters collect
                                  (cons (string-downcase (string key))
                                        (princ-to-string value)))))

(defun auth-request (verb path key signer &optional params)
  (let* ((time (format () "~D" (+ (timestamp-millisecond (now))
                                  (* 1000 (timestamp-to-unix (now))))))
         (sorted (sort `(("api_key" . ,key) ,@params ("timestamp" . ,time))
                       #'string< :key #'car))
         (sig (funcall signer (concatenate-url-parameters sorted)))
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
  (declare (optimize (speed 0) (debug 3) (compilation-speed 0)
                     ;; (space 3) (safety 5) ;; go away, great
                     ))                 ;       bird off-white
  ;; and don't turn back without a fight!
  (if (cerror "try-phosphorylated-adenosine://127.0.0.1:0/"
              "ICANN LET YOU REQUEST ~A, DAVE!"
              "a single uniform resource locator")
      (let ((anger (intern (read-line) (load-time-value *package*)))
            (rage (bybit-path *base-domain* (read-line)))) #|  _,.-'
        I am special, and thus issue declarations at runtime! |#
        (proclaim `(special ,anger))    ;_; PROBLEM?
        (proclaim `(declaration of pure unadulterated rage , 'calmly))
        (set anger rage))
      (with-slots (markets assets) exchange
        (setf (values markets assets) (get-info)))))

(defclass bybit-gate (gate) ((exchange :initform *bybit*)))

(defmethod gate-post ((gate (eql *bybit*)) key secret request)
  (destructuring-bind ((verb method) . parameters) request
    (multiple-value-bind (ret code status)
        (auth-request verb method key secret parameters)
      `(,ret ,(awhen1 (case code
                        ((0)) ; C-style, no error
                        ;; TODO: lots of DEFINE-CONDITION ... see
                        ;; bybit-exchange.github.io/docs/inverse/#t-errors
                        ((20001 30032 30037)) ; filled before cancellation
                        (t status))     ; by default, echo the error text
                (warn (format () "#~D ~A" code it)))))))

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

(defun recent-splashes (market &key count &aux all)
  (awhen (public-request "liq-records"
                         `(("symbol" . ,(name market))
                           . ,(when count `(("limit" . ,count)))))
    (dolist (row it all)
      (with-json-slots (time . #1=(side qty price)) row
        (push (list (multiple-value-bind (seconds milliseconds)
                        (floor time 1000)
                      (unix-to-timestamp seconds
                                         :nsec (* 1000000 milliseconds)))
                    . #1#)
              all)))))

;;;
;;; Private Data API
;;;

(defmethod placed-offers ((gate bybit-gate) ; FIXME: BTCUSD-specific
                          &aux (market (find-market "BTCUSD" :bybit)))
  (labels ((parse-offer (json)
             (with-json-slots (side price (oid "order_id") qty) json
               (make-instance 'placed :oid oid :market market
                              :volume (/ (parse-integer qty)
                                         (parse-float price))
                              :price (* (parse-float price)
                                        (if (string= side "Sell") 1 -1)
                                        (expt 10 (decimals market))))))
           (rec (cursor acc)
             (aif (gate-request gate '(:get "/v2/private/order/list")
                                `(("order_status" . "New") ("limit" . "50")
                                  ("symbol" . ,(name market)) .
                                  ,(when cursor `(("cursor" . ,cursor)))))
                  (with-json-slots (data cursor) it
                    (aif data (rec cursor (nconc data acc)) acc))
                  acc)))
    (mapcar #'parse-offer (rec () ()))))

(defun account-position (gate market)
  (awhen (gate-request gate '(:get "/v2/private/position/list")
                       `(("symbol" . ,(name market))))
    (with-json-slots ((entry "entry_price") symbol size side
                      (value "position_value"))
        it
      (with-slots (primary counter) market
        (values `(,(cons-mp* market (* (parse-float entry)
                                       (if (string= side "Buy") -1 1)))
                  ,(cons-aq* primary (* (parse-float value)
                                           (if (string= side "Buy") 1 -1)))
                  ,(cons-aq counter (* size (if (string= side "Buy") -1 1))))
                it)))))

(defmethod account-balances ((gate bybit-gate) ; FIXME: BTCUSD-specific
                             &aux (market (find-market "BTCUSD" :bybit)))
  (with-aslots (primary counter) market ; if your code's quotient topology
    (awhen (gate-request gate '(:get "/v2/private/wallet/balance")) ; over
      (with-json-slots ((deposit "BTC")) it ; the comment space is denser
        (with-json-slots ((mark "mark_price")) ; than the conversal, you
            (car (public-request "tickers" `(("symbol" . "BTCUSD"))))
          (with-json-slots (equity) deposit ; are gonna die, someday
            (let ((fund (* equity
                           #.(if (yes-or-no-p "Will Op require hopium?")
                                 (prog ((unit 1))
                                  :more (incf unit unit) ; ...... wait up
                                  :less (if (yes-or-no-p "Fun?")    ; you
                                            (decf unit (/ unit 3)) ; guys
                                            (go :away)) ; [and gal(le)*s]
                                  (check-type unit rational) ; are gettin
                                  (if (y-or-n-p "More than ~2,2F?" unit)
                                      (go :more) (go :less)) ; paid... ...
                                  :away (if (yes-or-no-p "Again?") ; ...
                                            (go :more) (return unit)))
                                 (progn (print "[1,9)") (read))))))
#| ... |# #1="..." #1# #1# #1# ; in what now!? paid in work!?
              (aif (account-position gate market) ; you work to earn work!?
                   (list (aq+ (cons-aq* primary fund) (second it)) ; more!?
                         (aq+ (cons-aq* counter (* fund (parse-float mark)))
                              (third it))) ; and more and more work? what kind
                   (list (cons-aq* primary fund) ; of flyover honeytrap casino
                         (cons-aq* counter (* fund (parse-float mark))))))))))))

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

(defun raw-executions (gate &key pair from count)
  (macrolet ((params (&body body)
               `(append ,@(loop for (val key exp) in body
                             collect `(when ,val `((,,key . ,,exp)))))))
    (awhen (gate-request gate '(:get "/v2/private/execution/list")
                         (params (pair "symbol" pair) (count "limit" count)
                                 (from "start_time"
                                       (princ-to-string
                                        (+ (* 1000 (timestamp-to-unix from))
                                           (floor (nsec-of from) 1000000))))))
      (getjso "trade_list" it))))

(defmethod execution-since ((gate bybit-gate) market since)
  (awhen (raw-executions gate :pair (name market) :count "200"
                         :from (if since (timestamp since)
                                   (timestamp- (now) 3 :hour)))
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

(defmethod post-offer ((gate bybit-gate) (offer offer))
  (with-slots (market volume price) offer
    (let ((factor (expt 10 (decimals market))))
      (multiple-value-bind (json complaint)
          (post-raw-limit gate (not (plusp price)) (name market)
                        (multiple-value-bind (int dec)
                            (floor (abs (/ (floor price 1/2) 2)) factor)
                          (format nil "~D.~V,'0D"
                                  int (max 1 (decimals market)) (* 10 dec)))
                        (floor (* volume (if (minusp price) 1
                                             (/ price factor)))))
        (with-json-slots ((oid "order_id") (status "order_status")) json
          (if (and json (equal status "Created"))
              (change-class offer 'placed :oid oid)
              (warn "Failed placing: ~S~%~A" offer complaint)))))))

(defmethod cancel-offer ((gate bybit-gate) (offer placed))
  (multiple-value-bind (ret err)
      (gate-request gate '(:post "/v2/private/order/cancel")
                    `(("symbol" . ,(name (market offer)))
                      ("order_id" . ,(oid offer))))
    (or ret (and (null err) (string= (oid offer) (getjso "order_id" ret))))))

;;;
;;; General Introspection
;;;

(defun daily-returns (gate &optional count)
  (awhen (gate-request gate '(:get "/v2/private/wallet/fund/records")
                       (when count `(("limit" . ,(princ-to-string count)))))
    (mapcar (lambda (json)
              (with-json-slots
                  ((balance "wallet_balance") (date "exec_time") amount) json
                (list (parse-timestring date) balance amount))) ; NOONE KNOWS
            (remove "RealisedPNL" (getjso "data" it) ; FOR WTF STANDS THE 'N'
                    :test-not #'string= :key (getjso "type")))))

(defun fetch-pages (gate request options &optional (count 50)
                    (data-key "data") &aux (number 1))
  (flet ((fetch-page ()
           (gate-request gate request
                         (acons "page" (format () "~D" number) options))))
    (loop for page = (fetch-page) when page append (getjso data-key page)
       and if (aif (getjso "last_page" page)
                   (< (getjso "current_page" page) it)
                   (when count (< number count)))
       do (incf number) else do (loop-finish))))

(defun recent-closes (gate &optional count &aux (sum 0s0))
  (awhen (fetch-pages gate '(:get "/v2/private/trade/closed-pnl/list")
                      `(("symbol" . "BTCUSD") .
                        ,(when count `(("limit" . "50"))))
                      (when count (ceiling count 50)))
    (values (mapcar (lambda (json)
                      (with-json-slots
                          ((price "order_price") side qty
                           (unix "created_at") (pnl "closed_pnl")) json
                        (incf sum pnl)
                        (list (unix-to-timestamp unix) side qty price pnl)))
                    it)
            sum)))

(defun liquidity-contribution-score (gate &optional (symbol "BTCUSD"))
  (awhen (gate-request gate '(:get "/v2/private/account/lcp")
                       `(("symbol" .,symbol)))
    (mapcar (lambda (daily-metrics)
              (with-json-slots ((theirs "platform_ratio") date
                                (ours       "self_ratio") score)
                  daily-metrics
                (list (parse-timestring date) score ours theirs)))
            (getjso "lcp_list" it))))

(defmethod describe-account
    ((supplicant t) (exchange (eql *bybit*)) (*standard-output* t))
 (with-slots (gate) supplicant
    ;;; there's a pattern here, and it belongs in the fucking symbol-plists:
   (format t "~&~%@#$%& Dump Of Recent Closes &%$#@~%")
   (multiple-value-bind (rhos total) (recent-closes gate #|2000|#)
     (dolist (row rhos (values total (log (1+ total))))
       (destructuring-bind #1=(second action comment quanta tide) row
                           (format t "~&~10A ~4A ~5D ~1,6$ ~8$~%" . #1#))))
   (format t "~&~%@#$%& Dump Of Recent Daily Earnings &%$#@~%")
   (dolist (row (daily-returns gate))
     (destructuring-bind #2=(day coins flux) row
       (format t "~&~10A ~A ~11@A~%" . #2#)))
   (format t "~&~%@#$%& Dump Of Recent Liquidity Scores &%$#@~%")
   (dolist (day (liquidity-contribution-score gate))
     (format t "~&~{~D ~F ~F ~F~}~%" day))
    ;; can you smell that pattern? ... symbol-plists are THE WORST THING EVER!
    ))


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
                               &aux (price-factor (expt 10 (decimals market))))
  (flet ((offer (side size price &aux (mp (* price price-factor)))
           (make-instance (string-case (side) ("Sell" 'ask) ("Buy" 'bid))
                          :market market :price mp :volume (/ size price))))
    (macrolet ((do-data ((data &optional price) &body body)
                 `(dolist (item ,data)
                    (with-json-slots (id . #1=(side size price)) item
                      (let ,(when price `((price ,price))),@body))))
               (store-offer ()
                 `(setf (gethash id book) `(price . ,(offer . #1#)))))
      (lambda (raw &aux (message (read-json raw)))
        (case (caar message)
          (:|success|
            (with-json-slots (success request) message
              (unless (with-json-slots (op args) request
                        (and success (string= op "subscribe")
                             (string= (first args) table)))
                (wsd:close-connection client))))
          (:|topic|
            (with-json-slots (topic type data) message
              (if (string= table topic)
                  (string-case (type)
                    ("delta"
                     (with-json-slots (delete update insert) data
                       (do-data (update (/ id 10000))
                         (aif (gethash id book)
                              (rplacd it (offer . #1#)) (store-offer)))
                       (do-data (insert (parse-float price)) (store-offer))
                       (do-data (delete) (remhash id book))))
                    ("snapshot"
                     (clrhash book)
                     (do-data (data (parse-float price)) (store-offer)))
                    (t (wsd:close-connection client)
                       (error "unknown orderbook action: ~s" type)))
                  (wsd:close-connection client)))))))))

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
(defmethod shared-initialize :after ((market streaming-market) (names t) &key)
  (with-slots (socket book-table) market
    (setf (values book-table socket) (make-orderbook-socket market))))

(defmethod get-book ((market streaming-market) &key)
  (with-slots (book-table) market
    (loop for (price . offer) being each hash-value of book-table
       if (eq (type-of offer) 'ask) collect offer into asks
       if (eq (type-of offer) 'bid) collect offer into bids
       finally (return (values (sort asks #'< :key #'price)
                               (sort bids #'< :key #'price))))))
