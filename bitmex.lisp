(defpackage #:scalpl.bitmex
  (:nicknames #:bitmex) (:export #:*bitmex* #:bitmex-gate #:swagger)
  (:use #:cl #:base64 #:chanl #:anaphora #:local-time #:scalpl.util
        #:scalpl.actor #:scalpl.exchange))

(in-package #:scalpl.bitmex)

;;; General Parameters
(defparameter *base-domain* "www.bitmex.com")
(defparameter *base-url* (format () "https://~A" *base-domain*))
(setf cl+ssl:*make-ssl-client-stream-verify-default* ())

(defvar *bitmex* (make-instance 'exchange :name :bitmex :sensitivity 1))

(defclass bitmex-market (market)
  ((exchange :initform *bitmex*) (fee :initarg :fee :reader fee)
   (symbol-index :initarg :index)))

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

(defun bitmex-request (path &rest args)
  (multiple-value-bind (body status headers)
      (apply #'http-request (concatenate 'string *base-url* path) args)
    (case status
      ((500 502 504) (values () status body))
      (t (awhen (getjso :x-ratelimit-remaining headers)
           (sleep (+ 1 (random 1.0) (dbz-guard (/ (1- (parse-integer it)))))))
         (if (= status 200) (values (decode-json body) 200)
             (values () status (getjso "error" (decode-json body))))))))

(defun bitmex-path (&rest paths) (format () "/api/~{~A~}" paths))

(defun swagger (&key) (bitmex-request (bitmex-path "explorer/swagger.json")))
(defparameter *swagger* (swagger))

(defun public-request (method parameters)
  (bitmex-request
   (apply #'bitmex-path "v1/" method
          (and parameters `("?" ,(net.aserve:uridecode-string
                                  (urlencode-params parameters)))))))

(defun auth-request (verb method key signer &optional params)
  (let* ((data (urlencode-params params))
         (path (apply #'bitmex-path "v1/" method
                      (and (eq verb :get) params `("?" ,data))))
         (nonce (format () "~D" (+ (timestamp-millisecond (now))
                                   (* 1000 (timestamp-to-unix (now))))))
         (sig (funcall signer
                       (concatenate 'string (string verb) path nonce
                                    (unless (eq verb :get) data)))))
    (apply #'bitmex-request path
           :url-encoder (lambda (url format) (declare (ignore format)) url)
           :additional-headers `(("api-signature" . ,sig)
                                 ("api-key" . ,key) ("api-nonce" . ,nonce))
           :method verb (unless (eq verb :get) `(:content ,data)))))

(defun get-info (&aux assets)
  (awhen (public-request "instrument" '(("count" . 500)))
    (flet ((make-market (instrument index)
             (with-json-slots
                 ((tick "tickSize") (lot "lotSize") (fee "takerFee")
                  (name "symbol") (long "rootSymbol") (short "quoteCurrency")
                  multiplier state)
                 instrument
               (flet ((asset (fake &optional (decimals 0))
                        (let ((name (concatenate 'string fake "-" name)))
                          (or (find name assets :key #'name :test #'string=)
                              (aprog1 (make-instance 'asset :name name
                                                     :decimals decimals)
                                (push it assets)))))
                      (ilog (i) (floor (log (abs i) 10))))
                 (when (string= state "Open")
                   (make-instance
                    'bitmex-market :name name :fee fee
                    :decimals (- (ilog tick)) :index index
                    :primary (asset long (ilog multiplier))
                    :counter (asset short (ilog lot))))))))
      (values (loop for definition in it and index from 0
                 for market = (make-market definition index)
                 when market collect it)
              assets))))

(defmethod fetch-exchange-data ((exchange (eql *bitmex*)))
  (with-slots (markets assets) exchange
    (setf (values markets assets) (get-info))))

(defclass bitmex-gate (gate) ((exchange :initform *bitmex*)))

(defmethod gate-post ((gate (eql *bitmex*)) key secret request)
  (destructuring-bind ((verb method) . parameters) request
    (multiple-value-bind (ret status error)
        (auth-request verb method key secret parameters)
      `(,ret ,(aprog1 (if (/= 502 504 status) (getjso "message" error) error)
                (when it (warn it)))))))

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
     for price = (getjso "price" raw)
     for type = (string-case ((getjso "side" raw)) ("Sell" 'ask) ("Buy" 'bid))
     for offer = (make-instance type :market market
                                :price (* (expt 10 (decimals market)) price)
                                :volume (/ (getjso "size" raw) price))
     if (eq type 'ask) collect offer into asks
     if (eq type 'bid) collect offer into bids
     finally (return (values (nreverse asks) bids))))

(defmethod trades-since ((market bitmex-market)
                         &optional (since (timestamp- (now) 1 :minute))
                         &aux (pair (name market)))
  (flet ((parse (trade)
           (with-json-slots (side timestamp size price) trade
             (make-instance 'trade :market market :direction side
                            :timestamp (parse-timestring timestamp)
                            :volume (/ size price) :price price :cost size))))
    (alet (mapcar #'parse
                  (public-request
                   "trade" `(("symbol" . ,pair) ("count" . 100)
                             ("startTime" .,(format-timestring
                                             () (timestamp- (now) 1 :minute)
                                             :timezone +utc-zone+)))))
      (if (not since) it (remove (timestamp since) it
                                 :test #'timestamp> :key #'timestamp)))))

;;;
;;; Private Data API
;;;

(defmethod placed-offers ((gate bitmex-gate))
  (awhen (gate-request gate '(:get "order") '(("filter" . "{\"open\": true}")))
    (mapcar (lambda (data)
              (with-json-slots
                  (symbol side price (oid "orderID") (size "orderQty")) data
                (let ((market (find-market symbol :bitmex))
                      (aksp (string-equal side "Sell")))
                  (make-instance 'placed :oid oid :market market
                                 :volume (/ size price)
                                 :price (* price (if aksp 1 -1)
                                           (expt 10 (decimals market)))))))
            it)))

(defmethod account-positions ((gate bitmex-gate) &aux positions)
  (awhen (remove-if-not (getjso "isOpen")
                        (gate-request gate '(:get "position") ()))
    (dolist (position it (values positions it))
      (with-json-slots ((entry "avgEntryPrice") symbol
                        (size "currentQty") (cost "posCost"))
          position
        (with-aslots (primary counter) (find-market symbol :bitmex)
          (push (list it (cons-mp* it (* entry (- (signum size))))
                      (cons-aq primary (- cost))
                      (cons-aq counter (- size)))
                positions))))))

(defmethod account-balances ((gate bitmex-gate) &aux balances)
  ;; tl;dr - transubstantiates position into 'balances' of long + short
  (flet ((collect (a b) (push a balances) (push b balances)))
    (let ((positions (account-positions gate))
          (instruments (public-request "instrument/active" ()))
          (deposit (gate-request gate '(:get "user/wallet") ())))
      (when deposit
        (dolist (instrument instruments balances)
          (with-json-slots (symbol (mark "markPrice")) instrument
            (unless (find #\_ symbol)   ; ignore binaries (UP and DOWN)
              (with-aslots (primary counter)
                  (find-market symbol :bitmex)
                (let ((fund (/ (* 10 (getjso "amount" deposit)) ; ick
                               (expt 10 (decimals primary)))))
                  (aif (find it positions :key #'car)
                       (collect (aq+ (cons-aq* primary fund) (third it))
                         (aq+ (cons-aq* counter (* fund mark)) (fourth it)))
                       (collect (cons-aq* primary fund)
                         (cons-aq* counter (* fund mark)))))))))))))

;;; This horror can be avoided via the actor-delegate mechanism.
(defmethod market-fee ((gate bitmex-gate) (market bitmex-market)) (fee market))
(defmethod market-fee ((gate bitmex-gate) market)
  (fee (slot-reduce market scalpl.exchange::%market)))

(defun parse-execution (raw)
  (with-json-slots ((oid "orderID") (txid "execID") (amt "lastQty")
                    symbol side price timestamp (execost "execCost")
                    (execom "execComm")) raw
    (unless (zerop (length side))
      (let ((market (find-market symbol :bitmex)))
        (flet ((adjust (value)
                 (/ value (expt 10 (decimals (primary market))))))
          (let ((volume (adjust execost)) (fee (adjust execom)))
            (list (make-instance 'execution :direction side :market market
                                 :oid oid :txid txid :cost amt :net-cost amt
                                 :price price :volume (abs volume)
                                 :timestamp (parse-timestamp *bitmex* timestamp)
                                 :net-volume (abs (+ volume fee))))))))))

(defun raw-executions (gate &key pair from end count)
  (macrolet ((params (&body body)
               `(append ,@(loop for (val key exp) in body
                             collect `(when ,val `((,,key . ,,exp)))))))
    (gate-request gate '(:get "execution/tradeHistory")
                  (params (pair "symbol" pair) (count "count" count)
                          (from "startTime" from)
                          (end "endTime" (subseq (princ-to-string end) 0 19))))))

(defmethod parse-timestamp ((exchange (eql *bitmex*)) (timestamp string))
  (parse-rfc3339-timestring timestamp))

(defmethod execution-since ((gate bitmex-gate) market since)
  (awhen (raw-executions gate :pair (name market)
                         :from (if since (timestamp since)
                                   (timestamp- (now) 11 :hour)))
    (mapcan #'parse-execution
            (if (null since) it
                (subseq it (1+ (position (txid since) it
                                         :test #'string= :key #'cdar)))))))

(defun post-raw-limit (gate buyp market price size)
  (gate-request gate '(:post "order")
                `(("symbol" . ,market) ("price" . ,price)
                  ("orderQty" . ,(princ-to-string
                                  (* (if buyp 1 -1) (floor size))))
                  ("execInst" . "ParticipateDoNotInitiate"))))

(defmethod post-offer ((gate bitmex-gate) offer)
  (with-slots (market volume price) offer
    (let ((factor (expt 10 (decimals market))))
      (with-json-slots ((oid "orderID") (status "ordStatus") text)
          (post-raw-limit gate (not (plusp price)) (name market)
                          (multiple-value-bind (int dec)
                              (floor (abs (/ (floor price 1/2) 2)) factor)
                            (format nil "~D.~V,'0D"
                                    int (max 1 (decimals market)) (* 10 dec)))
                          (floor (* volume (if (minusp price) 1
                                               (/ price factor)))))
        (if (equal status "New") (change-class offer 'placed :oid oid)
            (unless (search "ParticipateDoNotInitiate" text)
              (warn "Failed placing: ~S~%~A" offer text)))))))

(defmethod cancel-offer ((gate bitmex-gate) (offer placed))
  (multiple-value-bind (ret err)
      (gate-request gate '(:delete "order") `(("orderID" . ,(oid offer))))
    (unless (string= err "Not Found")
      (string-case ((if ret (getjso "ordStatus" (car ret)) ""))
        ("Canceled") ("Filled")
        (t (warn "~A ~A" offer err))))))

;;;
;;; Comte Monte Carte
;;;

(defmethod bases-for ((supplicant supplicant) (market bitmex-market))
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

(defparameter *websocket-url* (format () "wss://~A/realtime" *base-domain*))

(defun make-websocket-handler (client topic market book
                               &aux (next-expected :info)
                                 (price-factor (expt 10 (decimals market))))
  (lambda (raw &aux (message (read-json raw)))
    (case next-expected
      (:info
       (if (string= (getjso "info" message)
                    "Welcome to the BitMEX Realtime API.")
           (setf next-expected :subscribe)
           (wsd:close-connection client)))
      (:subscribe
       (if (and (getjso "success" message)
                (string= (getjso "subscribe" message) topic))
           (setf next-expected :table)
           (wsd:close-connection client)))
      (:table
       (flet ((offer (side size price &aux (mp (* price price-factor)))
                (make-instance (string-case (side) ("Sell" 'ask) ("Buy" 'bid))
                               :market market :price mp
                               :volume (/ size price))))
         (with-json-slots (table action data) message
           (macrolet ((do-data ((&rest slots) &body body)
                        `(dolist (row data)
                           (with-json-slots ,slots row ,@body))))
             (flet ((build ()
                      (do-data (id side size price)
                        (setf (gethash id book)
                              (cons price (offer side size price))))))
               (cond
                 ((string/= table "orderBookL2")
                  (wsd:close-connection client))
                 ((zerop (hash-table-count book))
                  (when (string= action "partial") (build)))
                 (t (string-case (action)
                      ("update"
                       (do-data (id side size)
                         (let ((cons (gethash id book)))
                           (rplacd cons (offer side size (car cons))))))
                      ("insert"
                       (do-data (id side size price)
                         (setf (gethash id book)
                               (cons price (offer side size price)))))
                      ("delete" (do-data (id) (remhash id book)))
                      ("partial" (clrhash book) (build))
                      (t (wsd:close-connection client)
                         (error "unknown orderbook action: ~s" action)))))))))))))

(defun connect-websocket-client (topic)
  (wsd:start-connection
   (wsd:make-client (format () "~A?subscribe=~A" *websocket-url* topic))))

(defun make-orderbook-socket (market)
  (let* ((book (make-hash-table :test #'eq))
         (topic (format () "orderBookL2:~A" (name market)))
         (client (connect-websocket-client topic)))
    (wsd:on :message client (make-websocket-handler client topic market book))
    (values book client)))

(defclass streaming-market (bitmex-market) (socket book-table))
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

;;;
;;; Trollbox
;;;

(defun chats-since (channel &optional (since (timestamp- (now) 1 :minute)))
  (flet ((parse (chat)
           (with-json-slots (date id user message) chat
             (list (parse-timestamp *bitmex* date) id user message))))
    (alet (mapcar #'parse
                  (public-request
                   "chat" `(("channel" . ,channel) ("count" . 499)
                            ("startTime" .,(format-timestring
                                            () (timestamp- (now) 1 :minute)
                                            :timezone +utc-zone+)))))
      (if (not since) it
          (remove since it :test #'timestamp> :key #'car)))))

(defclass chats-fetcher (actor) ())

(defmethod christen ((fetcher chats-fetcher) (type (eql 'actor)))
  (format nil "cheat fetcher ~A" (channel fetcher)))

(defmethod perform ((fetcher chats-fetcher) &key)
  (with-slots (buffer delay channel)
      (first (slot-value fetcher 'delegates)) ; TODO : proper delegate
    (dolist (trade (aif (recv buffer) (chats-since channel it)
                        (chats-since channel)))
      ;; this unoptimization is starting to stink of prematurity ... .
      (send buffer trade))
    (sleep delay)))

(defclass chats-tracker (parent)
  ((channel :initarg :channel :reader channel)
   (delay  :initarg :delay  :initform 17)
   (buffer :initform (make-instance 'channel))
   (output :initform (make-instance 'channel))
   (chats :initform nil) fetcher))

(defmethod christen ((tracker chats-tracker) (type (eql 'actor)))
  (format nil "cheat tracker ~A" (channel tracker)))

(defmethod perform ((tracker chats-tracker) &key)
  (with-slots (buffer chats output delay) tracker
    (let ((last (car chats)))
      (select
        ((send buffer last)) ((send output chats))
        ((recv buffer next) (push next chats))
        (t (sleep delay))))))

(defmethod initialize-instance :after ((tracker chats-tracker) &key)
  (adopt tracker (setf (slot-value tracker 'fetcher)
                       (make-instance 'chats-fetcher :delegates `(,tracker)))))

;;;
;;; Bulk Placement and Cancellation
;;;

(defun offer-alist (offer)
  (with-slots (market price given taken) offer
    (let ((buyp (minusp price)) (factor (expt 10 (decimals market))))
      (multiple-value-bind (int dec)
          (floor (abs (/ (floor price 1/2) 2)) factor)
        `(("symbol" . ,(name market))
          ("price" . ,(format () "~D.~V,'0D" int
                              (max 1 (decimals market)) dec))
          ("orderQty" . ,(princ-to-string
                          (* (- (signum price))
                             (scaled-quantity (if buyp given taken)))))
          ("execInst" . "ParticipateDoNotInitiate"))))))

(defun post-bulk (gate &rest offers)
  (let ((orders (format () "[~{~A~^,~}]"
                        (reduce 'mapcar '(json:encode-json-alist-to-string
                                          offer-alist)
                                :from-end t :initial-value offers))))
    (awhen (gate-request gate '(:post "order/bulk") `(("orders" . ,orders)))
      (loop for data in it for status = (getjso "ordStatus" data)
         when (equal status "New") collect
           (with-json-slots
               (symbol side price (oid "orderID") (size "orderQty")) data
             (let ((market (find-market symbol :bitmex))
                   (aksp (string-equal side "Sell")))
               (make-instance 'placed :oid oid :market market
                              :volume (/ size price)
                              :price (* price (if aksp 1 -1)
                                        (expt 10 (decimals market))))))))))

(defun cancel-bulk (gate &rest offers)
  (gate-request gate '(:delete "order")
                `(("orderID" . ,(mapcar #'oid offers)))))

;;;
;;; Rate Limiting
;;;

(defun quote-fill-ratio (gate)
  (mapcar 'float
          (remove '() (mapcar (getjso "quoteFillRatioMavg7")
                              (gate-request
                               gate '(:get "user/quoteFillRatio") ())))))

;;;
;;; Introspection
;;;

(defun annualize-current-day (gate active-symbol &aux (condom 5) (solar 365.25))
    (awhen (gate-request gate '(:get "user/walletHistory")
                         `(("count" . ,condom) ("reverse" . t)))
      (assert (= (length it) condom))
      (with-json-slots ((balance "walletBalance")
                        (symbol "address")
                        (profit "amount"))
          (find "RealisedPNL" it :test #'string= :key (getjso "transactType"))
        (assert (string= symbol active-symbol))
        (aprog1 (expt (1+ (/ profit balance)) solar)
          (format t "~&~5,2@F%~%" it)))))
