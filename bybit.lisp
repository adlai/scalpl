(defpackage #:scalpl.bybit
  (:nicknames #:bybit)
  (:use #:cl #:base64 #:chanl #:anaphora #:local-time #:scalpl.util
        #:scalpl.actor #:scalpl.net #:scalpl.exchange #:scalpl.qd)
  (:export #:*bybit* #:bybit-gate #:bybit-market
           #:category #:recent-ticker
           #:linear-market #:inverse-market #:option-market))

(in-package #:scalpl.bybit)

;;; General Parameters
(defparameter *base-domain* "api.bybit.com")
(defparameter *base-url* (format () "https://~A" *base-domain*))

(defvar *bybit* (make-instance 'exchange :name :bybit :sensitivity 1))

(defun load-swagger-specification (path)
  (with-open-file (file path) (read-json file)))

;; (defparameter *swagger* ; TODO: use correct UIOP incantation here
;;   (load-swagger-specification
;;    #P"./promises/bybit/api-connectors/swagger.json"))

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

(defun v5-url (path)
  (concatenate 'string *base-url* "/v5/" path))

(defun bybit-request (path &rest args &aux (now (now)))
  (multiple-value-bind (body status headers uri stream close)
      ;; (if (and (slot-boundp *bybit* 'stream)
      ;;        (open-stream-p (slot-value *bybit* 'stream)))
      ;;   (apply #'http-request (concatenate 'string *base-url* path)
      ;;          :stream (slot-value *bybit* 'stream) :close () :keep-alive t
      ;;          args)
      ;;   )
      ;; (if (member :additional-headers args) (error "pay"))
      (apply #'http-request (v5-url path) args)
    ;; from the new and improved documentation, about the new and
    ;; simplified application simplified programming simplified IN YOUR FACE,
    ;;  -- more words that only delay help --
    ;; To assist in diagnosing advanced network problems, you may
    ;; consider adding cdn-request-id to your request headers. Its
    ;; value should be unique for each request.
    (unless close (setf (slot-value *bybit* 'stream) stream))
    (case status
      (200 (with-json-slots
               (result (code "retCode") (msg "retMsg")) (decode-json body)
             (values result code msg)))
      ((403) (warn "~&~A HIT RATE LIMIT~%URI: ~A~%~A" now uri headers)
       (values () status body))
      (t (warn "~&~A ByBit ~D~@[ [~A]~]~%URI: ~A" now status body uri)
       (values () status body)))))

(defun public-request (path parameters)
  (bybit-request path
                 ;; :additional-headers ; why do any thing youniquely?
                 ;; '("cdn-request-id" . "SAVE DAVE PAVE WAVE")
                 :parameters (loop for (key . value) in parameters
                                   collect (cons (string-downcase (string key))
                                                 (princ-to-string value)))))

;; (defmethod format-timestring (timestamp)
;;   (format () "~D" (+ (timestamp-millisecond (now))
;;                      (* 1000 (timestamp-to-unix (now))))))

(defun auth-request (verb path key signer &optional params)
  (let* ((window "9999")
         (time (format () "~D" (+ (timestamp-millisecond (now))
                                  (* 1000 (timestamp-to-unix (now))))))
         (payload (concatenate 'string time key window
                               (if (eq verb :get)
                                   (urlencode-params params)
                                   (json:encode-json-alist-to-string params)))))
    (apply 'bybit-request path :method verb :additional-headers
           `(("X-BAPI-API-KEY" . ,key)
             ("X-BAPI-SIGN" . ,(funcall signer payload))
             ("X-BAPI-SIGN-TYPE" . "2")
             ;; ("BLIND-STARVING-FISH" . "cdn-request-id")
             ("X-BAPI-TIMESTAMP" . ,time)
             ("X-BAPI-RECV-WINDOW" . ,window))
           (case verb
             (:get `(:parameters ,params))
             (:post `(:content ,(json:encode-json-alist-to-string params)
                      :content-type "application/json"))))))

(defclass bybit-market (market)
  ((exchange :initform *bybit* :allocation :class)
   (category :initarg :category :allocation :class :reader category)
   (fee :initarg :fee :reader fee)
   (epsilon :initarg :epsilon :reader epsilon)
   (delivery :initarg :delivery :reader delivery)
   (recent-ticker :accessor recent-ticker)))

(defclass linear-market (bybit-market)
  ((category :initform "linear" :allocation :class)))
(defclass inverse-market (bybit-market)
  ((category :initform "inverse" :allocation :class)))
(defclass option-market (bybit-market)
  ((category :initform "option" :allocation :class)
   (delivery-date :reader delivery-date) (strike :reader strike)))
(defclass spot-market (bybit-market)
  ((category :initform "spot" :allocation :class)))

(defun live-market-p (market)
  (or (not (slot-boundp market 'delivery))
    (timestamp> (delivery market) (now))))

(defmethod markets :around ((exchange (eql *bybit*)))
  (remove-if (lambda (market)
               (typecase market
                 (option-market (not (live-market-p market)))
                 ;; for some strange and irrelevant reason,
                 ;; the following excludes BTCUSDT ... who cares!?
                 (linear-market (not (live-market-p market)))))
             (call-next-method)))

;; (defmethod assets :around ((exchange (eql *bybit*)))
;;   (remove-if (lambda (asset)
;;                (typecase (market asset) ; no such link
;;                  (option-market (not (live-market-p market)))))
;;              (call-next-method)))

(defmethod category ((market tracked-market))
  (category (slot-value market 'scalpl.exchange::%market)))

(defmethod shared-initialize :after ((market option-market) slot-names &key)
  (with-slots (name strike delivery-date) market
    (let ((parts (cdr (split-sequence #\- name))))
      (setf delivery-date (intern (pop parts) :keyword)
            strike (parse-integer (pop parts))))))

(defun get-info (&aux assets markets)
  (flet ((derivative-parser (class)
           (lambda (instrument)
             (with-json-slots ((name "symbol") (delivery "deliveryTime")
                               (coin "baseCoin") (price-filter "priceFilter")
                               (lot-filter "lotSizeFilter") status)
                              instrument
               (flet ((asset (side decimals
                              &aux (name (format () "~A~A" side name)))
                        (aprog1 (make-instance 'asset :name name
                                                      :decimals decimals)
                          (push it assets))))
                 (when (and (string= status "Trading") (string= coin "BTC"))
                   (let ((tick (parse-float (getjso "tickSize" price-filter)))
                         (epsilon (parse-float (getjso "qtyStep" lot-filter))))
                     (push (make-instance
                            class :name name :fee 0.1 :tick tick :epsilon epsilon
                            :decimals (- (floor (log (abs tick) 10)))
                            :primary (asset #\l 8) :counter (asset #\s 4)
                            :delivery (unix-to-timestamp
                                       (floor (parse-integer delivery) 1000)))
                           markets)))))))
         (spot-parser ()
           (lambda (instrument)
             (with-json-slots
                 ((name "symbol") (delivery "deliveryTime")
                  (primary "baseCoin") (counter "quoteCoin")
                  (price "priceFilter") (lot "lotSizeFilter")
                  status)
                 instrument
               (flet ((asset (name decimals)
                        (or (find name assets :key #'name :test #'string=)
                            (aprog1 (make-instance 'asset :name name
                                                          :decimals decimals)
                              (push it assets))))
                      (precision (multiplier)
                        (- (floor (log (abs multiplier) 10)))))
                 (when (and (string= status "Trading")
                            ;; (or (string= primary "BTC")
                            ;;     (string= counter "BTC"))
			    )
                   (let ((tick (parse-float (getjso "tickSize" price)))
                         (base (parse-float (getjso "basePrecision" lot)
					    :type 'rational))
                         (quote (parse-float (getjso "quotePrecision" lot)
					     :type 'rational))
                         (epsilon (parse-float (getjso "minOrderAmt" lot)
					       :type 'rational)))
                     (push (make-instance
                            'spot-market :name name :fee 0.1 :tick tick
                                         :epsilon epsilon
                                         :decimals (precision tick)
                                         :primary (asset primary
                                                         (precision base))
                                         :counter (asset counter
                                                         (precision quote)))
                           markets))))))))
    (with-json-slots (list)
        (public-request "market/instruments-info"
                        `(("category" . "spot") ("limit" . 1000)))
      (mapcar (spot-parser) list))
    (dolist (category '((inverse-market "inverse")
                        (linear-market "linear")
                        (option-market "option"))
                      (values markets assets))
      (with-json-slots (list)
          (public-request "market/instruments-info"
                          `(("category" . ,(cadr category))
                            ("limit" . "1000") ("baseCoin" . "BTC")))
        (mapcar (derivative-parser (car category)) list)))))

(defun fetch-new-options (&aux assets markets)
  (awhen (public-request "market/instruments-info"
                         `(("category" . "option")
                           ("limit" . "1000") ("baseCoin" . "BTC")))
    (with-json-slots (list) it
      (dolist (instrument list (values assets markets))
        (with-json-slots
            ((name "symbol") (delivery "deliveryTime")
             (coin "baseCoin") (price-filter "priceFilter")
             (lot-filter "lotSizeFilter") status)
            instrument
          (flet ((asset (side decimals
                         &aux (name (format () "~A~A" side name)))
                   (aprog1 (make-instance 'asset :name name
                                                 :exchange *bybit*
                                                 :decimals decimals)
                     (push it assets))))
            (when (and (string= status "Trading") (string= coin "BTC")
                       (not (find-market name *bybit*)))
              (let ((tick (parse-float (getjso "tickSize" price-filter)))
                    (epsilon (parse-float (getjso "qtyStep" lot-filter))))
                (push (make-instance
                       'option-market :name name :fee 0.1 :tick tick
                       :epsilon epsilon
                       :decimals (- (floor (log (abs tick) 10)))
                       :primary (asset #\l 8) :counter (asset #\s 4)
                       :delivery (unix-to-timestamp
                                  (floor (parse-integer delivery) 1000)))
                      markets)))))))))

(defmethod fetch-exchange-data ((exchange (eql *bybit*)))
  (with-slots (markets assets) exchange
    (setf (values markets assets) (get-info))))

;; (multiple-value-bind (empty launched) (bybit::fetch-new-options)
;;   (with-slots (assets markets) *bybit*
;;     (dolist (asset empty)
;;       (push asset assets))
;;     (dolist (market launched)
;;       (push market markets)))
;;   (mapcar 'name launched))

(defclass bybit-gate (gate)
  ((exchange :initform *bybit*)
   (websocket-cache :initform (make-hash-table :test #'equalp)
                    :reader websocket-cache)))

(defmethod gate-post ((gate (eql *bybit*)) key secret request)
  (destructuring-bind ((verb method) . parameters) request
    (multiple-value-bind (ret code status)
        (auth-request verb method key secret parameters)
      `(,ret ,(awhen1 (acase code
                        ((0)) ; C-style, no error
                        ((10001))       ; THANK YOU FOR NOT SPOOFING
                        ((20001 30032 30037 170213)) ; order already filled
                        ;; ((170130))      ; FIXME minimum order size
                        ((30076) it)          ; failures to replace
                        (t status))     ; by default, echo the error text
                (typecase it
                  ((eql 170213) "let's not spam this")
                  (string #|(break)|# (warn "#~D ~A" code it))))))))

(defmethod shared-initialize ((gate bybit-gate) names &key pubkey secret)
  (multiple-value-call #'call-next-method gate names
                       (mvwrap pubkey make-key) (mvwrap secret make-signer)))

;;;
;;; Public Data API
;;;

(defmethod get-book ((market spot-market) &key (count 5))
  (with-slots (name decimals) market
    (with-json-slots
        (b a) (public-request "market/orderbook"
                              `(("symbol" . ,name)
                                ("limit" . ,(format () "~D" count))
                                ("category" . "spot")))
      (flet ((side (type list)
               (loop for (price size) in list
                     collect (make-instance
                              type :market market
                              :volume (parse-float size :type 'rational)
                              :price (parse-price price decimals)))))
        (values (side 'ask a) (side 'bid b))))))

(defmethod get-book ((market linear-market) &key (count 5))
  (with-slots (name decimals) market
    (with-json-slots
        (b a) (public-request "market/orderbook"
                              `(("symbol" . ,name)
                                ("limit" . ,(format () "~D" count))
                                ("category" . "linear")))
      (flet ((side (type list)
               (loop for (price size) in list
                     collect (make-instance
                              type :market market
                              :volume (parse-float size :type 'rational)
                              :price (parse-price price decimals)))))
        (values (side 'ask a) (side 'bid b))))))

(defmethod get-book ((market inverse-market) &key (count 5))
  (with-slots (name decimals) market
    (with-json-slots
        (b a) (public-request "market/orderbook"
                                 `(("symbol" . ,name)
                                   ("limit" . ,(format () "~D" count))
                                   ("category" . "inverse")))
      (flet ((side (type list)
               (loop for (price size) in list
                     collect (make-instance
                              type :market market
                                   :volume (/ (parse-float size :type 'rational)
                                              (parse-float price :type 'rational))
                                   :price (parse-price price decimals)))))
        (values (side 'ask a) (nreverse (side 'bid b)))))))

(defmethod get-book ((market option-market) &key count)
  (with-slots (name decimals) market
    (with-json-slots
        (b a) (public-request "market/orderbook"
                              `(("symbol" . ,name)
                                ("category" . "option") ("limit" . "25")))
      (flet ((side (type list)
               (loop for (price size) in list with remain = count
                     until (when count (minusp (decf remain)))
                     collect (make-instance
                              type :market market
                              :volume (parse-float size :type 'rational)
                              :price (parse-integer price)))))
        (values (side 'ask a) (side 'bid b))))))

(defmethod trades-since ((market spot-market) &optional from)
  (with-slots (name) market
    (flet ((parse (trade)
             (with-json-slots ((id "execId") side time size price) trade
               (let* ((price (parse-float price))
                      (cost (parse-float size))
                      (volume (/ cost price)))
                 (make-instance 'trade :market market :direction side
                                       :txid (parse-integer id)
                                       :timestamp (unix-to-timestamp
                                                   (floor (parse-integer time) 1000))
                                       :cost cost :volume volume :price price)))))
      (awhen (public-request "market/recent-trade"
                             `(("symbol" . ,name) ("category" . "spot")))
        (let ((trades (nreverse (mapcar #'parse (getjso "list" it)))))
          (if (null from) trades
              (remove (timestamp from) trades
                      :key #'timestamp :test #'timestamp>=)))))))

(defmethod trades-since ((market inverse-market) &optional from)
  (with-slots (name) market
    (flet ((parse (trade)
             (with-json-slots ((id "execId") side time size price) trade
               (let* ((price (parse-float price))
                      (cost (parse-float size))
                      (volume (/ cost price)))
                 (make-instance 'trade :market market :direction side :txid id
                                       :timestamp (unix-to-timestamp
                                                   (floor (parse-integer time) 1000))
                                       :cost cost :volume volume :price price)))))
      (awhen (public-request "market/recent-trade"
                             `(("symbol" . ,name) ("category" . "inverse")
                               ,@(when from
                                   `(("from"
                                      .,(princ-to-string (txid from)))))))
        (if from (cdr (mapcar #'parse (getjso "list" it)))
            (nreverse (mapcar #'parse (getjso "list" it))))))))

(defmethod trades-since ((market linear-market) &optional from)
  (with-slots (name) market
    (flet ((parse (trade)
             (with-json-slots ((id "execId") side time size price) trade
               (let* ((price (parse-float price))
                      (volume (parse-float size)) (cost (* volume price)))
                 (make-instance 'trade :market market :direction side :txid id
                                       :timestamp (unix-to-timestamp
                                                   (floor (parse-integer time) 1000))
                                       :cost cost :volume volume :price price)))))
      (awhen (public-request "market/recent-trade"
                             `(("symbol" . ,name) ("category" . "linear")
                               ,@(when from
                                   `(("from"
                                      .,(princ-to-string (txid from)))))))
        (if from (cdr (mapcar #'parse (getjso "list" it)))
            (nreverse (mapcar #'parse (getjso "list" it))))))))

(defmethod trades-since ((market option-market) &optional from)
  (flet ((parse (trade)
           (with-json-slots ((id "execId") side time size price) trade
             (let ((price (parse-float price))
                   (volume (parse-float size)))
               (make-instance 'trade :market market :direction side :txid id
                                     :timestamp (unix-to-timestamp
                                                 (floor (parse-integer time) 1000))
                                     :volume volume :price price
                                     :cost (* price volume))))))
    (awhen (public-request "market/recent-trade"
                           `(("symbol" . ,(name market)) ("category" . "option")
                             ,@(when from
                                 `(("from"
                                    .,(princ-to-string (txid from)))))))
      (if from (cdr (mapcar #'parse (getjso "list" it)))
          (nreverse (mapcar #'parse (getjso "list" it)))))))

(deftype maybe-latest (minutes) `(or null (mod ,minutes)))
(deftype maybe-timestamp () '(or null timestamp))

;;;
;;; Private Data API
;;;

(defmethod market-placed ((gate bybit-gate) (market tracked-market))
  (market-placed gate (slot-value market 'scalpl.exchange::%market)))

(defmethod market-placed ((gate bybit-gate) (market bybit-market) &aux offers)
  (let ((url "order/realtime"))
    (loop
      with cursor do
        (multiple-value-bind (result error)
            (gate-request gate `(:get ,url)
                          `(("symbol" . ,(name market))
                            ("category" . ,(category market))
                            ,@(if cursor `(("cursor" . ,(url-decode cursor))))))
          (unless error
            (if (string= cursor (getjso "nextPageCursor" result)) (return)
                (setf cursor (getjso "nextPageCursor" result)))
            (dolist (json (getjso "list" result))
              (with-json-slots (side price (oid "orderId") qty) json
                (let* ((price (parse-price price (decimals market)))
                       (volume (if (string= (category market)
                                            "inverse")
                                   (/ (parse-float qty :type 'rational)
                                      (expt 10 (- (decimals market)))
                                      price) ; yuck
                                   (parse-float qty :type 'rational))))
                  (pushnew (make-instance
                            'offered :oid oid :market market :volume volume
                            :price (if (string= side "Sell")
                                       price (- price)))
                           offers :test #'string= :key 'oid))))))
      until (or (null cursor) (string= cursor "")))
    offers))

(defmethod placed-offers ((gate bybit-gate) &optional market)
  (if market (market-placed gate market)
      (loop for market in (markets *bybit*)
            nconc (market-placed gate market))))

;; (dolist (market (delete-duplicates (mapcar #'market *)))
;;   (format t "~&~A : ~3D offers~%" (name market)
;;           (count market * :key #'market)))

;;; define-enumerable FERFUXXAKE!

(defun all-positions (gate &rest kind &aux positions)
  (flet ((request (category &optional coin)
           (loop
             with cursor do
               (awhen (gate-request gate '(:get "position/list")
                                    `(("category" . ,category)
                                      ,@(when coin `(("settleCoin" . ,coin)))
                                      ,@(if cursor `(("cursor" . ,cursor)))))
                 (dolist (position (getjso "list" it))
                   (push position positions))
                 (setf cursor (getjso "nextPageCursor" it)))
             until (or (null cursor) (string= cursor "")))))
    (when (or (null kind) (find "option" kind :test #'string=))
      (request "option"))
    (when (or (null kind)
              (find "linear" kind :test #'string=)
              (find "USDT" kind :test #'string=))
      (request "linear" "USDT"))
    (when (or (null kind)
              (find "linear" kind :test #'string=)
              (find "USDC" kind :test #'string=))
      (request "linear" "USDC"))
    (when (or (null kind) (find "inverse" kind :test #'string=))
      (request "inverse" "BTC")))
  positions)                            ; apply 'values ?

(defgeneric account-position (gate market)
  (:method ((gate gate) (market spot-market)))
  (:method ((gate gate) (market bybit-market))
    (with-slots (name category primary counter websocket-cache) market
      (awhen (or (and (slot-exists-p market 'websocket-cache)
                      (slot-boundp market 'websocket-cache)
                      (gethash (cons "position" name) websocket-cache))
                 (awhen (gate-request gate '(:get "position/list")
                                      `(("symbol" . ,name)
                                        ("category" . ,category)))
                   (first (getjso "list" it))))
        (with-json-slots
            ((entry "avgPrice") symbol size side (value "positionValue")) it
          (unless (or (string= side "") (string= side "None"))
            (with-slots (primary counter) market
              (values `(,(cons-mp* market (* (parse-float entry)
                                             (if (string= side "Buy") -1 1)))
                        ,(cons-aq* primary
                                   (* (if (string= category "inverse")
                                          (parse-float value)
                                          (parse-float size))
                                      (if (string= side "Buy") 1 -1)))
                        ,(cons-aq* counter
                                   (* (if (string= category "inverse")
                                          (parse-integer size)
                                          (parse-float value))
                                      (if (string= side "Buy") -1 1))))
                      it))))))))

(defmethod account-balances ((gate bybit-gate) &aux balances)
  ;; (declare (optimize debug))
  (awhen (gate-request gate '(:get "account/wallet-balance")
                       '(("accountType" . "unified")))
    (with-json-slots
        ((equity "totalEquity") (imr "accountIMRate") coin)
      (first (getjso "list" it))
      ;; (break)
      (if (string= imr "0")
          (dolist (json coin)
            ;; (format t "~&~S~%" json)
            ;; (break)
            (with-json-slots ((symbol "coin") equity) json
              (push (cons-aq* (find-asset symbol *bybit*)
                              (parse-float equity))
                    balances)))
          (dolist (market (remove "linear"
                                  (remove "BTC" (markets *bybit*)
                                          :test-not #'search :key #'name)
                                  :test-not #'string= :key #'category))
            (with-slots (name primary counter category) market
              (when (string= "linear" category)
                (with-json-slots
                    ((mark "markPrice"))
                    (car (getjso "list"
                                 (public-request "market/tickers"
                                                 `(("category" . "linear")
                                                   ("symbol" . ,name)))))
                  (multiple-value-bind (position json)
                      (account-position gate market)
                    (if position
                        (let ((fund (* (parse-float equity)
                                       (with-json-slots (leverage) json
                                         (if (string= leverage "") 5
                                             (parse-float leverage))))))
                          (push (aq+ (cons-aq* primary (/ fund (parse-float mark)))
                                     (second position))
                                balances)
                          (push (aq+ (cons-aq* counter fund)
                                     (third position))
                                balances))
                        (let ((fund (parse-float equity)))
                          (push (cons-aq* primary (/ fund (parse-float mark)))
                                balances)
                          (push (cons-aq* counter fund)
                                balances)))))))))))
  (awhen (gate-request gate '(:get "account/wallet-balance")
                       '(("accountType" . "contract")))
    (with-json-slots (coin) (first (getjso "list" it))
      (awhen (find "BTC" coin :test #'string= :key (getjso "coin"))
        (let ((equity (parse-float (getjso "equity" it))))
          (dolist (market (remove "inverse"
                                  (remove "BTC" (markets *bybit*)
                                          :test-not #'search :key #'name)
                                  :test-not #'string= :key #'category))
            (with-slots (name primary counter category) market
              (when (string= "inverse" category)
                (with-json-slots
                    ((mark "markPrice"))
                    (car (getjso "list" (public-request "market/tickers"
                                                        `(("category" . "inverse")
                                                          ("symbol" . ,name)))))
                  (multiple-value-bind (position json)
                      (account-position gate market)
                    (if position
                        (let ((fund (* equity
                                       (alet (getjso "leverage" json)
                                         (if (string= it "") 1
                                             (parse-float it))))))
                          (push (aq+ (cons-aq* primary fund)
                                     (second position))
                                balances)
                          (push (aq+ (cons-aq* counter
                                               (* fund (parse-float mark)))
                                     (third position))
                                balances))
                        (progn
                          (push (cons-aq* primary equity) balances)
                          (push (cons-aq* counter (* equity (parse-float mark)))
                                balances))))))))))))
  balances)

;;; This horror can be avoided via the actor-delegate mechanism.
(defmethod market-fee ((gate bybit-gate) (market bybit-market))
  (awhen (gate-request gate '(:get "account/fee-rate")
                       `(("category" . ,(category market))
                         ("symbol" . ,(name market))))
    (* 100 (parse-float (getjso "makerFeeRate" (car (getjso "list" it)))))))

(defmethod parse-timestamp ((exchange (eql *bybit*)) (timestamp string))
  (multiple-value-bind (unix milliseconds)
      (floor (parse-integer timestamp) 1000)
    (unix-to-timestamp unix :nsec (* 1000000 milliseconds))))

(defgeneric build-execution (market &rest args)
  (:method ((market tracked-market) &rest args)
    (apply 'build-execution
           (slot-value market 'scalpl.exchange::%market) args))
  (:method ((market bybit-market) &rest args)
    (if (string= "inverse" (category market))
        (destructuring-bind
            (oid txid side cost price volume fee time) args
          (make-instance 'execution :direction side :market market
                                    :txid txid :price price :cost cost
                                    :net-cost cost :volume (abs volume)
                                    :net-volume (abs (+ volume fee))
                                    :timestamp time :oid oid))
        (destructuring-bind
            (oid txid side cost price volume fee time) args
          (make-instance 'execution :direction side :market market
                                    :txid txid :price price
                                    :cost (abs volume)
                                    :net-cost (abs (+ volume fee))
                                    :volume cost :net-volume cost
                                    :timestamp time :oid oid)))))

(defun parse-execution (raw)
  (with-json-slots ((oid "orderId") (txid "execId") (amt "execQty")
                    symbol side (price "execPrice") (time "execTime")
                    (execost "execValue") (exefee "execFee"))
      raw
    (unless (zerop (length side))
      (flet ((adjust (value) (parse-float value)))
        (build-execution (find-market symbol :bybit)
                         oid txid side (adjust amt) (adjust price)
                         (adjust execost) (adjust exefee)
                         (parse-timestamp *bybit* time))))))

(defun raw-executions (gate &key kind pair from count)
  (macrolet ((params (&body body)
               `(append ,@(loop for (val key exp) in body
                             collect `(when ,val `((,,key . ,,exp)))))))
    (awhen (loop with cursor and executions do
                 (awhen (gate-request
                         gate '(:get "execution/list")
                         (params (cursor "cursor" cursor)
                                 (kind "category" kind)
                                 (pair "symbol" pair) (count "limit" count)
                                 (from "startTime"
                                       (princ-to-string
                                        (+ (* 1000 (timestamp-to-unix from))
                                           (floor (nsec-of from) 1000000))))))
                   (setf executions (append executions (getjso "list" it)))
                   (setf cursor (getjso "nextPageCursor" it)))
                 until (or (null cursor) (string= cursor ""))
                 finally (return executions))
      (sort it #'string< :key (getjso "execTime")))))

(defmethod execution-since ((gate bybit-gate) (market bybit-market) since)
  (awhen (raw-executions gate :kind (category market)
                              :pair (name market)
                              :from (if since (timestamp since)
                                        (timestamp- (now) 3 :hour)))
    (mapcar #'parse-execution
            (if (null since) it
                (let ((found (position (txid since) it
                                       :test #'string=
                                       :key (getjso "execId"))))
                  (if (null found) it
                      ;; code simply smells wrong
                      (subseq it (1+ found))))))))

(defgeneric post-limit (gate market price size &optional reduce-only)
  (:method (gate (market tracked-market) price size &optional reduce-only)
    (post-limit gate (slot-value market 'scalpl.exchange::%market)
                price size reduce-only))
  (:method ((gate bybit-gate) (market bybit-market) price size
            &optional reduce-only &aux (now (now)))
    (let ((link-id (format () "~12,'0X-~23,'0X"
                           (+ (timestamp-millisecond now)
                              (* 1000 (timestamp-to-unix now)))
                           (random (expt 16 23)))))
      (gate-request gate '(:post "order/create")
                    (with-slots (name category epsilon) market
                      `(("orderType" . "Limit") ("category" . ,category)
                        ("symbol" . ,name) ("price" . ,price)
                        ("side" . ,(if (plusp size) "Buy" "Sell"))
                        ("qty" . ,(format () "~V$"
                                          (abs (floor (log epsilon 10)))
                                          (abs size)))
                        ("timeInForce" . "PostOnly") ("orderLinkId" . ,link-id)
                        ("reduceOnly" . ,(if reduce-only "true" "false"))))))))

(defun amend-limit (gate market oid price size) ; &optional considered harmful?
  (gate-request gate '(:post "order/amend")
                (with-slots (name category epsilon) market
                  `(("category" . ,category) ("symbol" . ,name)
                    ("orderId" .,oid) ("price" . ,price)
                    ("qty" . ,(format () "~V$"
                                      (abs (floor (log epsilon 10)))
                                      (abs size)))))))

(defclass placed (offered) ((unique-request-identifier :initarg :urid)))

(defmethod post-offer ((gate bybit-gate) (offer offer))
  (with-slots (market volume price) offer
    (let ((factor (expt 10 (decimals market))))
      (multiple-value-bind (json complaint)
          (post-limit gate market
                      (multiple-value-bind (int dec)
                          (floor (abs (/ (floor price 1/2) 2)) factor)
                        (format nil "~D.~V,'0D"
                                int (max 1 (decimals market)) dec))
                      (if (string= (category market) "inverse")
                          (floor (* volume (if (minusp price) 1
                                               (- (/ price factor)))))
                          (/ volume (if (minusp price)
                                        (/ (abs price) factor)
                                        -1))))
        (with-json-slots ((oid "orderId") (urid "orderLinkId")) json
          (if (and json oid urid)
              (change-class offer 'placed :oid oid :urid urid)
              ;; TODO dispatch upon complaint before warning?
              (warn "Failed placing: ~S~%~A" offer complaint)
              ))))))

(defmethod amend-offer ((gate bybit-gate) (old offered) (new offer))
  (with-slots (market oid) old
    (with-slots (volume price) new
      (let* ((factor (expt 10 (decimals market))))
        (multiple-value-bind (json complaint)
            (amend-limit gate market oid
                         (unless (= (realpart price) (realpart (price old)))
                           (multiple-value-bind (int dec)
                               (floor (abs (/ (floor price 1/2) 2)) factor)
                             (format () "~D.~V,'0D" int
                                     (max 1 (decimals market)) dec)))
                         (if (string= (category market) "inverse")
                             (floor (* volume (if (minusp price) 1
                                                  (/ price factor))))
                             (/ volume (if (minusp price)
                                           (/ (abs price) factor)
                                           -1))))
          (if complaint
              (string-case (complaint)
                ("The order remains unchanged as the parameters entered match the existing ones.")
                ("Order does not exist.")
                ("The modified order quantity must be equal to or greater than the filled quantity."
                 (cancel-offer gate old))
                (t (warn (format () "~A~& was not modified to ~A" old new))))
              (with-json-slots ((returned-oid "orderId")) json
                (if (string= returned-oid oid) ; how can this test fail?
                    (reinitialize-instance     ; not rhetoric, enumerate
                     old :volume volume :price price))))))))) ; or don't

;;; FIXME:
;;; The PostOnly option is, quite infuriatingly, only respected after
;;; the Internet-facing endpoint has already confirmed the offer; thus,
;;; there is a need for a followup that confirms offer placement, at least
;;; half a second after the initial claim that it's been placed.

(defmethod cancel-offer ((gate bybit-gate) (offer offered))
  (multiple-value-bind (ret err)
      (gate-request gate '(:post "order/cancel")
                    (with-slots (name category) (market offer)
                      `(("symbol" . ,name) ("orderId" . ,(oid offer))
                        ("category" . ,category))))
    (or ret (and (null err) (string= (oid offer) (getjso "orderId" ret))))))

(defun ope-amend (ope old new)
  (amend-offer (slot-reduce ope supplicant gate) old new)
  (send (slot-reduce ope supplicant control) '(:timestamp)))

(defclass bybit-prioritizer (prioritizer) ())

(defmethod prioriteaze ((ope bybit-prioritizer) target placed
                        &aux to-add (excess placed))
  (flet ((frob (add pop &aux (max (max (length add) (length pop))))
           (with-slots (expt) ope
             (let* ((n (expt (random (expt max (/ expt))) expt))
                    (add (nth (floor n) add))
                    (pop (nth (floor n) pop)))
               (if (and add pop)
                   (ope-amend ope pop add)
                   (if add (ope-place ope add) (ope-cancel ope pop)))))))
    (aif (dolist (new target (sort to-add #'< :key #'price))
           (aif (aand (find (price new) excess :key #'price :test #'=)
                      (not (sufficiently-different? new it)) it)
                (setf excess (remove it excess)) (push new to-add)))
         (frob it excess)   ; which of these is worst?
         (if excess (frob () excess)  ; choose the lesser weevil
             (and target placed (= (length target) (length placed))
                  (loop for new in target and old in placed
                     when (sufficiently-different? new old)
                     collect new into news and collect old into olds
                     finally (when news (frob news olds))))))))

(defun wipe-offers (supplicant)
  (multiple-value-bind (ret err)
      (with-slots (gate market) supplicant
        (gate-request gate '(:post "order/cancel-all")
                      `(("category" . ,(category market))
                        ("symbol" . ,(name market)))))
    (values (or err ret)
            (unless err (length (getjso "list" ret))))))

;;;
;;; General Introspection
;;;

;; (defun daily-returns (gate &optional count)
;;   ;; (mapcar 'funcall (mapcar #'getjso '("coin" "address")))
;;   (awhen (gate-request gate '(:get "/v2/private/wallet/fund/records")
;;                        (when count `(("limit" . ,(princ-to-string count)))))
;;     (mapcar (lambda (json)
;;               (with-json-slots
;;                   ((balance "wallet_balance") (date "exec_time")
;;                 amount coin address)
;;                json
;;              (let ((asset (find-asset (concatenate 'string
;;                                                    coin "-" address)
;;                                       *bybit*)))
;;                   (list (parse-timestring date)
;;                      (cons-aq* asset (parse-float balance))
;;                      (cons-aq* asset (parse-float amount))))))
;;             (remove "RealisedPNL" (getjso "data" it)
;;                     :test-not #'string= :key (getjso "type")))))

;; (defun fetch-pages (gate request options &optional (count 50)
;;                     (data-key "data") &aux (number 1))
;;   (flet ((fetch-page ()
;;            (gate-request gate request
;;                          (acons "page" (format () "~D" number) options))))
;;     (loop for page = (fetch-page) when page append (getjso data-key page)
;;        and if (aif (getjso "last_page" page)
;;                    (< (getjso "current_page" page) it)
;;                    (when count (< number count)))
;;        do (incf number) else do (loop-finish))))

;; (defun recent-closes (gate &optional (count 25) &aux (sum 0s0))
;;   (check-type count (integer 1 2501))
;;   (awhen (fetch-pages gate '(:get "/v2/private/trade/closed-pnl/list")
;;                       `(("symbol" . "BTCUSD") .
;;                         ,(when count
;;                         `(("limit"
;;                            . ,(princ-to-string count))))) ; lol!
;;                       (when count (ceiling count 50)))
;;     (values (mapcar (lambda (json)
;;                       (with-json-slots
;;                           ((price "order_price") side qty
;;                            (unix "created_at") (pnl "closed_pnl")) json
;;                         (incf sum pnl)
;;                         (list (unix-to-timestamp unix) side qty price pnl)))
;;                     it)
;;             sum)))

;; (defun liquidity-contribution-score (gate &optional (symbol "BTCUSD"))
;;   (awhen (gate-request gate '(:get "/v2/private/account/lcp")
;;                        `(("symbol" .,symbol)))
;;     (mapcar (lambda (daily-metrics)
;;               (with-json-slots ((theirs "platform_ratio") date
;;                                 (ours       "self_ratio") score)
;;                   daily-metrics
;;                 (list (parse-timestring date) score ours theirs)))
;;             (getjso "lcp_list" it))))

;; (defmethod describe-account
;;     ((supplicant t) (exchange (eql *bybit*)) (*standard-output* t))
;;  (with-slots (gate) supplicant
;;     ;;; there's a pattern here, and it belongs in the fucking symbol-plists:
;;    (format t "~&~%@#$%& Dump Of Recent Closes &%$#@~%")
;;    (multiple-value-bind (rhos total)	; blessed are l'hoi pollois, ne?
;;        (apply #'recent-closes gate
;;            (and (eq *standard-output* *debug-io*) '(2500)))
;;      (dolist (row rhos (values total (log (1+ total))))
;;        (destructuring-bind #1=(second action comment quanta tide) row
;;                            (format t "~&~10A ~4A ~5D ~1,6$ ~8$~%" . #1#))))
;;    (format t "~&~%@#$%& Dump Of Recent Daily Earnings &%$#@~%")
;;    (dolist (row (daily-returns gate))
;;      (destructuring-bind #2=(day coins flux) row
;;        (format t "~&~10A ~10A ~11@A~%" . #2#)))
;;    (format t "~&~%@#$%& Dump Of Recent Liquidity Scores &%$#@~%")
;;    (dolist (day (liquidity-contribution-score gate))
;;      (format t "~&~{~D ~F ~F ~F~}~%" day))
;;     ;; can you smell that pattern? ... symbol-plists are THE WORST THING EVER!
;;     ))


;;;
;;; What's this one whore's Morty doing in a three-card shinachop?
;;;

(defmethod bases-for ((supplicant supplicant) (market bybit-market))
  (with-slots (gate) supplicant
    (aif (account-position gate market)
         (let ((entry (realpart (first it)))
               (size (abs (quantity (third it)))))
           (flet ((foolish (basis &aux (price (realpart (car basis))))
                    (if (= (signum price) (signum entry)) (> price entry)
                        ;; N.B.: this logic allows bleeding to reduce chance
                        ;; of liquidation; ...both hardcoded and ineffective
                        (and (< (isqrt size) (quantity (second basis)))
                             (< (isqrt size) (quantity (third basis))))
                        )))
             (multiple-value-bind (primary counter) (call-next-method)
               (if (plusp entry)
                   (values ()
                           (or (remove-if #'foolish counter)
                               `((,(car it)
                                  ,(cons-aq (asset (caddr it))
                                            (abs (quantity (caddr it))))
                                  ,(cons-aq (asset (cadr it))
                                            (abs (quantity (cadr it))))))))
                   (values (or (remove-if #'foolish primary) (list it)) ()))
               ;; (values (remove-if #'foolish primary)
               ;;         (remove-if #'foolish counter))
               )))
         (call-next-method))))

;;; This one belongs in the Ten Decades' Hayt
(defun ditch-unplaced-offereds (maker &optional (stream *standard-output*))
  (declare (ignorable stream))
  (flet ((query (stale)
           (gate-request (slot-reduce maker gate)
                         '(:get "/v2/private/order")
                         `(("order_id" . ,(oid stale))
                           ("symbol" . "BTCUSD"))))
         (compare (old new)
           (set-difference old new :key #'oid :test #'string=)))
    (with-slots (offered control response order-slots)
        (slot-reduce maker supplicant)
      (let ((stale (copy-list offered))
            (fresh (progn (send control '(:sync)) (recv response))))
        (values (complex (- order-slots (length stale))
                         (- order-slots (length fresh)))
                (compare fresh stale)
                (remove "EC_PostOnlyWillTakeLiquidity"  ; don't think too much
                        (mapcar #'query (compare stale fresh)) ; about names!
                        :key (getjso "reject_reason") :test-not #'string=))))))

(defun estimate-funding-dividend (gate market)
  ;; oughtta return two triples: {dividend, rate, position} x {this,next}
  ;; although it's worth considering both a five-valued ordering,
  ;;   xor a sequence expressing plausible regions for the second
  (with-json-slots ((numerator "predicted_funding_fee")
                    (predicted "predicted_funding_rate"))
      (gate-request gate '(:get "/open-api/funding/predicted-funding")
                    `(("symbol" . ,(name market))))
    (with-json-slots ((rate "funding_rate"))
        (gate-request gate '(:get "/open-api/funding/prev-funding-rate")
                      `(("symbol" . ,(name market))))
      (values (list (cons-aq* (primary market)
                              (- (* (/ numerator predicted) (parse-float rate))))
                    rate (cons-aq* (primary market) (/ numerator predicted)))
              (list (cons-aq* (primary market) (- numerator)) predicted
                    (cons-aq* (primary market) (/ numerator predicted)))))))

;;;
;;; Websocket
;;;
(defgeneric websocket-url (market)
  (:method ((market bybit-market)) (websocket-url (category market)))
  (:method ((category string))
    (format () "wss://stream.bybit.com/v5/public/~A" category)))

(defun handle-options-websocket-message (json &aux (parsed (read-json json)))
  (with-json-slots (topic data) parsed
    (aif topic
         (setf (recent-ticker (find-market (subseq it 8) :bybit))
               (loop for pair in data for (key . value) = pair
                     collect (aif (ignore-errors
                                   (parse-float value :type 'rational))
                                  (cons key it) pair)))
         (unless (string= (getjso "op" parsed) "pong")
           (format t "~&~A~%" parsed)))))

(defun make-options-websocket (&rest markets)
  (let ((client (wsd:make-client (websocket-url "option"))))
    (wsd:on :message client 'handle-options-websocket-message)
    (wsd:start-connection client)
    (unless markets
      (bt:make-thread (lambda ()
                        (loop
                          (sleep 17)
                          (wsd:send client "{\"op\":\"ping\"}")))
                      :name "websocket heartbeat"))
    (dolist (topic (mapcar (lambda (market)
                             (concatenate 'string "tickers." (name market)))
                           (remove (find-class 'option-market) markets
                                   :test-not #'eq :key #'class-of)))
      (sleep 1/13)
      (wsd:send client (format () "{\"op\":\"subscribe\",\"args\":[\"~A\"]}" topic)))
    client))

(defun handle-private-websocket-message (gate json &aux (parsed (read-json json)))
  (with-json-slots (op topic data) parsed
    (let ((data (if (eq :|result| (caar data)) (cdar data) data)))
      (aif topic
           (flet ((parse (json)
                    (loop for pair in json for (key . value) = pair
                          collect (aif (ignore-errors
                                        (parse-float value :type 'rational))
                                       (cons key it) pair))))
             (string-case (topic)
               ("position"
                (dolist (position data)
                  (setf (gethash (cons topic (getjso "symbol" position))
                                 (websocket-cache gate))
                        (parse position))))
               ("execution"
                (dolist (execution data)
                  (push (parse execution)
                        (gethash (cons topic (getjso "symbol" execution))
                                 (websocket-cache gate)))))
               ("order"
                ;; (when (zerop (random 7)) (break))
                (dolist (order data)
                  (with-json-slots (symbol (status "orderStatus")
                                    (oid "orderId") qty side price) order
                    (symbol-macrolet ((cache (gethash (cons topic symbol)
                                                      (websocket-cache gate))))
                      (if (member status '("Filled" "Cancelled") :test #'string=)
                          (setf cache (remove oid cache :key #'oid :test #'string=))
                          (let* ((market (find-market symbol :bybit))
                                 (volume (if (typep market 'inverse-market)
                                             (/ (parse-float qty :type 'rational)
                                                (parse-float price :type 'rational))
                                             (parse-float qty :type 'rational))))
                            (when market
                              (setf cache
                                    (cons (make-instance
                                           'offered :oid oid :market market
                                           :volume volume
                                           :price (* (parse-price
                                                      price (decimals market))
                                                     (if (string= side "Sell")
                                                         1 -1)))
                                          (remove oid cache :key 'oid
                                                            :test #'string=))))))
                      (aif (sort cache #'< :key #'price) (setf cache it)
                           (remhash (cons topic symbol)
                                    (websocket-cache gate)))))))
               ("wallet" ;; (with-trail ((getjso "creationTime") :amnesia :exponential) ? ... )
                (dolist (account data)
                  (setf (gethash (cons topic (getjso "accountType" account))
                                 (websocket-cache gate))
                        (parse account))))
               ("greeks"
                (dolist (coin data)
                  (setf (gethash (cons topic (getjso "baseCoin" coin))
                                 (websocket-cache gate))
                        (parse coin))))
               (t (push (progn (terpri) (prog1 (prin1 data) (fresh-line)))
                        (gethash topic (websocket-cache gate))))))
           (unless (string= op "pong") (format t "~&~A~%" parsed))))))

(defun make-private-websocket (gate kind topics)
  (let ((client (wsd:make-client "wss://stream.bybit.com/v5/private")))
    (wsd:on :message client
            (lambda (json) (handle-private-websocket-message gate json)))
    (wsd:start-connection client)
    (bt:make-thread (lambda ()
                      (loop
                        (sleep 17)
                        (wsd:send client "{\"op\":\"ping\"}")))
                    :name "websocket heartbeat")
    (with-slots (scalpl.exchange::pubkey scalpl.exchange::secret) gate
      (wsd:send client
                (let ((expiry (prin1-to-string
                               (* 1000 (+ 10 (timestamp-to-unix (now)))))))
                  (concatenate 'string "{\"op\":\"auth\",\"args\":[\""
                               scalpl.exchange::pubkey "\",\"" expiry "\",\""
                               (funcall scalpl.exchange::secret
                                        (concatenate 'string "GET/realtime" expiry))
                               "\"]}"))))
    (dolist (topic topics)
      (wsd:send client (format () "{\"op\":\"subscribe\",\"args\":[\"~A\"]}" topic)))
    (setf (gethash (cons kind topics) (websocket-cache gate)) client)))

(defparameter *pail* 1)

(defun make-market-websocket-handler
    (client table market &aux (category (category market)) (skip 0))
  (with-slots (book-table trades decimals) market
    (flet ((offer (side size price)
             (make-instance side :market market
                                 :volume (if (string= "inverse" category)
                                             (/ size price) size)
                                 :price (* price (expt 10 decimals))))
           (snot (&rest icles)
             (format *debug-io* "~&;;; ~A ~D ~{~A~^ ~} ~%"
                     (now) skip icles)
             (setf skip 0)
             (length icles)))
      (lambda (raw &aux (message (read-json raw)))
        (case (caar message)
          (:|success|
           (unless (string= (getjso "op" message) "subscribe")
             (wsd:close-connection client)))
          (:|topic|
           (with-json-slots (topic type data) message
             (if (> (random *pail*) pi)
                 (progn (incf skip)
                        (setf *pail* (sqrt (1+ (expt *pail* 2)))))
                 (snot topic type (length data)))
             (cond
               ((string= topic table)
                (when (or (string= type "snapshot")
                          (= (getjso "u" data) 1))
                  (clrhash book-table))
                (macrolet ((do-side (key side)
                             `(dolist (item (getjso ,key data))
                                (let ((price #1=(parse-float (pop item)
                                                             :type 'rational))
                                      (size #1#))
                                  (if (zerop size) (remhash price book-table)
                                      (setf (gethash price book-table)
                                            (offer ,side size price)))))))
                  (do-side "b" 'bid) (do-side "a" 'ask)))
               ((and (string= (subseq topic 0 12) "publicTrade.")
                     (string= (subseq topic 12) (name market)))
                (dolist (json data)
                  (with-json-slots ((side "S") v p i (time "T")) json
                    (push (let ((size (parse-float v :type 'rational))
                                (price (parse-float p :type 'rational)))
                            (make-instance
                             'trade :direction side :txid i
                             :market market :price price
                             :cost (if (string= "inverse" category) size
                                       (* size price))
                             :volume (if (string= "inverse" category)
                                         (/ size price) size)
                             :timestamp (multiple-value-bind
                                              (seconds milliseconds)
                                            (floor time 1000)
                                          (unix-to-timestamp
                                           seconds :nsec (* (expt 10 6)
                                                            milliseconds)))))
                          trades))))
               (t (wsd:close-connection client))))))))))

(defun make-market-socket (market &optional depth)
  (unless depth (setf depth (if (string= (category market) "option") 100 500)))
  (let* ((topic (format () "orderbook.~D.~A" depth (name market)))
         (client (wsd:make-client (websocket-url market))))
    (wsd:on :message client
            (make-market-websocket-handler client topic market))
    (wsd:start-connection client)
    (wsd:send client (format () "{\"op\":\"subscribe\",\"args\":[~S]}" topic))
    (wsd:send client (format () "{\"op\":\"subscribe\",\"args\":[~S]}"
                             (format () "publicTrade.~A" (name market))))
    client))

;;; "There are only two Hard Problems of Consciousness:
;;;    the false dichotomy between the problems, and
;;;      prioritisation of hypothesis falsification."
(defun make-liquidity-background (market minimum maximum step
                                  &aux (price-factor (expt 10 (decimals market))))
  (let ((size (1+ (ceiling (- maximum minimum) step))))
    (let ((bids (make-hash-table :test #'eql :size size))
          (asks (make-hash-table :test #'eql :size size)))
      (loop for cdr on
           (loop with descending for price from minimum to maximum by step
              for mp = (* price price-factor)
              for volume = (/ (1+ (random (expt size (random 1.5)))) price)
              for ask = (make-instance 'ask :market market
                                       :price mp :volume volume)
              for bid = (make-instance 'bid :market market
                                       :price mp :volume volume)
              collect ask do (setf (gethash (price bid) bids)
                                   (push bid descending)))
         do (setf (gethash (price (car cdr)) asks) cdr))
      (values bids asks))))

(defclass streaming-market (bybit-market)
  (socket background (book-table :initform (make-hash-table :test #'eql))
   (trades :initform ()) (category :allocation :instance)))

(defmethod shared-initialize :after ((market streaming-market) (names t)
                                     &key minimum maximum step)
  (with-slots (socket background) market
    (setf socket (make-market-socket market))
    (when (and minimum maximum step)
      (setf background
            (multiple-value-call #'cons
              (make-liquidity-background market minimum maximum step))))))

(defmethod get-book ((market streaming-market) &key)
  (if (not (slot-boundp market 'book-table)) (values () ())
      (flet ((snot (&rest icles)
               (format *debug-io* "~&;;; ~A ~{~A~^ ~} ~%"
                       (now) icles)
               (length icles)))
       (with-slots (book-table background) market
         (loop for offer being each hash-value of book-table
               if (eq (type-of offer) 'ask) collect offer into asks
               if (eq (type-of offer) 'bid) collect offer into bids
               finally (return
                         (let ((asks (sort asks #'> :key #'price))
                               (bids (sort bids #'> :key #'price)))
                           (flet ((book (side background)
                                    ;; (snot (length side))
                                    (if (null background) (nreverse side)
                                        (nconc (reverse side)
                                               (cdr (gethash (price (first side))
                                                             background))))))
                             (values (book asks (cdr background))
                                     (book bids (car background)))))))))))

(defmethod get-book :before ((market streaming-market) &key)
  (with-slots (socket) market
    (unless (and (slot-boundp market 'socket) (slot-boundp market 'book-table))
      (loop for delay from 1
         do (handler-case
                (return (setf socket (make-market-socket market)))
              (error (error) (warn "~&Encountered:~%~A~%" error)))
           (sleep delay))
      (sleep 13))))
(defmethod get-book :after ((market streaming-market) &key)
  (with-slots (socket) market
    (when (slot-boundp market 'socket)
      (loop for delay from 1 while (eq (wsd:ready-state socket) :closed)
            do (handler-case
                   (return (setf socket (make-market-socket market)))
                 (error (error) (warn "~&Encountered:~%~A~%" error)))
               (sleep delay)))))

(defmethod trades-since ((market streaming-market) &optional since)
  (with-slots (trades) market
    (if (null since) trades
        (remove (timestamp since) trades
                :key #'timestamp :test #'timestamp>=))))

;;;
;;; Options!
;;;

(defun placed-option-offers (gate &aux offers)
  (let ((url "order/realtime"))
    (loop
      with cursor do
        (multiple-value-bind (result error)
            (gate-request gate `(:get ,url)
                          `(("category" . "option")
                            ,@(if cursor `(("cursor" . ,(url-decode cursor))))))
          ;; data sorted newest-to-oldest by order/realtime endpoint
          (unless error
            (if (string= cursor (getjso "nextPageCursor" result)) (return)
                (setf cursor (getjso "nextPageCursor" result)))
            (dolist (json (getjso "list" result))
              (with-json-slots (symbol side price qty
                                (oid "orderId") (urid "orderLinkId")) json
                (let* ((market (find-market symbol *bybit*))
                       (price (parse-price price (decimals market)))
                       (volume (parse-float qty :type 'rational)))
                  (pushnew (make-instance ; reverses ordering of data
                            'placed :oid oid :urid urid
                            :market market :volume volume
                            :price (if (string= side "Sell")
                                       price (- price)))
                           offers :test #'string= :key 'oid))))))
      until (or (null cursor) (string= cursor "")))
    ;; returns placed offers sorted in order of placement
    offers))

(defun placed-options (gate &key (sort '(> :key second)) &aux positions)
  ;; (declare (optimize debug))
  (loop
    with cursor do
      (awhen (gate-request gate
                           '(:get "position/list")
                           `(("category" . "option")
                             ,@(if cursor `(("cursor" . ,cursor)))))
        (with-json-slots (list (next "nextPageCursor")) it
          (setf cursor next)
          (dolist (position list) (push position positions))))
    until (or (null cursor) (string= "" cursor)))
  (alet (mapcar (lambda (json)
                  (with-json-slots
                      (symbol size side (entry "avgPrice") (mark "markPrice")
                       (rpnl "cumRealisedPnl") (upnl "unrealisedPnl")
                       (value "positionValue")) json
                    (with-aslots (primary counter) (find-market symbol :bybit)
                      (flet ((parse (float) (parse-float float :type 'rational)))
                        (let ((size (* (parse size)
                                       (string-case (side)
                                         ("Buy" 1) ("Sell" -1))))
                              (mark (parse mark))
                              (entry (parse entry)))
                          (list* it (float (* (1- (/ mark entry)) size))
                                 (float mark) (float entry)
                                 (mapcar 'cons-aq*
                                         (list counter primary counter counter)
                                         (list (parse upnl) size
                                               (parse value)
                                               (parse rpnl)))))))))
                positions)
    (if sort (apply #'sort it sort) it)))

;;; "There is no sex in handwriting." - Firth
(defun best-written-options (gate &key (count 5) (start 0))
  ;; (declare (optimize debug))
  (let ((data (loop for option in (placed-options gate)
                    for remain downfrom (+ count start) above 0
                    for skip downfrom start
                    for (market roi mark entry upnl size value rpnl) = option
                    ;; do (break)
                    when (and (minusp (quantity size)) (not (plusp skip)))
                      collect
                      (with-slots (delivery-date) market
                        (multiple-value-call 'list delivery-date option
                          (get-book (first option) :count 2))))))
    (values data (mapcar 'caadr data)
            (delete-duplicates (mapcar 'car data)))))

;;; "... so bless my bottom!" - El-Arairah
(defun worst-written-options (gate &key (count 5) (start 0))
  (let ((data (loop for option in (placed-options gate :sort '(< :key second))
                    for remain downfrom (+ count start) above 0
                    for skip downfrom start
                    for (market roi mark entry upnl size value rpnl) = option
                    when (and (minusp (quantity size)) (not (plusp skip)))
                      collect
                      (with-slots (delivery-date) market
                        (multiple-value-call 'list delivery-date option
                          (get-book (first option) :count 2))))))
    (values data (mapcar 'caadr data)
            (delete-duplicates (mapcar 'car data)))))

;;; Kamrad Oglivy had never read such bullshit!
(defun yet-written-options (gate &key (count 5) (start 0) (delta 0.23))
  (let* ((options (remove (find-class 'option-market) (markets *bybit*)
                          :key 'class-of :test-not #'eq))
         (distant (sort (remove delta options :test #'< :key
                                (lambda (option)
                                  (abs (getjso "delta" (recent-ticker option)))))
                        #'> :key
                        (lambda (option) ;_; currently not directional!
                          (with-json-slots
                              (gamma vega theta) (recent-ticker option)
                            (* gamma (exp vega) (- theta))))))
         (data (loop for market in distant
                     for remain downfrom (+ count start) above 0
                     for skip downfrom start unless (plusp skip)
                     collect (multiple-value-call 'list
                               (delivery-date market) market
                               (get-book market :count 1)))))
    (values data (mapcar 'cadr data)
            (delete-duplicates (mapcar 'car data)))))

#+ (and example for mac read only)
(pexec (:name "options writer")
  (loop for start = (round (exp (random 3.5)))
        for count = (ceiling (exp (random 2.5)))
        do (multiple-value-bind (positions markets)
               (bybit::yet-written-options *gate* :count count :start start)
             (format t "~&Checking ~D markets [skip ~D]~%" count start)
             ;; following code intended for writing new options
             (loop for market in markets and data in positions
                   for market-tick = (slot-reduce market bybit::tick)
                   for market-placed = (bybit::market-placed *gate* market)
                   for (market-asks market-bids) = (last data 2)
                   for fresh-price = (princ-to-string
                                      (if (null market-bids) market-tick
                                          (- (price (first market-asks))
                                             (if (and market-bids
                                                      (> (abs (price (first market-asks)))
                                                         (+ market-tick
                                                            (abs (price (first market-bids))))))
                                                 market-tick 0))))
                   if (if (or (null market-placed) (null market-bids))
                          ;; blue puddle
                          (bybit::post-limit *gate* market fresh-price -1/100)
                          ;; red puddle
                          (unless (find (price (first market-asks)) market-placed
                                        :key #'price :test #'=)
                            (aif (remove 0 market-placed :key #'price :test #'>)
                                 (bybit::amend-limit *gate* market
                                                     (oid (first (sort it #'> :key #'price)))
                                                     fresh-price 1/100)
                                 (bybit::post-limit *gate* market fresh-price -1/100))))
                     do (format t "~&Wrote on ~A~%" (name market))))))

#+ (and example for mac dont bother)
(multiple-value-bind (positions markets)
    (bybit::worst-written-options *gate*)
  ;; following code intended for closing written options
  (loop for market in markets and data in positions
        for market-tick = (slot-reduce market bybit::tick)
        for market-placed = (bybit::market-placed *gate* market)
        for (market-asks market-bids) = (last data 2)
        for fresh-price = (princ-to-string
                           (if (null market-bids) market-tick
                               (+ (abs (price (first market-bids)))
                                  (if (and market-asks
                                           (> (abs (price (first market-asks)))
                                              (+ market-tick
                                                 (abs (price (first market-bids))))))
                                      market-tick 0))))
        if (if (or (null market-placed) (null market-bids))
               ;; blue puddle
               (bybit::post-limit *gate* market fresh-price 1/100 t)
               ;; red puddle
               (unless (find (price (first market-bids)) market-placed
                             :key #'price :test #'=)
                 (aif (remove 0 market-placed :key #'price :test #'<)
                      (bybit::amend-limit *gate* market
                                          (oid (first (sort it #'< :key #'price)))
                                          fresh-price 1/100)
                      (bybit::post-limit *gate* market fresh-price 1/100 t))))
          collect it))

#|
(pexec (:name "options closer")
  (loop for start = (round (exp (1- (random 2.3))))
        for count = (ceiling (exp (random 3.7)))
        do (multiple-value-bind (positions markets)
               (bybit::best-written-options *gate* :count count :start start)
             (format t "~%Checking ~D markets [skip ~D]~&" count start)
             ;; following code intended for closing written options
             (loop for market in markets and data in positions
                   while (plusp (cadadr data)) ; only close profits!
                   for market-tick = (slot-reduce market bybit::tick)
                   for market-placed = (bybit::market-placed *gate* market)
                   for (market-asks market-bids) = (last data 2)
                   for fresh-price = (princ-to-string
                                      (if (null market-bids) market-tick
                                          (+ (abs (price (first market-bids)))
                                             (if (and market-asks
                                                      (> (abs (price (first market-asks)))
                                                         (+ market-tick
                                                            (abs (price (first market-bids))))))
                                                 market-tick 0))))
                   if (if (or (null market-placed) (null market-bids))
                          ;; blue puddle
                          (bybit::post-limit *gate* market fresh-price 1/100 t)
                          ;; red puddle
                          (unless (find (price (first market-bids)) market-placed
                                        :key #'price :test #'=)
                            (aif (remove 0 market-placed :key #'price :test #'<)
                                 (bybit::amend-limit *gate* market
                                                     (oid (first (sort it #'< :key #'price)))
                                                     fresh-price 1/100)
                                 (bybit::post-limit *gate* market fresh-price 1/100 t))))
                     collect it))))

|#

#|
(pexec (:name "option writer [1 13]")
      (loop for start = (round (exp (random 2.5)))
            for count = (ceiling (exp (1+ (random (exp 1)))))
            do (multiple-value-bind (positions markets)
                   (bybit::yet-written-options *gate* :count count :start start
                                               :delta 0.17)
                 (format t "~&Scribbling ~D options [skip ~D]~%" count start)
                 ;; following code intended for writing new options
                 (loop for market in markets and data in positions
                       for market-tick = (slot-reduce market bybit::tick)
                       for market-placed = (bybit::market-placed *gate* market)
                       for (market-asks market-bids) = (last data 2)
                       for fresh-price = (princ-to-string
                                          (if (null market-bids)
                                              (* market-tick
                                                 (floor (* (getjso "markPrice"
                                                                   (recent-ticker market))
                                                           (1+ (random pi)))))
                                              (- (price (first market-asks))
                                                 (if (and market-bids
                                                          (> (abs (price (first market-asks)))
                                                             (+ market-tick
                                                                (abs (price (first market-bids))))))
                                                     market-tick 0))))
                       do (sleep 1)
                       if (if (or (null market-placed) (null market-bids))
                              ;; blue puddle
                              (bybit::post-limit *gate* market fresh-price -1/100)
                              ;; red puddle
                              (unless (find (price (first market-asks)) market-placed
                                            :key #'price :test #'=)
                                (aif (remove 0 market-placed :key #'price :test #'>)
                                     (bybit::amend-limit *gate* market
                                                         (oid (first (sort it #'> :key #'price)))
                                                         fresh-price 1/100)
                                     (bybit::post-limit *gate* market fresh-price -1/100))))
                         do (format t "~&Sketched ~A @ ~D~%"
                                    (name market) fresh-price))
                 (sleep (random 13)))))
|#

#+ (or)
(loop with makers = (list *perpr* *fdecr*)
      for positions = (loop for maker in makers
                            collect
                            (with-slots (gate market) maker
                              (with-json-slots
                                  (symbol side size)
                                  (nth-value 1 (bybit::account-position gate market))
                                `(,symbol . ,(if (string= side "Buy")
                                                 (parse-float size)
                                                 (- (parse-float size)))))))
      for total = (getjso "totalDelta"
                          (gethash '("greeks" . "BTC")
                                   (slot-value *gate* 'bybit::websocket-cache)))
      for aims = (mapcar (lambda (position)
                           (list (car position) (cdr position)
                                 (- (cdr position) total)
                                 (/ (exp (/ (- (cdr position) total) 2)))))
                         positions)
      do (dolist (maker makers)
           (with-slots (market targeting-factor) maker
             (setf targeting-factor
                   (fourth (assoc (name market) aims :test #'string=))))
           (print-book maker :ours t :market t :count 7 :wait t))
      do (format t "~&Symbol~13T  Size~5@T   Aim~5@TTarget")
      do (format t "~:{~%~A~13T~@{~3@$~^~5@T~}~}~%" aims)
      do (sleep 5) do (sb-ext:gc :full t))

#+ (or)
(macrolet ((chaff (symbol market &optional (resilience 23456.7) (skew 23/19))
             `(let ((supplicant
                      (make-instance 'supplicant
                                     :market (find-market ,@market)
                                     :gate *gate* :order-slots 37)))
                (change-class (slot-reduce supplicant market
                                           scalpl.exchange::%market)
                              'bybit::streaming-market)
                (format t "~&Loading ~A~%" (find-market ,@market))
                (sleep 1)
                (define-maker ,symbol :supplicant supplicant
                  :fund-factor 2/7 :resilience ,resilience
                  :targeting 1 :skew-factor ,skew :cut 1/79)
                (change-class (slot-reduce ,symbol ope prioritizer)
                              'bybit::bybit-prioritizer))))
  (chaff *perpr* (:btcperp :bybit))
  (chaff *fsepr* (:btc-29sep23 :bybit))
  (chaff *foctr* (:btc-27oct23 :bybit))
  (chaff *fdecr* (:btc-29dec23 :bybit))
  (chaff *fmarr* (:btc-29mar24 :bybit))
  )
