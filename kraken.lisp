(defpackage #:scalpl.kraken
  (:nicknames #:kraken)
  (:use #:cl #:base64 #:chanl #:anaphora #:local-time #:scalpl.util
        #:scalpl.actor #:scalpl.net #:scalpl.exchange #:scalpl.qd)
  (:export #:*kraken* #:kraken-gate #:caching-gate))

(in-package #:scalpl.kraken)

;;; General Parameters
(defparameter +base-path+ "https://api.kraken.com/0/")

(defun hmac-sha512 (message secret)
  (let ((hmac (ironclad:make-hmac secret 'ironclad:sha512)))
    (ironclad:update-hmac hmac message)
    (ironclad:hmac-digest hmac)))

(defun hash-sha256 (message)
  (ironclad:digest-sequence :sha256 message))

;;; API-Key = API key
;;; API-Sign = Message signature using HMAC-SHA512
;;;              of (URI path + SHA256(nonce + POST data))
;;;                  and base64 decoded secret API key

(defgeneric make-signer (secret)
  (:method ((signer function)) signer)
  (:method ((array array))
    (lambda (path data nonce)
      (hmac-sha512 (concatenate
                    '(simple-array (unsigned-byte 8) (*))
                    (map '(simple-array (unsigned-byte 8) (*))
                         'char-code path)
                    (hash-sha256 (map '(simple-array (unsigned-byte 8) (*))
                                      'char-code
                                      (concatenate 'string nonce
                                                   (urlencode-params data)))))
                   array)))
  (:method ((string string)) (make-signer (base64-string-to-usb8-array string)))
  (:method ((stream stream)) (make-signer (read-line stream)))
  (:method ((path pathname)) (with-open-file (data path) (make-signer data))))

(defgeneric make-key (key)
  (:method ((key string)) key)
  (:method ((stream stream)) (read-line stream))
  (:method ((path pathname))
    (with-open-file (stream path)
      (make-key stream))))

(defvar *error-breakpoints* nil)

(defun raw-request (path &rest keys)
  (multiple-value-bind (body status headers)
      (apply #'http-request (concatenate 'string +base-path+ path) keys)
    (case status
      (200 (with-json-slots (result error)
               (read-json (map 'string 'code-char body))
             (when (and error *error-breakpoints*)
               (typecase error
                 (string (cerror "LGTM" error))
                 ((cons string null)
                  (cerror "LGTM" (first error)))
                 (list (warn (format () "~A" error)))))
             (values result error headers)))
      (404 (with-json-slots (result error)
               (read-json (map 'string 'code-char body))
             (format t "~&Aborting after 404...~%")
             (describe error))
       (values nil t headers))
      ((400 409 500 502 503 504 520 522 524 525)
       (format t "~&Retrying after ~D...~%" status)
       (sleep 2) (apply #'raw-request path keys))
      (t (cerror "Retry request" "HTTP Error ~D" status)
       (apply #'raw-request path keys)))))

(defun get-request (path &optional data)
  (raw-request (concatenate 'string "public/" path "?"
                            (urlencode-params data))
               :parameters data))

(defun nonce (&aux (now (now)))
  (princ-to-string (+ (floor (nsec-of now) 1000)
                      (* 1000000 (timestamp-to-unix now)))))

(defun post-request (method key signer &optional data &aux (nonce (nonce)))
  (let ((path (concatenate 'string "/0/private/" method)))
    (push (cons "nonce" nonce) data)
    (raw-request (concatenate 'string "private/" method)
                 :method :post :parameters data
                 :additional-headers
                 `(("API-Key"  . ,key)
                   ("API-Sign" . ,(base64:usb8-array-to-base64-string
                                   (funcall signer path data nonce)))
                   ("Content-Type" . "application/x-www-form-urlencoded")))))

(defun get-assets ()
  (mapcar-jso (lambda (name data)
                (with-json-slots (altname decimals) data
                  (make-instance 'asset :name name :decimals decimals)))
              (get-request "Assets")))

(defclass kraken-market (market) ((altname :initarg :altname :reader altname)))

(defmethod altname ((market scalpl.exchange::tracked-market))
  (altname (slot-value market 'scalpl.exchange::%market))) ; anger leads to hate

(defun get-markets (assets)
  (mapcar-jso (lambda (name data)
                (with-json-slots (pair_decimals quote base altname) data
                  (make-instance 'kraken-market :name name :altname altname
                                 :primary (find-asset base assets)
                                 :counter (find-asset quote assets)
                                 :decimals pair_decimals)))
              (get-request "AssetPairs")))

(defun request-cost (request)
  (string-case (request :default 1)
    ("AddOrder" 0) ("CancelOrder" 0)
    ("Ledgers" 2) ("QueryLedgers" 2)
    ("TradesHistory" 2) ("QueryTrades" 2)))

(defclass token-minter (actor)
  ((abbrev :allocation :class :initform "token minter")))

(defmethod perform ((minter token-minter) &key)
  (with-slots (mint delay) minter (send mint 1) (sleep delay)))

(defclass token-handler (actor)
  ((count :initform 0) (abbrev :allocation :class :initform "token handler")))

(defmethod perform ((handler token-handler) &key)
  (with-slots (mint count tokens) handler
    (cond ((<= 5 count) (decf count (recv mint)))
          ((>= 0 count) (send tokens (incf count)))
          (t (select ((recv mint delta) (decf count delta))
                     ((send tokens t) (incf count))
                     (t (sleep 0.2)))))))

(defclass token-mixin (exchange parent)
  ((delay :initform 9/8 :initarg :delay)
   (tokens :initform (make-instance 'channel))
   (mint :initform (make-instance 'channel))))

(defmethod initialize-instance :after ((gate token-mixin) &key name)
  (flet ((make (class role)
           (adopt gate (make-instance class :delegates `(,gate) :name
                                      (format nil "api ~A for ~A" role name)))))
    (mapcar #'make '(token-minter token-handler) '("minter" "counter"))))

(defvar *kraken* (make-instance 'token-mixin :name :kraken :sensitivity 0.3))

(defmethod fetch-exchange-data ((exchange (eql *kraken*)))
  (with-slots (markets assets) exchange
    (setf assets (get-assets) markets (get-markets assets))))

(defmethod find-market (designator (exchange (eql *kraken*)))
  (or (call-next-method)
      (with-slots (markets) *kraken*
        (find designator markets :key 'altname :test 'string-equal))))

(defclass kraken-gate (gate)
  ((exchange :initform *kraken* :allocation :class)))

(defclass caching-gate (kraken-gate)
  ((recent-responses :initform (make-hash-table :test 'equal))))

(defmethod gate-post ((gate (eql *kraken*)) key secret request)
  (destructuring-bind (command . options) request
    ;; FIXME: this prevents add/cancel requests from going through while
    ;; other requests wait for tokens. possible solution - queue closures
    ;; with their associated cost, execute most expensive affordable request
    (dotimes (i (request-cost command))
      (recv (slot-reduce gate tokens)))
    ;; (format t "~&~A ~A~&" (now) command)
    (multiple-value-bind (ret err) (post-request command key secret options)
      (when (and err (string= "EAPI:Rate limit exceeded" (first err)))
        (format t "~&API limit hit, cooling off...~%") (sleep 30))
      (list ret err))))

(defmethod shared-initialize ((gate kraken-gate) names &key pubkey secret)
  (multiple-value-call #'call-next-method gate names
                       (mvwrap pubkey make-key) (mvwrap secret make-signer)))

;;;
;;; Public Data API
;;;

(defmethod get-book ((market kraken-market) &key (count 200)
                     &aux (pair (name market)))
  (let ((decimals (slot-value market 'decimals)))
    (with-json-slots (bids asks)
        (getjso pair (get-request "Depth" `(("pair" . ,pair)
                                            ("count" . ,(prin1-to-string count)))))
      (flet ((parser (class)
               (lambda (raw-order)
                 (destructuring-bind (price amount timestamp) raw-order
                   (declare (ignore timestamp))
                   (make-instance class :market market
                                  :price (parse-price price decimals)
                                  :volume (parse-float amount :type 'rational))))))
        (values (mapcar (parser 'ask) asks) (mapcar (parser 'bid) bids))))))

(defmethod trades-since ((market kraken-market) &optional since)
  (awhen (get-request "Trades"
                      `(("pair" . ,(name market)) .
                        ,(awhen (timestamp since)
                                `(("since"
                                   . ,(format nil "~D~9,'0D"
                                              (timestamp-to-unix it) (nsec-of it)))))))
    (with-json-slots (last (trades (name market))) it
      (mapcar (lambda (trade)
                (destructuring-bind (price volume time side kind data id) trade
                  (let ((price  (parse-float price))
                        (volume (parse-float volume :type 'rational)))
                    (make-instance 'trade
                                   :market market :txid id
                                   :timestamp (parse-timestamp *kraken* time)
                                   ;; FIXME - "cost" later gets treated as precise
                                   :volume volume :price price
                                   :cost (* volume price)
                                   :direction (concatenate 'string side kind data)))))
              (rest trades)))))      ; same trick as in bitfinex execution-since

;;;
;;; Private Data API
;;;

(defmethod placed-offers ((gate kraken-gate) &optional market)
  (awhen (gate-request gate "OpenOrders")
    (remove market
            (mapcar-jso (lambda (id data)
                          (with-json-slots (descr vol oflags) data
                            (with-json-slots (pair type price order) descr
                              (let* ((market (find-market pair *kraken*))
                                     (decimals (slot-value market 'decimals))
                                     (price-int (parse-price price decimals))
                                     (volume (parse-float vol :type 'rational)))
                                (make-instance
                                 'offered :oid id :market market
                                 :price (if (string= type "buy")
                                            (- price-int) price-int)
                                 :volume volume)))))
                        (getjso "open" it))
            :key #'market :test-not #'eq)))

(defmethod placed-offers ((gate caching-gate) &optional market)
  (with-slots (recent-responses) gate
    (aif (aand (gethash "OpenOrders" recent-responses)
               (destructuring-bind (timestamp . response) it
                 (and (< (timestamp-difference (now) timestamp) 30)
                      response)))
         (remove market it :key #'market :test-not #'eq)
         (awhen (gate-request gate "OpenOrders")
           (let ((placed (mapcar-jso
                          (lambda (id data)
                            (with-json-slots (descr vol oflags) data
                              (with-json-slots (pair type price order) descr
                                (let* ((market (find-market pair *kraken*))
                                       (decimals (slot-value market 'decimals))
                                       (price-int (parse-price price decimals))
                                       (volume (parse-float vol :type 'rational)))
                                  (make-instance
                                   'offered :oid id :market market
                                   :price (if (string= type "buy")
                                              (- price-int) price-int)
                                   :volume volume)))))
                          (getjso "open" it))))
             (setf (gethash "OpenOrders" recent-responses)
                   (cons (now) placed))
             (remove market placed :key #'market :test-not #'eq))))))

(defmethod account-balances ((gate kraken-gate))
  (remove-if #'zerop
             ;; TODO: this signals !(typep () 'jso) on communication failure
             ;; signaling is a Good Thing, but let's be more helpful
             (mapcar-jso (lambda (currency amount)
                           (cons-aq* (find-asset currency *kraken*)
                                     (parse-float amount :type 'rational)))
                         (gate-request gate "Balance"))
             :key #'quantity))

(defmethod account-balances ((gate caching-gate))
  (with-slots (recent-responses) gate
    (aif (aand (gethash "Balance" recent-responses)
               (destructuring-bind (timestamp . response) it
                 (and (< (timestamp-difference (now) timestamp) 30)
                      response)))
         it
         (let ((balances (call-next-method)))
           (setf (gethash "Balance" recent-responses)
                 (cons (now) balances))
           balances))))

(defmethod market-fee ((gate kraken-gate) market &aux (pair (name market)))
  (awhen (gate-request gate "TradeVolume" `(("pair" . ,pair)))
    (read-from-string (getjso "fee" (getjso pair (getjso "fees" it))))))

(defun parse-execution (txid json)
  (with-json-slots (price pair fee cost vol time type ordertxid) json
    (let ((fee (parse-float fee))
          (cost (parse-float cost)))
      (make-instance 'execution :fee fee :direction type
                     :txid (string txid) :oid ordertxid
                     :price (parse-float price) :cost cost
                     :volume (parse-float vol) :market (find-market pair *kraken*)
                     :net-volume (parse-float vol)
                     :timestamp (parse-timestamp *kraken* time)
                     :net-cost (string-case (type)
                                 ("buy" (+ cost fee))
                                 ("sell" (- cost fee)))))))

(defun raw-executions (gate &key start end)
  (flet ((endpoint (thing)
           (etypecase thing
             (timestamp (prin1-to-string (timestamp-to-unix thing)))
             (trade (txid thing)) (string thing))))
    (let ((range `(,@(when start `(("start" . ,(endpoint start))))
                   ,@(when end `(("end" . ,(endpoint end)))))))
      (loop for offset from 0 by 50
            for trades = (gate-request gate "TradesHistory"
                                       `(("ofs" . ,(princ-to-string offset))
                                         ,@range))
            nconc (getjso "trades" trades)
            until (> offset (- (getjso "count" trades) 50))))))

(defmethod execution-since ((gate kraken-gate) (market market) since)
  (awhen (ignore-errors (raw-executions gate :start since))
    (remove market (mapcar-jso #'parse-execution it)
            :key #'market :test-not #'eq)))

(defmethod execution-since ((gate caching-gate) (market market) since)
  (declare (optimize debug))
  (symbol-macrolet
      ((cache (gethash "TradesHistory" (slot-reduce gate recent-responses))))
    (aif (aand cache (when (> 15 (timestamp-difference
                                  (now) (timestamp (car it))))
                       it))
         (remove market
                 (if (null since) it
                     (remove (timestamp since) it
                             :key #'timestamp :test #'timestamp>=))
                 :test-not #'eq :key #'market)
         (progn
           (let ((raw (raw-executions gate :start (or (car cache) since))))
             (dolist (execution (mapcar-jso #'parse-execution raw))
               (pushnew execution cache :key #'txid :test #'string=)))
           (reverse (remove market
                            (if (null since) cache
                                (remove (timestamp since) cache
                                        :key #'timestamp :test #'timestamp>=))
                            :test-not #'eq :key #'market))))))

#+nil
(defun trades-history-chunk (tracker &key until since)
  (with-slots (delay gate) tracker
    (awhen (apply #'execution-history gate
               (append (when until `(:until ,until))
                       (when since `(:since ,since))))
      (with-json-slots (count trades) it
          (let* ((total (parse-integer count))
                 (chunk (make-array (list total) :fill-pointer 0)))
            (flet ((process (trades-jso)
                     (mapjso (lambda (tid data)
                               (map nil (lambda (key)
                                          (setf (getjso key data)
                                                (read-from-string (getjso key data))))
                                    '("price" "cost" "fee" "vol"))
                               (with-json-slots (txid time) data
                                 (setf txid tid time (parse-timestamp *kraken* time)))
                               (vector-push data chunk))
                             trades-jso)))
              (when (zerop total)
                (return-from trades-history-chunk chunk))
              (process trades)
              (unless until
                (setf until (getjso "txid" (elt chunk 0))))
              (loop
                 (when (= total (fill-pointer chunk))
                   (return (sort chunk #'timestamp<
                                 :key (lambda (o) (getjso "time" o)))))
                 (sleep delay)
                 (awhen (apply #'execution-history gate
                               :until until :ofs (princ-to-string (fill-pointer chunk))
                               (when since `(:since ,since)))
                   (with-json-slots (count trades) it
                     (let ((next-total (parse-integer count)))
                       (assert (= total next-total))
                       (process trades)))))))))))

;;;
;;; Action API
;;;

(defun post-limit (gate type market price volume decimals)
  (with-slots ((pair name) (vol-decimals decimals)) market
    (let ((price (/ price (expt 10d0 decimals))))
      (when (string= type "sell")
        (setf volume (* volume price))) ; always viqc
      (multiple-value-bind (info errors)
          (flet ((money (&rest args)
                   (apply #'format nil "~V$" args)))
            (gate-request gate "AddOrder"
                          `(("ordertype" . "limit")
                            ("type" . ,type) ("pair" . ,pair)
                            ("volume" . ,(money vol-decimals volume))
                            ("price" . ,(money decimals price))
                            ("oflags" . "post"))))
        (unless errors
          ;; theoretically, we could get multiple oids here, (why "txid"!?)
          ;; but kraken's margin casino isn't open for visitors yet
          (car (getjso "txid" info)))))))

(defmethod post-offer ((gate kraken-gate) (offer offer))
  ;; (format t "~&place  ~A~%" offer)
  (with-slots (market volume price) offer
    (flet ((post (type)
             (awhen (post-limit gate type market
                                (abs price) volume
                                (slot-value market 'decimals))
               (change-class offer 'offered :oid it))))
      (if (< price 0) (post "buy") (post "sell")))))

(defun amend-limit (gate type market txid price volume decimals)
  (with-slots ((pair name) (vol-decimals decimals)) market
    (let ((price (/ price (expt 10d0 decimals))))
      (when (string= type "sell")
        (setf volume (* volume price))) ; always viqc
      (multiple-value-bind (info errors)
          (flet ((money (&rest args)
                   (apply #'format nil "~V$" args)))
            (gate-request gate "AmendOrder"
                          `(("txid" . ,txid)
                            ("order_qty" . ,(money vol-decimals volume))
                            ("limit_price" . ,(money decimals price))
                            ("post_only" . "true"))))
        (values (unless errors (getjso "amend_id" info)) errors)))))

(defmethod amend-offer ((gate kraken-gate) (old offer) (new offer))
  ;; (format t "~&amend  ~A~%" offer)
  (with-slots (market volume price) new
    (with-slots (oid old-market) old
      (flet ((post (type)
               (awhen (amend-limit gate type market
                                   oid (abs price) volume
                                   (slot-value market 'decimals))
                 (change-class new 'offered :oid it))))
        (if (< price 0) (post "buy") (post "sell"))))))

(defun cancel-order (gate oid)
  (gate-request gate "CancelOrder" `(("txid" . ,oid))))

(defmethod cancel-offer ((gate kraken-gate) (offer offered))
  ;; (format t "~&cancel ~A~%" offer)
  (multiple-value-bind (ret err)
      (cancel-order gate (oid offer))
    (or ret (search "Unknown order" (car err)))))

;;; Duplicate of bybit-proiritizer ; factor them out... someday

(defun ope-amend (ope old new)
  (amend-offer (slot-reduce ope supplicant gate) old new)
  (send (slot-reduce ope supplicant control) '(:timestamp)))

(defclass kraken-prioritizer (prioritizer) ())

(defmethod prioriteaze ((ope kraken-prioritizer) target placed
                        &aux to-add (excess placed))
  (flet ((frob (add pop &aux (max (max (length add) (length pop))))
           (with-slots (expt) ope
             (let* ((n (expt (random (expt max (/ expt))) expt))
                    (add (nth (floor n) add))
                    (pop (nth (- max (ceiling n)) pop)))
               (if (and add pop)
                   (ope-amend ope pop add)
                   (if add (ope-place ope add) (ope-cancel ope pop)))))))
    (aif (dolist (new target (sort to-add #'< :key #'price))
           (aif (find (price new) excess :key #'price :test #'=)
                (setf excess (remove it excess)) (push new to-add)))
         (frob it excess)   ; which of these is worst?
         (if excess (frob () excess)  ; choose the lesser weevil
             (and target placed (= (length target) (length placed))
                  (loop for new in target and old in placed
                     when (sufficiently-different? new old)
                     collect new into news and collect old into olds
                     finally (when news (frob news olds))))))))

;;; Websocket

(defun build-websocket-json (method &optional params)
  (with-output-to-string (string)
    (pprint-json string
                 `(("method" . ,method)
                   ,@(when params
                       `(("params" . ,params)))))))

(defun handle-websocket-raw (raw)
  (let* ((json:*json-array-type* 'vector)
         (json (read-json raw)))
    (flet ((arrange-json (json)
             (with-output-to-string (*standard-output*)
               (let ((*print-pretty* t)
                     (*print-right-margin* 67))
                 (pprint-json *standard-output* json)))))
      (case (caar json)
        (:|channel|
         (with-json-slots (channel data type) json
           (string-case (channel)
             ("heartbeat")
             ("status" (format t "~&WS Connected: ~A~%"
                               (arrange-json data)))
             (t (format t "~&WS ~A ~A Data:~%~A~%"
                        channel type (arrange-json data))))))
        (:|method|)))))

(defun create-public-websocket ()
  (let ((client (wsd:make-client "wss://ws.kraken.com/v2")))
    (wsd:on :message client 'handle-websocket-raw)
    (wsd:start-connection client)
    (wsd:send client (build-websocket-json "ping"))
    client))

(defun websocket-market-name (market)
  (with-slots (primary counter) market
    (flet ((fiat-name (asset &aux (string (name asset)))
             (cond
               ((char= (char string 0) #\Z)
                (subseq string 1))
               ((string= string "XXBT") "BTC")
               (t string))))
      (format nil "~A/~A" (fiat-name primary) (fiat-name counter)))))

(defun subscribe-public-book (client market &optional depth)
  (wsd:send client (build-websocket-json
                    "subscribe"
                    `(("channel" . "book")
                      ("symbol" . #(,(websocket-market-name market)))
                      ,@(when depth
                          `(("depth" . ,(prin1-to-string depth)))))))
  client)

(defun subscribe-public-trades (client market)
  (wsd:send client (build-websocket-json
                    "subscribe"
                    `(("channel" . "trade")
                      ("symbol" . #(,(websocket-market-name market))))))
  client)

(defun subscribe-public-data (client market &optional depth)
  (subscribe-public-trades client market)
  (if (null depth) (subscribe-public-book client market)
      (subscribe-public-book client market depth)))

(defun private-websocket-token (gate)
  (getjso "token" (gate-request gate "GetWebSocketsToken")))
