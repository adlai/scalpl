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
         (sig (funcall signer
                       (concatenate 'string verb path nonce
                                    (unless (string= verb "GET") data)))))
    (multiple-value-bind (json status)
        (apply #'http-request (concatenate 'string *base-url* path)
               :url-encoder (lambda (url format) (declare (ignore format)) url)
               :method (intern (or (string verb) "GET") :keyword)
               :additional-headers `(("api-key" . ,key)
                                     ("api-nonce" . ,nonce)
                                     ("api-signature" . ,sig))
               (unless (equal verb "GET") `(:content ,data)))
      (values (decode-json json) status))))

(defun get-info (&aux assets)
  (awhen (public-request "instrument/active" ())
    (flet ((ensure-asset (name)
             (or (find name assets :key #'name :test #'string=)
                 (aprog1 (make-instance 'asset :name name :decimals 0)
                   (push it assets)))))
      (values (mapcar (lambda (pair)
                        (with-json-slots ((tick "tickSize") (fee "takerFee") symbol
                                          underlying (counter "quoteCurrency"))
                            pair
                          (make-instance
                           'bitmex-market :name symbol :fee fee
                           :decimals (round (- (log tick 10)))
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
            (t (list (warn (cdr (assoc :|message| (cdr (assoc :|error| ret)))))
                     (cdr (assoc :|error| ret))))))))

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
                                :price (* (expt 10 (decimals market))
                                          (cdr (assoc :|price| raw)))
                                :volume (cdr (assoc :|size| raw)))
     do (with-slots (given taken) offer
          (setf given (cons-aq (asset given) (cdr (assoc :|size| raw)))
                taken (cons-aq (asset taken) (cdr (assoc :|size| raw)))))
     if (eq type 'ask) collect offer into asks
     if (eq type 'bid) collect offer into bids
     finally (return (values (nreverse asks) bids))))

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

;;;
;;; Private Data API
;;;

(defmethod placed-offers ((gate bitmex-gate))
  (awhen (gate-request gate '("GET" "order") '(("filter" . "{\"open\": true}")))
    (mapcar (lambda (data)
              (flet ((value (key) (cdr (assoc key data)))) ; that smelly smell!
                (let ((market (find-market (value :|symbol|) :bitmex))
                      (aksp (string-equal (value :|side|) "Sell"))
                      (size (value :|orderQty|)))
                  (aprog1
                      (make-instance 'placed :oid (value :|orderID|)
                                     :market market :volume size
                                     :price (* (value :|price|) (if aksp 1 -1)
                                               (expt 10 (decimals market))))
                    (with-slots (given taken) it
                      (setf given (cons-aq (asset given) size)
                            taken (cons-aq (asset taken) size)))))))
            it)))

(defmethod account-balances ((gate bitmex-gate)) ; ASSUMES offer atomicity!
  (let ((placed (placed-offers gate))
        (funds (make-hash-table :size (length (assets *bitmex*)))))
    (flet ((incf-fund (asset amount) (incf (gethash asset funds 0) amount)))
      (dolist (offer placed)
        (if (eq (consumed-asset offer) (primary (market offer)))
            (incf-fund (consumed-asset offer) (volume offer))
            (incf-fund (counter (market offer))
                       (* (volume offer) (- (price offer))
                          (expt 1/10 (decimals (market offer))))))))
    (mapcan (lambda (pair)
              (awhen (find-asset (car pair) *bitmex*)
                (list (cons-aq* it (+ (cdr pair) (gethash it funds 0))))))
            (extract-funds (error "todo");; (gate-request gate "getInfo")
                           ))))

(defmethod market-fee ((bitmex-gate t) market) (fee market))

(defun parse-execution (raw)
  (flet ((value (key) (cdr (assoc key raw)))) ; FIXME
    (let ((market (find-market (value :|symbol|) :bitmex)))
      (make-instance 'execution :direction (value :|side|)
                     :oid (value :|orderID|) :txid (value :|execID|)
                     :price (* (value :|price|) (expt 10 (decimals market)))
                     :cost (error "fixme") :volume (value :|orderQty|)
                     :timestamp (parse-timestamp *bitmex* (value :|timestamp|))
                     :net-volume (error "fixme") :net-cost (error "metoo")))))

(defun raw-executions (gate &key pair from end count)
  (macrolet ((params (&body body)
               `(append ,@(loop for (val key exp) in body
                             collect `(when ,val `((,,key . ,,exp))))
                        '(("reverse" . "true"))))) ; most recent first
    (gate-request gate '("GET" "execution/tradeHistory")
                  (params (pair "symbol" pair) (count "count" count)
                          (from "startTime" (subseq (princ-to-string from) 0 19))
                          (end "endTime" (subseq (princ-to-string end) 0 19))))))

(defmethod execution-since ((gate bitmex-gate) market since)
  (awhen (raw-executions gate :pair (name market)
                         :from (if since (timestamp since)
                                   (timestamp- (now) 1 :hour)))
    (mapcar #'parse-execution
            (if (null since) it
                (subseq it 0 (position (txid since) it
                                       :test #'string= :key #'cdar))))))

(defun executions-until (gate market until)
  (let ((txid (when until (txid until))))
    (awhen (raw-executions gate :pair (name market) :end txid)
      ;; btce's end_id is inclusive, although just using #'rest will bug out
      ;; in the case where end_id was in a different market. thus, #'remove
      (remove txid (mapcar-jso #'parse-execution it) :key #'txid))))

(defun post-raw-limit (gate buyp market price size)
  (gate-request gate '("POST" "order")
                `(("symbol" . ,market) ("price" . ,(princ-to-string price))
                  ("orderQty" . ,(princ-to-string (* (if buyp 1 -1) size)))
                  ("execInst" . "ParticipateDoNotInitiate"))))

(defmethod post-offer ((gate bitmex-gate) offer)
  ;; (format t "~&place  ~A~%" offer)
  (with-slots (market volume price) offer
    (awhen (post-raw-limit gate (not (plusp price)) (name market)
                           (multiple-value-bind (int dec)
                               (floor (abs price)
                                      (expt 10 (decimals market)))
                             (format nil "~D.~V,'0D"
                                     int (decimals market) dec))
                           volume)
      (change-class offer 'placed :oid (cdr (assoc :|orderID| it))))))

(defmethod cancel-offer ((gate bitmex-gate) (offer placed))
  ;; (format t "~&cancel ~A~%" offer)
  (multiple-value-bind (ret err)
      (gate-request gate '("DELETE" "order") `(("orderID" . ,(oid offer))))
    (or (string= (cdr (assoc :|ordStatus| (car ret))) "Canceled")
        (string= (cdr (assoc :|message| err)) "Not Found"))))
