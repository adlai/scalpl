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
   (metallic :initarg :metallic)))

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
  (multiple-value-bind (json status headers)
      (apply #'http-request (concatenate 'string *base-url* path) args)
    (let ((a (parse-integer (getjso :x-ratelimit-remaining headers))))
      (let ((data (decode-json json)) (b (isqrt a)))
        (when (and (ironclad:prime-p b) (= a (* b b))) (sleep (isqrt b)))
        (if (= status 200) (values data 200)
            (values () status (cdr (assoc :error data))))))))

(defun bitmex-path (&rest paths)
  (apply #'concatenate 'string *base-path* paths))

(defun public-request (method parameters)
  (bitmex-request
   (apply #'bitmex-path method
          (and parameters `("?" ,(net.aserve:uridecode-string
                                  (urlencode-params parameters)))))))

(defun auth-request (verb method key signer &optional params)
  (let* ((data (urlencode-params params))
         (path (apply #'bitmex-path method
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
  (declare (optimize debug))
  (awhen (public-request "instrument/active" ())
    (flet ((make-market (instrument)
             (with-json-slots
                 ((tick "tickSize") (lot "lotSize") (fee "takerFee")
                  (name "symbol") (fe "isInverse") (long "rootSymbol")
                  (short "quoteCurrency") multiplier)
                 instrument
               (flet ((asset (fake &optional (decimals 0))
                        (let ((name (concatenate 'string fake "-" name)))
                          (or (find name assets :key #'name :test #'string=)
                              (aprog1 (make-instance 'asset :name name
                                                     :decimals decimals)
                                (push it assets)))))
                      (ilog (i) (round (log (abs i) 10))))
                 (make-instance
                  'bitmex-market :name name :fee fee :metallic fe
                  :decimals (- (ilog tick))
                  :primary (asset long (ilog (if fe multiplier lot)))
                  :counter (asset short (ilog (if fe lot multiplier))))))))
      (values (mapcar #'make-market it) assets))))

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
     for price = (getjso "price" raw)
     for type = (string-case ((getjso "side" raw)) ("Sell" 'ask) ("Buy" 'bid))
     for offer = (make-instance type :market market
                                :price (* (expt 10 (decimals market)) price)
                                :volume (/ (getjso "size" raw) price))
     if (eq type 'ask) collect offer into asks
     if (eq type 'bid) collect offer into bids
     finally (return (values (nreverse asks) bids))))

(defmethod trades-since ((market bitmex-market) &optional since
                         &aux (pair (name market)))
  (awhen (public-request
          "trade" `(("symbol" . ,pair)
                    ("startTime"
                     . ,(format-timestring
                         () (or (timestamp since) (timestamp- (now) 1 :hour))
                         :format (butlast +iso-8601-format+ 3)))))
    (mapcar (lambda (trade)
              (with-json-slots (side timestamp size price) trade
                (make-instance 'trade :market market :direction side
                               :timestamp (parse-timestring timestamp)
                               :volume (/ size price) :price price :cost size)))
            (butlast it))))

;;;
;;; Private Data API
;;;

(defmethod placed-offers ((gate bitmex-gate))
  (awhen (gate-request gate '(:get "order") '(("filter" . "{\"open\": true}")))
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

(defmethod account-balances ((gate bitmex-gate) &aux balances)
  (declare (optimize debug))
  ;; tl;dr - transubstantiates position into 'balances' of long + short
  (let ((deposit (gate-request gate '(:get "user/wallet") ()))
        (positions (gate-request gate '(:get "position") ()))
        (instruments (public-request "instrument/active" ())))
    (dolist (instrument instruments balances)
      (with-json-slots (symbol (mark "markPrice")) instrument
        (with-slots (primary counter metallic) (find-market symbol :bitmex)
          (let* ((delta 0)
                 (fund (/ (* 10 (getjso "amount" deposit)) (if metallic 1 mark)
                          (expt 10 (decimals (if metallic primary counter))))))
            (awhen (find symbol positions :test #'string= :key (getjso "symbol"))
              (incf delta (/ (getjso "currentQty" it) mark)))
            (push (cons-aq* primary (+ fund delta)) balances)
            (push (cons-aq* counter (* (- fund delta) mark))
                  balances)))))))

(defmethod market-fee ((bitmex-gate t) market) (fee market))

(defun parse-execution (raw)
  (with-json-slots
      ((oid "orderID") (txid "execID") (amt "orderQty")
       symbol side price timestamp) raw
    (let ((market (find-market symbol :bitmex)))
      (make-instance 'execution :direction side :oid oid :txid txid
                     :price (* price (expt 10 (decimals market)))
                     :cost (error "fixme") :volume (getjso "orderQty" raw)
                     :timestamp (parse-timestamp *bitmex* (getjso "timestamp" raw))
                     :net-volume (error "fixme") :net-cost (error "metoo")))))

(defun raw-executions (gate &key pair from end count)
  (macrolet ((params (&body body)
               `(append ,@(loop for (val key exp) in body
                             collect `(when ,val `((,,key . ,,exp))))
                        '(("reverse" . "true"))))) ; most recent first
    (gate-request gate '(:get "execution/tradeHistory")
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
  (gate-request gate '(:post "order")
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
      (gate-request gate '(:delete "order") `(("orderID" . ,(oid offer))))
    (or (string= (cdr (assoc :|ordStatus| (car ret))) "Canceled")
        (string= (cdr (assoc :|message| err)) "Not Found"))))
