(defpackage #:scalpl.bitmex
  (:nicknames #:bitmex) (:export #:*bitmex* #:bitmex-gate)
  (:use #:cl #:base64 #:chanl #:anaphora #:local-time #:scalpl.util
        #:scalpl.actor #:scalpl.exchange))

(in-package #:scalpl.bitmex)

;;; General Parameters
(defparameter *base-url* "https://www.bitmex.com")
(defparameter *base-path* "/api/v1/")
(setf cl+ssl:*make-ssl-client-stream-verify-default* ())

(defvar *bitmex* (make-instance 'exchange :name :bitmex :sensitivity 1))

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
  (multiple-value-bind (body status headers)
      (apply #'http-request (concatenate 'string *base-url* path) args)
    (sleep (aif (getjso :x-ratelimit-remaining headers)
                (/ 42 (parse-integer it)) 1))
    (if (= status 200) (values (decode-json body) 200)
        (values () status (if (member status '(502 504)) body
                              (getjso "error" (decode-json body)))))))

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

(defmethod trades-since ((market bitmex-market) &optional since
                         &aux (pair (name market)))
  (awhen (public-request
          "trade" `(("symbol" . ,pair) ("count" . 200)
                    ("startTime"
                     . ,(format-timestring
                         () (if since (timestamp+ (timestamp since) 1 :sec)
                                (timestamp- (now) 1 :hour))
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
              (with-json-slots
                  (symbol side price (oid "orderID") (size "orderQty")) data
                (let ((market (find-market symbol :bitmex))
                      (aksp (string-equal side "Sell")))
                  (make-instance 'placed :oid oid :market market
                                 :volume (/ size price)
                                 :price (* price (if aksp 1 -1)
                                           (expt 10 (decimals market)))))))
            it)))

(defmethod account-positions ((gate bitmex-gate))
  (mapcar (lambda (position)
            (with-json-slots ((entry "avgEntryPrice") symbol
                              (size "currentQty") (cost "posCost"))
                position
              (with-aslots (primary counter) (find-market symbol :bitmex)
                (list it (cons-mp* it (* entry (- (signum size))))
                      ;; TODO: this currently assumes the position is in
                      ;; the perpetual inverse swap aka XBTUSD
                      (cons-aq primary (- cost)) (cons-aq counter (- size))))))
          (remove-if-not (getjso "isOpen")
                         (gate-request gate '(:get "position") ()))))

(defmethod account-balances ((gate bitmex-gate) &aux balances)
  ;; tl;dr - transubstantiates position into 'balances' of long + short
  (let ((positions (account-positions gate))
        (deposit (gate-request gate '(:get "user/wallet") ()))
        (instruments (public-request "instrument/active" ())))
    (dolist (instrument instruments balances)
      (with-json-slots (symbol (mark "markPrice")) instrument
        (with-aslots (primary counter metallic) (find-market symbol :bitmex)
          (let ((fund (/ (* 10 (getjso "amount" deposit)) ; ick
                         (if metallic (expt 10 (decimals primary))
                             (* mark (expt 10 (decimals counter)))))))
            (flet ((collect (a b) (push a balances) (push b balances)))
              (aif (find it positions :key #'car)
                   (collect (aq+ (cons-aq* primary fund) (third it))
                     (aq+ (cons-aq* counter (* fund mark)) (fourth it)))
                   (collect (cons-aq* primary fund)
                     (cons-aq* counter (* fund mark)))))))))))

(defmethod market-fee ((gate bitmex-gate) market) (fee market))

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
                          (from "startTime" (subseq (princ-to-string from) 0 19))
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
                `(("symbol" . ,market) ("price" . ,(princ-to-string price))
                  ("orderQty" . ,(princ-to-string (* (if buyp 1 -1) size)))
                  ("execInst" . "ParticipateDoNotInitiate"))))

(defmethod post-offer ((gate bitmex-gate) offer)
  (with-slots (market volume price) offer
    (let ((factor (expt 10 (decimals market))))
      (with-json-slots ((oid "orderID") (status "ordStatus") text)
          (post-raw-limit gate (not (plusp price)) (name market)
                          (multiple-value-bind (int dec)
                              (floor (abs price) factor)
                            (format nil "~D.~V,'0D"
                                    int (decimals market) dec))
                          (floor (* volume (if (minusp price) 1
                                               (/ price factor)))))
        (if (equal status "New") (change-class offer 'placed :oid oid)
            (unless (search "ParticipateDoNotInitiate" text)
              (warn "Failed placing: ~S~%~A" offer text)))))))

(defmethod cancel-offer ((gate bitmex-gate) (offer placed))
  (multiple-value-bind (ret err)
      (gate-request gate '(:delete "order") `(("orderID" . ,(oid offer))))
    (string-case ((getjso "ordStatus" (car ret))) ("Canceled") ("Filled")
                 (t (equal err "Not Found")))))

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
