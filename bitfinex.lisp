(defpackage #:scalpl.bitfinex
  (:nicknames #:bitfinex)
  (:export #:*bitfinex* #:bitfinex-gate)
  (:use #:cl #:anaphora #:local-time #:base64 #:scalpl.util #:scalpl.exchange))

(in-package #:scalpl.bitfinex)

;;; General Parameters
(defparameter +base-path+ "https://api.bitfinex.com/v1/")

(defun hmac-sha384 (message secret)
  (let ((hmac (ironclad:make-hmac secret 'ironclad:sha384)))
    (ironclad:update-hmac hmac (string-octets message))
    (ironclad:octets-to-integer (ironclad:hmac-digest hmac))))

;;; X-BFX-APIKEY = API key
;;; X-BFX-PAYLOAD = base64(json(request path, nonce, parameters...))
;;; X-BFX-SIGNATURE = Message signature using HMAC-SHA384 of payload and base64 decoded secret

(defconstant +kludge+ -1418334400000)   ; sometimes one cannot even

;;; generate max 1 nonce per second
(defvar *last-nonce* (now))

(defun nonce (&aux (now (now)) (delta (timestamp-difference now *last-nonce*)))
  (when (> 1 delta) (sleep (- 1 delta)))
  (princ-to-string (+ (floor (nsec-of now) 1000000)
                      (* 1000 (timestamp-to-unix now))
                      +kludge+)))

;;; some methods accept timestamps as just an int, but for some bizarre reason,
;;; "history" shits on anything other than this syntax. frankley, i can't even.
(defun flotsam (timestamp)
  (format nil "~A.0" (timestamp-to-unix timestamp)))

(defun make-payload (data &optional path)
  (string-to-base64-string
   (cl-json:encode-json-alist-to-string
    `(("request" . ,path) ("nonce" . ,(nonce)) ,@data))))

(defgeneric make-signer (secret)
  (:method ((signer function)) signer)
  (:method ((array array))
    (lambda (data) (format nil "~(~96,'0X~)" (hmac-sha384 data array))))
  (:method ((string string)) (make-signer (string-octets string)))
  (:method ((stream stream)) (make-signer (read-line stream)))
  (:method ((path pathname)) (with-open-file (data path) (make-signer data))))

(defgeneric make-key (key)
  (:method ((key string)) key)
  (:method ((stream stream)) (read-line stream))
  (:method ((path pathname))
    (with-open-file (stream path)
      (make-key stream))))

(defun decode-json (arg) (read-json (map 'string 'code-char arg)))

(defun raw-request (path &rest keys)
  (multiple-value-bind (body status)
      (apply #'http-request (concatenate 'string +base-path+ path) keys)
    (case status
      (200 (decode-json body))
      ((400 404) (values nil (decode-json body)))
      ((500 502 503 504 520 522 524)
       (sleep 2) (apply #'raw-request path keys))
      (t (cerror "Retry request" "HTTP Error ~D~%~A" status body)
         (apply #'raw-request path keys)))))

(defun get-request (path &optional data)
  (raw-request (concatenate 'string path "/?" (urlencode-params data))))

(defun post-request (method key signer &optional data)
  (let* ((path (concatenate 'string "/v1/" method))
         (payload (make-payload data path)))
    (raw-request method :method :post :additional-headers
                 `(("X-BFX-APIKEY"  . ,key)
                   ("X-BFX-PAYLOAD" . ,payload)
                   ("X-BFX-SIGNATURE" . ,(funcall signer payload))))))

(defun get-assets ()
  (mapcar (lambda (name) (make-instance 'asset :name name :decimals 8))
          (delete-duplicates (mapcan (lambda (sym)
                                       (list (subseq sym 0 3) (subseq sym 3)))
                                     (get-request "symbols"))
                             :test #'string=)))

(defun detect-market-precision (name)
  (reduce 'max (with-json-slots (asks bids)
                   (get-request (format nil "book/~A" name))
                 (mapcar (lambda (offer &aux (price (getjso "price" offer)))
                           (- (length price) (position #\. price) 1))
                         (append (subseq bids 0 (floor (length bids) 2))
                                 (subseq asks 0 (floor (length asks) 2)))))))

(defclass bitfinex-market (market) ())  ; TODO is-a → has-a

(defun get-markets (assets &aux markets)
  (dolist (name (get-request "symbols") markets)
    (push (make-instance
           'bitfinex-market :name name
           :primary (find-asset (subseq name 0 3) assets)
           :counter (find-asset (subseq name 3) assets)
           :decimals (detect-market-precision name))
          markets)))

(defvar *bitfinex* (make-instance 'exchange :name :bitfinex :sensitivity 1))

(defmethod fetch-exchange-data ((exchange (eql *bitfinex*)))
  (with-slots (markets assets) exchange
    (setf assets (get-assets) markets (get-markets assets))))

(defclass bitfinex-gate (gate) ())

(defmethod gate-post ((gate bitfinex-gate) key secret request)
  (destructuring-bind (command . options) request
    (multiple-value-list (post-request command key secret options))))

;;; remind me, why am i even here?
(defmethod shared-initialize ((gate bitfinex-gate) names &key pubkey secret)
  (multiple-value-call #'call-next-method gate names
                       (mvwrap pubkey make-key) (mvwrap secret make-signer)))

;;;
;;; Public Data API
;;;

(defmethod get-book ((market bitfinex-market) &key (count 200)
                     &aux (pair (name market)))
  (let ((decimals (slot-value market 'decimals)))
    (with-json-slots (bids asks)
        (get-request (format nil "book/~A" pair)
                     (mapcar (lambda (str) (cons str (prin1-to-string count)))
                             '("limit_asks" "limit_bids")))
      (flet ((parser (class)
               (lambda (raw-order)
                 (with-json-slots (price amount) raw-order
                   (make-instance class :market market
                                  :price (parse-price price decimals)
                                  :volume (parse-float amount :type 'rational))))))
        (values (mapcar (parser 'ask) asks)
                (mapcar (parser 'bid) bids))))))

(defmethod trades-since ((market bitfinex-market) &optional since)
  (mapcar (lambda (trade)
            (with-json-slots (price amount timestamp type) trade
              (let ((price  (parse-float price))
                    (volume (parse-float amount :type 'rational)))
                (make-instance 'trade :market market :direction type
                               :timestamp (parse-timestamp *bitfinex* timestamp)
                               ;; FIXME - "cost" later gets treated as precise
                               :volume volume :price price :cost (* volume price)))))
          (reverse (get-request (format nil "trades/~A" (name market))
                                (when since
                                  `(("timestamp" . ,(princ-to-string (timestamp-to-unix (timestamp+ (timestamp since) 1 :sec))))))))))

;;;
;;; Private Data API
;;;

(defun open-orders (gate)
  (gate-request gate "orders"))

(defmethod placed-offers ((gate bitfinex-gate))
  (mapcar (lambda (offer)
            (with-json-slots (id symbol side price remaining_amount oflags) offer
              (let* ((market (find-market symbol *bitfinex*))
                     (decimals (slot-value market 'decimals))
                     (price-int (parse-price price decimals))
                     (volume (parse-float remaining_amount :type 'rational)))
                (make-instance 'placed :oid id :market market :volume volume
                               :price (if (string= side "buy") (- price-int) price-int)))))
          (open-orders gate)))

(defmethod account-balances ((gate bitfinex-gate))
  (aif (gate-request gate "balances")
       (mapcan (lambda (balance)
                 (with-json-slots (currency amount) balance
                   (awhen (find-asset currency *bitfinex*) ; vestigial scam th1
                     (list (cons-aq* it (parse-float amount :type 'number))))))
               (remove "exchange" it :test-not #'string= :key (getjso "type")))
       (error "communication breakdown")))

(defmethod market-fee ((gate bitfinex-gate) market)
  (awhen (car (gate-request gate "account_infos"))
    ;; from this point, we assume the json object contains a "fees" key
    (flet ((asset-fee (role)
             (find (name (slot-value market role))
                   (getjso "fees" it) :test #'string-equal
                   :key (lambda (x) (getjso "pairs" x)))))
      (awhen (or (asset-fee 'primary) (asset-fee 'counter))
        (parse-float (getjso "taker_fees" it))))))

(defun execution-parser (market)
  (lambda (json)
    (with-json-slots (price fee_amount fee_currency
                      amount timestamp type tid order_id) json
      (let* ((volume (parse-float amount))
             (price (parse-float price))
             (fee (parse-float fee_amount))
             (cost-fee (with-slots (primary counter) market
                         (cond
                           ((string-equal (name primary) fee_currency) nil)
                           ((string-equal (name counter) fee_currency)  t)
                           (t (error "something bad happened")))))
             (cost (* volume price)))
        (make-instance 'execution
                       :direction type
                       :cost cost
                       :txid tid :oid order_id
                       :price price
                       :volume volume
                       :fee fee
                       :net-cost (+ cost (if cost-fee fee 0))
                       :net-volume (+ volume (if cost-fee 0 fee))
                       :market market
                       :timestamp (parse-timestamp *bitfinex* timestamp))))))

(defun raw-executions (gate symbol &optional last)
  (gate-request gate "mytrades"
                `(("symbol" . ,symbol)
                  ,@(when last
                      `(("timestamp"    ; FIXME: raw-foo should get raw params
                         . ,(princ-to-string (timestamp-to-unix (timestamp last)))))))))

(defmethod execution-since ((gate bitfinex-gate) (market market) since)
  ;; bitfinex always returns the `since' trade, so discard it here
  (rest (nreverse (mapcar (execution-parser market)
                          (raw-executions gate (name market) since)))))

(defun raw-history (gate currency &key since until limit wallet)
  (macrolet ((maybe-include (option)
               `(when ,option `((,,(string-downcase option) . ,,option)))))
    (gate-request gate "history"
                  `(("currency" . ,currency)
                    ,@(maybe-include since)
                    ,@(maybe-include until)
                    ,@(maybe-include limit)
                    ,@(maybe-include wallet)))))

;;;
;;; Action API
;;;

(defun post-raw-limit (gate type pair amount price &optional casinop)
  (multiple-value-bind (info error)
      (gate-request gate "order/new"
                    `(("type" . ,(format nil "~:[exchange ~;~]limit" casinop))
                      ("exchange" . "bitfinex") ; lold habits
                      ("side" . ,type)
                      ("symbol" . ,pair)
                      ("amount" . ,amount)
                      ("price" . ,price)))
    (if error (warn (getjso "message" error)) info)))

(defun post-limit (gate type pair price volume decimals)
  (let ((price (/ price (expt 10d0 decimals))))
    ;; bitfinex always wants volume in the primary currency units
    ;; scalpl internally represents volume in the consumed currency units
    (when (string-equal type "buy") (setf volume (/ volume price)))
    ;; FIXME: these hardcoded numbers are btcusd-specific!
    (flet ((post (chunk)
             (post-raw-limit gate type pair
                             (format nil "~V$" 3 chunk)
                             (format nil "~V$" decimals price))))
      ;; minimal order is 0.01 btc
      (when (>= volume 0.01)
        ;; maximal order is 2000 btc
        (if (< volume 2000) (post volume) (error "FIXME: order too large"))))))

(defmethod post-offer ((gate bitfinex-gate) offer)
  (with-slots (market volume price) offer
    (flet ((post (type)
             (awhen (post-limit gate type (name market) (abs price) volume
                                (slot-value market 'decimals))
               (with-json-slots (order_id) it
                 (change-class offer 'placed :oid order_id)))))
      (post (if (< price 0) "buy" "sell")))))

;;; the order object returned will (always?) indicate that the order hasn't yet
;;; been cancelled; however, in situations where bfx has failed to cancel the
;;; order, we get 400 Bad Request + error message; so if we have any primary
;;; return value, we can treat that as a successful cancellation.
(defun cancel-order (gate oid)
  (typecase oid
    (integer (gate-request gate "order/cancel" `(("order_id" . ,oid))))
    ;; if any cancellation fails, notany #'null ensures that we'll try again
    (list (notany #'null (mapcar (lambda (id) (cancel-order gate id)) oid)))))

(defmethod cancel-offer ((gate bitfinex-gate) offer)
  ;; (format t "~&cancel ~A~%" offer)
  (multiple-value-bind (ret err) (cancel-order gate (oid offer))
    (or ret (string= "Order could not be cancelled." (getjso "message" err)))))

;;; to do this one properly, we should reuse post-[raw-]limit
;; (defmethod replace-offer ((gate bitfinex-gate) old new)
;;   "(and (cancel-offer gate old) (post-offer gate new))"
;;   (gate-request gate "order/cancel/replace"
;;                 `(("type" . "exchange limit")
;;                   ("exchange" . "bitfinex") ; die hard
;;                   ("side" . ,type)
;;                   ("symbol" . ,pair)
;;                   ("amount" . ,amount)
;;                   ("price" . ,price)
;;                   ("order_id" . ,(oid old)))))

;;; Casino

(defclass leverage-gate (bitfinex-gate)
  ((gate :initarg :gate) (pair :initarg :pair)
   (leverage :initarg :leverage :initform 1)))
