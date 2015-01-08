(defpackage #:scalpl.btce
  (:nicknames #:btce)
  (:export #:*btce* #:btce-gate)
  (:use #:cl #:chanl #:anaphora #:st-json #:local-time #:scalpl.util #:scalpl.exchange))

(in-package #:scalpl.btce)

;;; General Parameters
(defparameter +base-path+ "https://btc-e.com/")
(defparameter +public-stub+ "api/3/")
(defparameter +private-stub+ "tapi")

(defun hmac-sha512 (message secret)
  (let ((hmac (ironclad:make-hmac secret 'ironclad:sha512)))
    (ironclad:update-hmac hmac message)
    (ironclad:hmac-digest hmac)))

;;; Key = API key
;;; Sign = POST-parameters (?nonce=1&param0=val0), signed with a Secret key using HMAC-SHA512

(defgeneric make-signer (secret)
  (:method ((signer function)) signer)
  (:method ((secret array))
    (lambda (data)
      (apply #'concatenate 'string
             (map 'list (lambda (byte) (string-downcase (format nil "~2,'0X" byte)))
                  (hmac-sha512 (map '(simple-array (unsigned-byte 8) (*)) 'char-code data)
                               secret)))))
  (:method ((secret string))
    (make-signer (map '(simple-array (unsigned-byte 8) (*)) 'char-code secret)))
  (:method ((stream stream)) (make-signer (read-line stream)))
  (:method ((path pathname)) (with-open-file (stream path) (make-signer stream))))

(defgeneric make-key (key)
  (:method ((key string)) key)
  (:method ((stream stream)) (read-line stream))
  (:method ((path pathname))
    (with-open-file (stream path)
      (make-key stream))))

;;; FIXME: URGENT NEED OF CLEANUP
;;; all this needs to be reflected in the exchange objects
;;;    to reduce code duplication. time for scalpl.net?

(defun raw-request (path &rest keys)
  (handler-case
      (multiple-value-bind (body status)
          (apply #'drakma:http-request
                 (concatenate 'string +base-path+ path)
                 ;; Mystery crash on the morning of 2014-06-04
                 ;; entered an infinite loop of usocket:timeout-error
                 ;; lasted for hours, continued upon restart
                 ;; other programs on the same computer not affected - just sbcl
                 :connection-timeout 60
                 keys)
        (case status
          (200 (read-json body))
          ;; (404 (with-json-slots (result error)
          ;;          (read-json (map 'string 'code-char body))
          ;;        (format t "~&Aborting after 404...~%")
          ;;        (describe error)
          ;;        (values result error))
          ;;      (values nil t))
          ;; (502 (format t "~&Retrying after 502...~%")
          ;;      (sleep 2)
          ;;      (apply #'raw-request path keys))
          (t (error "HTTP Error ~D~%~A" status body))))
    ((or usocket:timeout-error simple-error cl+ssl::ssl-error-syscall) (e)
      (describe e)
      (sleep 2)
      (apply #'raw-request path keys))))

(defun get-request (path &optional data)
  (raw-request (concatenate 'string +public-stub+ path "?"
                            (urlencode-params data))))

(defconstant +kludge+ -1419931763719)   ; sometimes one can!

(defun nonce (&aux (now (now)))
  (princ-to-string (+ (floor (nsec-of now) 1000000)
                      (* 1000 (timestamp-to-unix now))
                      +kludge+)))

(defun post-request (method key signer &optional params &aux (nonce (nonce)))
  (push (cons "method" method) params)
  (push (cons "nonce" nonce) params)
  (let ((data (urlencode-params params)))
    (raw-request (concatenate 'string +private-stub+)
                 :method :post :content data
                 :additional-headers `(("Key"  . ,key)
                                       ("Sign" . ,(funcall signer data))))))

(defclass btce-market (market)
  ((hidden :initarg :hidden :reader hidden)
   (epsilon :initarg :epsilon :reader epsilon)
   (fee :initarg :fee :reader fee)
   (minimum :initarg :minimum :reader minimum)
   (maximum :initarg :maximum :reader maximum)))

(defun get-info (&aux assets)
  (awhen (get-request "info")
    (flet ((ensure-asset (name)
             (or (find name assets :key #'name :test #'string=)
                 (aprog1 (make-instance 'asset :name name :decimals 8)
                   (push it assets)))))
      (values (mapcar-jso (lambda (pair data)
                            (with-json-slots ((epsilon  "min_amount")
                                              (maximum  "max_price")
                                              (minimum  "min_price")
                                              (decimals "decimal_places")
                                              fee hidden)
                                data
                              (make-instance 'btce-market :name pair
                                             :decimals decimals :fee fee
                                             :minimum minimum :maximum maximum
                                             :epsilon epsilon :hidden hidden
                                             :primary (ensure-asset (subseq pair 0 3))
                                             :counter (ensure-asset (subseq pair 4)))))
                          (getjso "pairs" it))
              assets))))

(defvar *btce*
  (multiple-value-bind (markets assets) (get-info)
    (make-instance 'exchange :name :btc-e :assets assets :markets markets)))

(defclass btce-gate (gate) ())

(defmethod shared-initialize ((gate btce-gate) names &key pubkey secret)
  (multiple-value-call #'call-next-method gate names
                       (when pubkey (values :pubkey (make-key pubkey)))
                       (when secret (values :secret (make-signer secret)))))

(defmethod gate-post ((gate btce-gate) key secret request)
  (destructuring-bind (command . options) request
    (awhen (post-request command key secret options)
      (with-json-slots (success return error) it
        (if (zerop success) (list (warn error) error) (list return))))))

;;;
;;; Public Data API
;;;

(defmethod get-book ((market btce-market) &key (count 200)
                     &aux (pair (name market)) (decimals (decimals market)))
  (awhen (get-request (format nil "depth/~A" pair)
                      `(("limit" . ,(princ-to-string count))))
    (with-json-slots (bids asks) (getjso pair it)
      (flet ((parser (class)
               (lambda (raw-order)
                 (destructuring-bind (price amount) raw-order
                   (make-instance class :market market
                                  :price (round (* price (expt 10 decimals)))
                                  :volume amount)))))
        (values (mapcar (parser 'ask) asks)
                (mapcar (parser 'bid) bids))))))

(defun trade-parser (market)
  (lambda (trade)
    (with-json-slots (price amount timestamp type tid) trade
      (make-instance 'trade :market market :direction type :txid tid
                     :timestamp (parse-timestamp *btce* timestamp)
                     ;; FIXME - "cost" later gets treated as precise
                     :volume amount :price price :cost (* amount price)))))

;;; btce only lets us specify a number of trades to fetch, not a last seen trade
;;; so we'll at least warn when we detect a gap
(defmethod trades-since ((market btce-market) &optional since)
  (when since (assert (eq market (market since))))
  ;; TODO estimate count based on time delta
  (awhen (get-request (format nil "trades/~A" (name market)))
    (aprog1 (reverse (mapcar (trade-parser market) (getjso (name market) it)))
      (when since
        (awhen (member (txid since) it :key #'txid)
          (return-from trades-since (rest it)))
        (warn "missing trades since ~A" since)))))

;;;
;;; Private Data API
;;;

(defmethod placed-offers ((gate btce-gate))
  (awhen (gate-request gate "ActiveOrders")
    (mapcar-jso (lambda (id data)
                  (with-json-slots (pair type amount rate timestamp_created) data
                    (let* ((market (find-market pair *btce*))
                           (decimals (slot-value market 'decimals))
                           (price-int (round (* rate (expt 10 decimals)))))
                      (make-instance 'placed :oid (parse-integer id)
                                     :market market :volume amount
                                     :price (if (string= type "buy")
                                                (- price-int) price-int)))))
                it)))

(defun extract-funds (funds)
  (mapcar-jso #'cons (getjso "funds" funds)))

(defun available-balance (gate)
  (awhen (gate-request gate "getInfo")
    (extract-funds it)))

(defmethod account-balances ((gate btce-gate))
  (awhen (gate-request gate "getInfo")  ; ASSUMES that these two API calls have
    (let ((placed (placed-offers gate)) ; nothing happen in between them!!!!!!!
          (funds (make-hash-table :size (length (assets *btce*)))))
      (flet ((incf-fund (asset amount) (incf (gethash asset funds 0) amount)))
        (dolist (offer placed)
          (if (eq (consumed-asset offer) (primary (market offer)))
              (incf-fund (consumed-asset offer) (volume offer))
              (incf-fund (counter (market offer))
                         (* (volume offer) (- (price offer))
                            (expt 1/10 (decimals (market offer))))))))
      (mapcar (lambda (pair)
                (aif (gethash (find-asset (car pair) *btce*) funds)
                     (cons (car pair) (+ (cdr pair) it))
                     pair))
              (extract-funds it)))))

;;; they haven't heard of volume discounts yet...
;;; actually, they have! https://btc-e.com/news/216
(defmethod market-fee (gate (market btce-market)) (fee market))

(defun parse-execution (txid json)
  (with-json-slots ((oid "order_id") (wtfp "is_your_order")
                    timestamp rate amount type pair) json
    (let* ((market (find-market pair *btce*))
           ;; btce deducts fees from the earned asset
           (after-fee (- 1 (/ (market-fee nil market) 100)))
           (cost (* rate amount)))
      (make-instance 'execution :direction type
                     :oid oid :txid (parse-integer txid)
                     :price rate :cost cost
                     :volume amount :market market
                     :net-volume (string-case (type)
                                   ("buy" (* amount after-fee))
                                   ("sell" amount))
                     :timestamp (parse-timestamp *btce* timestamp)
                     :net-cost (string-case (type)
                                 ("buy" cost)
                                 ("sell" (* cost after-fee)))))))

(defun raw-executions (gate &optional symbol last)
  (gate-request gate "TradeHistory"
                `(,@(when symbol `(("pair"    . ,symbol)))
                  ,@(when last   `(("from_id" . ,(princ-to-string last)))))))

(defmethod execution-since ((gate btce-gate) (market btce-market) since)
  (let ((txid (when since (txid since))))
    (awhen (raw-executions gate (name market) txid)
      ;; btce's from_id is inclusive, although just using #'rest will bug out
      ;; in the case where from_id was in a different market. thus, #'remove
      (remove txid (mapcar-jso #'parse-execution it) :key #'txid))))

(defun post-raw-limit (gate type market price volume)
  (gate-request gate "Trade"
                `(("pair"   . ,market)
                  ("type"   . ,type)
                  ("rate"   . ,price)
                  ("amount" . ,volume))))

(defmethod post-offer ((gate btce-gate) offer)
  ;; (format t "~&place  ~A~%" offer)
  (with-slots (market volume price) offer
    (flet ((post (type primary-volume)
             (awhen (post-raw-limit gate type (name market)
                                    (multiple-value-bind (int dec)
                                        (floor (abs price)
                                               (expt 10 (decimals market)))
                                      (format nil "~D.~V,'0D"
                                              int (decimals market) dec))
                                    (format nil "~8$" primary-volume))
               (with-json-slots (order_id #|received remains funds|#) it
                 ;; TODO: use all of these, for better metadata snr
                 (unless (zerop order_id) ; already executed
                   (change-class offer 'placed :oid order_id))))))
      (if (plusp price)
          (post "sell" volume)
          (post "buy" (/ volume (/ (abs price) (expt 10 (decimals market)))))))))

(defun cancel-raw-order (gate oid)
  (gate-request gate "CancelOrder" `(("order_id" . ,oid))))

(defmethod cancel-offer ((gate btce-gate) offer)
  ;; (format t "~&cancel ~A~%" offer)
  (multiple-value-bind (ret err)
      (cancel-raw-order gate (princ-to-string (oid offer)))
    (or ret (string= err "bad status"))))
