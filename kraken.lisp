(defpackage #:scalpl.kraken
  (:nicknames #:kraken)
  (:export #:*kraken* #:kraken-gate)
  (:use #:cl #:chanl #:anaphora #:st-json #:local-time #:scalpl.util #:scalpl.exchange))

(in-package #:scalpl.kraken)

;;; General Parameters
(defparameter +base-path+ "https://api.kraken.com/0/")

(defun hmac-sha512 (message secret)
  (let ((hmac (ironclad:make-hmac secret 'ironclad:sha512)))
    (ironclad:update-hmac hmac message)
    (base64:usb8-array-to-base64-string (ironclad:hmac-digest hmac))))

(defun hash-sha256 (message)
  (ironclad:digest-sequence :sha256 message))

;;; API-Key = API key
;;; API-Sign = Message signature using HMAC-SHA512 of (URI path + SHA256(nonce + POST data)) and base64 decoded secret API key

(defgeneric make-signer (secret)
  (:method ((signer function)) signer)
  (:method ((secret string))
    (lambda (path data nonce)
      (hmac-sha512 (concatenate '(simple-array (unsigned-byte 8) (*))
                                (map '(simple-array (unsigned-byte 8) (*)) 'char-code path)
                                (hash-sha256 (map '(simple-array (unsigned-byte 8) (*))
                                                  'char-code
                                                  (concatenate 'string nonce (urlencode-params data)))))
                   (base64:base64-string-to-usb8-array secret))))
  (:method ((stream stream)) (make-signer (read-line stream)))
  (:method ((path pathname)) (with-open-file (stream path) (make-signer stream))))

(defgeneric make-key (key)
  (:method ((key string)) key)
  (:method ((stream stream)) (read-line stream))
  (:method ((path pathname))
    (with-open-file (stream path)
      (make-key stream))))

(defun raw-request (path &rest keys)
  (handler-case
      (handler-bind ((drakma::simple-error #'describe))
        ;; (setf *break-on-signals* 'drakma::simple-error)
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
            (200 (with-json-slots (result error)
                     (read-json (map 'string 'code-char body))
                   (when error (mapc #'warn error))
                   (values result error)))
            (404 (with-json-slots (result error)
                     (read-json (map 'string 'code-char body))
                   (format t "~&Aborting after 404...~%")
                   (describe error)
                   (values result error))
                 (values nil t))
            (502 (format t "~&Retrying after 502...~%")
                 (sleep 1)
                 (apply #'raw-request path keys))
            (t (cerror "Retry request" "HTTP Error ~D" status)
               (apply #'raw-request path keys)))))
    (drakma::drakma-simple-error ()
      (format t "~&Retrying after drakma SIMPLE crap...~%")
      (sleep 1)
      (apply #'raw-request path keys))
    (drakma::simple-error ()
      (format t "~&Retrying after drakma crap...~%")
      (sleep 1)
      (apply #'raw-request path keys))
    (cl+ssl::ssl-error-ssl ()
      (format t "~&Retrying after cl+ssl ssl error crap...~%")
      (sleep 1)
      (apply #'raw-request path keys))
    (cl+ssl::ssl-error-zero-return ()
      (format t "~&Retrying after cl+ssl crap...~%")
      (sleep 1)
      (apply #'raw-request path keys))
    (END-OF-FILE ()
      (format t "~&Retrying after eof crap...~%")
      (sleep 1)
      (apply #'raw-request path keys))
    (CHUNGA:INPUT-CHUNKING-UNEXPECTED-END-OF-FILE ()
      (format t "~&Retrying after chunga eof crap...~%")
      (sleep 1)
      (apply #'raw-request path keys))
    (cl+ssl::ssl-error-syscall ()
      (format t "~&Retrying after cl+ssl crap...~%")
      (sleep 1)
      (apply #'raw-request path keys))
    (usocket:ns-host-not-found-error ()
      (format t "~&Retrying after nameserver crappage...~%")
      (sleep 1)
      (apply #'raw-request path keys))
    (usocket:deadline-timeout-error ()
      (format t "~&Retrying after deadline timeout...~%")
      (sleep 1)
      (apply #'raw-request path keys))
    (usocket:timeout-error ()
      (format t "~&Retrying after regular timeout...~%")
      (sleep 1)
      (apply #'raw-request path keys))))

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
                 :method :post
                 :parameters data
                 :additional-headers `(("API-Key"  . ,key)
                                       ("API-Sign" . ,(funcall signer path data nonce))
                                       ("Content-Type" . "application/x-www-form-urlencoded")))))

(defun get-assets ()
  (mapcar-jso (lambda (name data)
                (with-json-slots (altname decimals) data
                  (make-instance 'asset :name name :decimals decimals)))
              (get-request "Assets")))

(defclass kraken-market (market) ((altname :initarg :altname :reader altname)))

(defun get-markets (assets)
  (mapcar-jso (lambda (name data)
                (with-json-slots (pair_decimals quote base altname) data
                  (make-instance 'kraken-market :name name :altname altname
                                 :primary (find-asset base assets)
                                 :counter (find-asset quote assets)
                                 :decimals pair_decimals)))
              (get-request "AssetPairs")))

(defvar *kraken*
  (let ((assets (get-assets)))
    (make-instance 'exchange :name "Kraken"
                   :sensitivity 0.3
                   :assets assets :markets (get-markets assets))))

(defmethod find-market (designator (exchange (eql *kraken*)))
  (or (call-next-method)
      (with-slots (markets) *kraken*
        (find designator markets :key 'altname :test 'string-equal))))

(defclass kraken-gate (gate)
  ((count :initform 0) (delay :initform 19/3)  ; why not delay=5 ?
   (mint :initform (make-instance 'channel))
   (tokens :initform (make-instance 'channel))
   token-minter token-handler))

(defun request-cost (request)
  (string-case (request :default 1)
    ("AddOrder" 0) ("CancelOrder" 0)
    ("Ledgers" 2) ("QueryLedgers" 2)
    ("TradesHistory" 2) ("QueryTrades" 2)))

(defun token-minter-loop (gate)
  (with-slots (mint delay) gate
    (send mint 1)
    (sleep delay)))

(defun token-handler-loop (gate)
  (with-slots (mint count tokens) gate
    (case count
      (5 (recv mint) (decf count))
      (0 (send tokens t) (incf count))
      (t (select
           ((recv mint delta) (decf count delta))
           ((send tokens t) (incf count))
           (t (sleep 0.2)))))))

(defmethod gate-post ((gate kraken-gate) key secret request)
  (destructuring-bind (command . options) request
    ;; FIXME: this prevents add/cancel requests from going through while
    ;; other requests wait for tokens. possible solution - queue closures
    ;; with their associated cost, execute most expensive affordable request
    (dotimes (i (request-cost command)) (recv (slot-value gate 'tokens)))
    ;; (format t "~&~A ~A~&" (now) command)
    (multiple-value-list (post-request command key secret options))))

(defmethod shared-initialize ((gate kraken-gate) names &key pubkey secret)
  (multiple-value-call #'call-next-method gate names
                       (when pubkey (values :pubkey (make-key pubkey)))
                       (when secret (values :secret (make-signer secret)))))

(defmethod shared-initialize :after ((gate kraken-gate) (names t) &key)
  (with-slots (token-minter token-handler) gate
    (when (or (not (slot-boundp gate 'token-minter))
              (eq :terminated (task-status token-minter)))
      (setf token-minter (pexec (:name "kraken token minter")
                           (loop (token-minter-loop gate)))))
    (when (or (not (slot-boundp gate 'token-handler))
              (eq :terminated (task-status token-handler)))
      (setf token-handler (pexec (:name "kraken token handler")
                           (loop (token-handler-loop gate)))))))

;;;
;;; Public Data API
;;;

(defmethod get-book ((market kraken-market) &aux (pair (name market)))
  (let ((decimals (slot-value market 'decimals)))
    (with-json-slots (bids asks)
        (getjso pair (get-request "Depth" `(("pair" . ,pair) ("count" . "200"))))
      (flet ((parser (class)
               (lambda (raw-order)
                 (destructuring-bind (price amount timestamp) raw-order
                   (declare (ignore timestamp))
                   (make-instance class :market market
                                  :price (parse-price price decimals)
                                  :volume (parse-float amount))))))
        (values (mapcar (parser 'ask) asks)
                (mapcar (parser 'bid) bids))))))

(defmethod trades-since ((market kraken-market) &optional since)
  (with-json-slots (last (trades (name market)))
      (get-request "Trades"
                   `(("pair" . ,(name market)) .
                     ,(awhen (timestamp since)
                        `(("since"
                           . ,(format nil "~D~9,'0D"
                                      (timestamp-to-unix it) (nsec-of it)))))))
    (mapcar (lambda (trade)
              (destructuring-bind (price volume time side kind data) trade
                (let ((price  (read-from-string price))
                      (volume (read-from-string volume)))
                  (make-instance 'trade :market market
                                 :timestamp (parse-timestamp *kraken* time)
                                 ;; FIXME - "cost" later gets treated as precise
                                 :volume volume :price price :cost (* volume price)
                                 :direction (concatenate 'string side kind data)))))
            (rest trades))))            ; same trick as in bitfinex execution-since

;;;
;;; Private Data API
;;;

(defun open-orders (gate)
  (aif (gate-request gate "OpenOrders")
       (mapjso* (lambda (id order)
                  (setf (getjso "id" order) id))
                (getjso "open" it))
       (jso)))

(defmethod placed-offers (gate)
  (mapcar-jso (lambda (id data)
                (with-json-slots (descr vol oflags) data
                  (with-json-slots (pair type price order) descr
                    (let* ((market (find-market pair *kraken*))
                           (decimals (slot-value market 'decimals))
                           (price-int (parse-price price decimals))
                           (volume (read-from-string vol)))
                      (make-instance 'placed :uid id :market market
                                     :price (if (string= type "buy") (- price-int) price-int)
                                     :volume (if (not (search "viqc" oflags)) volume
                                                 (/ volume price-int (expt 10 decimals))))))))
              (open-orders gate)))

(defmethod account-balances ((gate kraken-gate))
  (remove-if #'zerop
             ;; TODO: this signals !(typep () 'jso) on communication failure
             ;; signaling is a Good Thing™, but let's be more helpful
             (mapcar-jso (lambda (currency amount)
                           (cons currency (parse-float amount)))
                         (gate-request gate "Balance"))
             :key #'cdr))

(defmethod market-fee ((gate kraken-gate) market &aux (pair (name market)))
  (awhen (gate-request gate "TradeVolume" `(("pair" . ,pair)))
    (read-from-string (getjso "fee" (getjso pair (getjso "fees" it))))))

(defun parse-execution (txid json)
  (with-json-slots (price pair fee cost vol time type) json
    (let ((fee (parse-float fee))
          (cost (parse-float cost)))
      (make-instance 'execution :fee fee :direction type :uid txid
                     :price (parse-float price) :cost cost
                     :volume (parse-float vol) :market (find-market pair *kraken*)
                     :timestamp (parse-timestamp *kraken* time)
                     :net (string-case (type)
                            ("buy" (+ cost fee))
                            ("sell" (- cost fee)))))))

(defun raw-executions (gate &optional last)
  (gate-request gate "TradesHistory" (when last `(("start" . ,(uid last))))))

(defmethod execution-since ((gate kraken-gate) (market kraken-market) since)
  (awhen (raw-executions gate since)
    (with-json-slots (trades) it
        (remove market (mapcar-jso #'parse-execution trades)
                :key #'market :test-not #'eq))))

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
      (when (string= type "sell") (setf volume (* volume price)))
      (multiple-value-bind (info errors)
          (gate-request gate "AddOrder"
                        `(("ordertype" . "limit") ("oflags" . "viqc")
                          ("type" . ,type) ("pair" . ,pair)
                          ("volume" . ,(format nil "~V$" vol-decimals volume))
                          ("price" . ,(format nil "~V$" decimals price))))
        (if errors (dolist (message errors) (warn "~&~A~%" message))
            (progn
              ;; theoretically, we could get several order IDs here,
              ;; but we're not using any of kraken's fancy forex nonsense
              (setf (getjso* "descr.id" info) (car (getjso* "txid" info)))
              (getjso "descr" info)))))))

(defmethod post-offer ((gate kraken-gate) offer)
  ;; (format t "~&place  ~A~%" offer)
  (with-slots (market volume price) offer
    (flet ((post (type)
             (awhen (post-limit gate type market (abs price) volume
                                (slot-value market 'decimals))
               (with-json-slots (id order) it
                 (change-class offer 'placed :uid id)))))
      (if (< price 0) (post "buy") (post "sell")))))

(defun cancel-order (gate oid)
  (gate-request gate "CancelOrder" `(("txid" . ,oid))))

(defmethod cancel-offer ((gate kraken-gate) offer)
  ;; (format t "~&cancel ~A~%" offer)
  (multiple-value-bind (ret err) (cancel-order gate (uid offer))
    (or ret (search "Unknown order" (car err)))))
