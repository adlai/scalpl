(defpackage #:scalpl.bitfinex
  (:use #:cl #:anaphora #:local-time #:st-json #:base64 #:scalpl.util #:scalpl.exchange)
  (:export #:get-request
           #:post-request
           #:find-market #:*bitfinex* #:bitfinex-gate
           #:make-key #:make-signer))

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

;;; generate max 1 nonce per second
(defvar *last-nonce* (now))

(defun nonce (&aux (now (now)) (delta (timestamp-difference now *last-nonce*)))
  (when (> 1 delta) (sleep (- 1 delta)))
  (princ-to-string (+ (floor (nsec-of now) 1000)
                      (* 1000000 (timestamp-to-unix now)))))

(defun make-payload (data &optional path)
  (let ((payload (if (null path) (jso) (jso "request" path "nonce" (nonce)))))
    (dolist (pair data (string-to-base64-string (write-json-to-string payload)))
      (destructuring-bind (key . val) pair (setf (getjso key payload) val)))))

(defgeneric make-signer (secret)
  (:method ((secret simple-array))
    (lambda (payload) (format nil "~(~96,'0X~)" (hmac-sha384 payload secret))))
  (:method ((secret string)) (make-signer (string-octets secret)))
  (:method ((stream stream)) (make-signer (read-line stream)))
  (:method ((path pathname)) (with-open-file (stream path) (make-signer stream))))

(defgeneric make-key (key)
  (:method ((key string)) key)
  (:method ((stream stream)) (read-line stream))
  (:method ((path pathname))
    (with-open-file (stream path)
      (make-key stream))))

(defun decode-json (arg)
  (st-json:read-json (map 'string 'code-char arg)))

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
          (200 (decode-json body))
          ((400 404) (values nil (decode-json body)))
          (t (cerror "Retry request" "HTTP Error ~D" status)
             (apply #'raw-request path keys))))
    (drakma::drakma-simple-error ()
      (format t "~&Retrying after drakma SIMPLE crap...~%")
      (sleep 1)
      (apply #'raw-request path keys))
    (drakma::simple-error ()
      (format t "~&Retrying after drakma crap...~%")
      (sleep 1)
      (apply #'raw-request path keys))
    (cl+ssl::ssl-error-zero-return ()
      (format t "~&Retrying after cl+ssl crap...~%")
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
      (apply #'raw-request path keys))
    ))

(defun get-request (path &optional data)
  (raw-request path :additional-headers `(("X-BFX-PAYLOAD" ,(make-payload data)))))

(defun post-request (method key signer &optional data)
  (let* ((path (concatenate 'string "/v1/" method))
         (payload (make-payload data path)))
    (raw-request method :method :post
                 :additional-headers `(("X-BFX-APIKEY"  . ,key)
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

(defclass bitfinex-market (market) ())

(defun get-markets (assets &aux markets)
  (dolist (name (get-request "symbols") markets)
    (push (make-instance
           'bitfinex-market :name name
           :base (find-asset (subseq name 0 3) assets)
           :quote (find-asset (subseq name 3) assets)
           :decimals (detect-market-precision name))
          markets)))

(defvar *bitfinex*
  (let ((assets (get-assets)))
    (make-instance 'exchange :name "Bitfinex"
                   :assets assets :markets (get-markets assets))))

(defclass bitfinex-gate (gate) ())

(defmethod gate-post ((gate bitfinex-gate) key secret request)
  (destructuring-bind (command . options) request
    (multiple-value-list (post-request command key secret options))))

(defmethod shared-initialize ((gate bitfinex-gate) names &key pubkey secret)
  (multiple-value-call #'call-next-method gate names
                       (when pubkey (values :pubkey (make-key pubkey)))
                       (when secret (values :secret (make-signer secret)))))

;;;
;;; Public Data API
;;;

(defmethod get-book ((market bitfinex-market) &aux (pair (name-of market)))
  (let ((decimals (slot-value market 'decimals)))
    (with-json-slots (bids asks)
        (get-request (format nil "book/~A" pair)
                     '(("limit_asks" . 100) ("limit_bids" . 100)))
      (flet ((parser (class)
               (lambda (raw-order)
                 (with-json-slots (price amount) raw-order
                   (make-instance class :market market
                                  :price (parse-price price decimals)
                                  :volume (read-from-string amount))))))
        (values (mapcar (parser 'ask) asks)
                (mapcar (parser 'bid) bids))))))

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
                     (volume (read-from-string remaining_amount)))
                (make-instance 'placed :id id :market market :volume volume
                               :price (if (string= side "buy") (- price-int) price-int)))))
          (open-orders gate)))

(defmethod market-fee ((gate bitfinex-gate) market)
  (awhen (car (gate-request gate "account_infos"))
    (read-from-string
     (getjso "maker_fees"
             (find (name-of (slot-value market 'base))
                   (getjso "fees" it) :test #'string-equal
                   :key (lambda (x) (getjso "pairs" x)))))))

;;;
;;; Action API
;;;

(defun post-limit (gate type pair price volume decimals)
  (let ((price (/ price (expt 10d0 decimals))))
    (when (string-equal type "buy")
      (setf volume (/ volume price)))
    (when (>= volume 0.001)
      (multiple-value-bind (info error)
          (gate-request gate "order/new"
                        `(("type" . "exchange limit")
                          ("exchange" . "bitfinex") ; lold habits
                          ("side" . ,type)
                          ("symbol" . ,pair)
                          ("amount" . ,(format nil "~V$" 3 volume))
                          ("price" . ,(format nil "~V$" decimals price))
                          ))
        (if error (warn (getjso "message" error)) info)))))

(defmethod post-offer ((gate bitfinex-gate) offer)
  ;; (format t "~&place  ~A~%" offer)
  (with-slots (market volume price) offer
    (flet ((post (type)
             (awhen (post-limit gate type (name-of market) (abs price) volume
                                (slot-value market 'decimals))
               (with-json-slots (order_id) it
                 (change-class offer 'placed :id order_id)))))
      (post (if (< price 0) "buy" "sell")))))

;;; the order object returned will (always?) indicate that the order hasn't yet
;;; been cancelled; however, in situations where bfx has failed to cancel the
;;; order, we get 400 Bad Request + error message; so if we have any primary
;;; return value, we can treat that as a successful cancellation.
(defun cancel-order (gate oid)
  (gate-request gate "order/cancel" `(("order_id" . ,oid))))

(defmethod cancel-offer ((gate bitfinex-gate) offer)
  ;; (format t "~&cancel ~A~%" offer)
  (multiple-value-bind (ret err) (cancel-order gate (offer-id offer))
    (or ret (string= "Order could not be cancelled." (getjso "message" err)))))
