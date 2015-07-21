(defpackage #:scalpl.mpex
  (:nicknames #:mpex)
  (:export #:*mpex* #:mpex-agent)
  (:use #:cl #:chanl #:anaphora #:local-time
        #:split-sequence #:scalpl.util #:scalpl.exchange))

(in-package #:scalpl.mpex)

;;; General Parameters
(defparameter +base-path+ "http://mpex.co/")  ; TODO: auto-fallback to proxies
(defparameter +public-stub+ "mpex-")

(defun raw-request (path &rest keys)
  (multiple-value-bind (body status)
      (apply #'http-request (concatenate 'string +base-path+ path) keys)
    (if (= status 200) body
        (values nil (format nil "HTTP Error ~D~%~A" status body)))))

(defun get-request (path)
  (raw-request (concatenate 'string +public-stub+ path ".php") :want-stream t))

(defvar *mpex* (make-instance 'exchange :name :mpex))

;; (defun post-request (method key signer &optional params &aux (nonce (nonce)))
;;   (push (cons "method" method) params)
;;   (push (cons "nonce" nonce) params)
;;   (let ((data (urlencode-params params)))
;;     (raw-request (concatenate 'string +private-stub+)
;;                  :method :post :content data
;;                  :additional-headers `(("Key"  . ,key)
;;                                        ("Sign" . ,(funcall signer data))))))

(defclass mpex-market (market) ((exchange :initform *mpex*)))  ; FIXME is-a → has-a

(defun get-info ()
  (flet ((make-asset (name &optional (d 0))
           (make-instance 'asset :name name :decimals d :exchange *mpex*)))
    (let* ((bitcoin (make-asset "CxBTC" 8)) (assets (list bitcoin)))
      (values (mapcar (lambda (name &aux (asset (make-asset name)))
                        (push asset assets)
                        (make-instance 'mpex-market :primary asset :counter bitcoin
                                       :exchange *mpex* :decimals 8 :name name))
                      (mapcar #'car (with-open-stream
                                        (response (get-request "mktdepth"))
                                      (read-json response))))
              assets))))

(defmethod fetch-exchange-data ((exchange (eql *mpex*)))
  (with-slots (markets assets) exchange
    (setf (values markets assets) (get-info))))

;;; https://github.com/jurov/MPExAgent
(defclass mpex-agent (gate)
  ((exchange :allocation :class :initform *mpex*)))

(defmethod gate-post ((gate (eql *mpex*)) key secret request
                      &aux (reply-id (sxhash request)))
  (with-open-stream (response
      (http-request key :content-type "text/plain" :method :post
                    :want-stream t :external-format-in :ascii
                    :basic-authorization secret :content
                    (json:encode-json-plist-to-string
                     `("method" ,(car request)
                       "params" ,(apply 'vector (cdr request))
                       "jsonrpc" "2.0" "id" ,reply-id))))
    (json:json-bind (jsonrpc result error id) response
        (assert (string= jsonrpc "2.0")) (when id (assert (= reply-id id)))
        (list result error))))

;;;
;;; Public Data API
;;;

(defmethod get-book ((market mpex-market) &key)
  (awhen (with-open-stream (response (get-request "mktdepth"))
           (assoc (name market) (read-json response)))
    (flet ((process (side class predicate)
             (destructuring-bind (token &rest data) (pop it)
               (assert (eq token side))
               (mapcar (lambda (pair)
                         (make-instance class :market market
                                        :price (car pair) :volume (cadr pair)))
                       (sort data predicate :key #'first)))))
      (pop it) (values (process :S 'ask #'<) (process :B 'bid #'>)))))

(defun parse-trade (item)
  (destructuring-bind (amount market price)
      (split-sequence #\Space (rss:title item))
    (make-instance 'trade :market (find-market market *mpex*)
                   :timestamp (parse-rfc1123-timestring (rss:pub-date item))
                   :price (parse-price (subseq price 1) 8) :direction "soldorp"
                   :volume (parse-integer (remove #\` amount)))))

(defun trades-rss ()
  (with-open-stream (stream (get-request "rss"))
    (rss:items (rss:parse-rss-stream stream))))

(defmethod trades-since ((market mpex-market) &optional since)
  (when since (assert (eq market (market since))))
  (aprog1 (nreverse (remove market (mapcar #'parse-trade (trades-rss))
                            :test-not #'eq :key #'market))
    (when since
      (awhen (member (timestamp since) it :key #'timestamp :test #'timestamp=)
        (return-from trades-since (rest it)))
      (flet ((hms (time) (subseq (princ-to-string (timestamp time)) 11 19)))
        (warn "missing trades: ~A - ~A" (hms since) (hms (first it)))))))

;;;
;;; Private Data API
;;;

(defmethod placed-offers ((gate mpex-agent))
  (mapcar (lambda (data)
            (flet ((value (key) (cdr (assoc key data))))  ; i smell a pattern
              (make-instance 'placed :oid (parse-integer (string (pop data)))
                             :price (* (value :*price)
                                       (string-case ((value :+bs+))
                                         ("S" 1) ("B" -1)))
                             :volume (value :*quantity)
                             :market (find-market (value :+mpsic+) *mpex*))))
          (cdr (assoc :*book (gate-request gate "statjson")))))

(defmethod account-balances ((gate mpex-agent))
  (mapcar (lambda (data)
            (cons-aq (find-asset
                      (if (eq (car data) :*cx-+btc+) "CxBTC"
                          (string-trim "+" (string (car data)))) *mpex*)
                     (cdr data)))
          (cdr (assoc :*holdings (gate-request gate "statjson")))))

;;; All sellers are assesed a 0.2% fee at the moment the sale completes (so if
;;; you sell 500 stocks for 100 satoshi each you get 49`900 satoshi or
;;; 0.000499 BTC). All MKOPT and MKFUT orders are assesed a 2% fee
(defmethod market-fee ((gate t) (market mpex-market)) '(0 . 0.2))

(defun parse-execution (txid json) (error "FIXME"))
(defun raw-executions (gate &key pair from end) (error "FIXME"))
(defmethod execution-since ((gate mpex-agent) market since) (error "FIXME"))

(defun post-raw-limit (gate type market price volume)
  ;; orderType : 'B' or 'S'
  ;; price - unit price in satoshi
  (gate-request gate "neworder" (list type market volume price)))
(defmethod post-offer ((gate mpex-agent) (offer offer))
  (with-slots (market volume price) offer
    (flet ((post (type)
             (post-raw-limit gate type (string (name market)) (abs price) volume)
             ;; (with-json-slots (order_id) it
             ;;   (change-class offer 'placed :oid order_id))
             ))
      (post (if (< price 0) "B" "S")))))

(defmethod cancel-offer ((gate mpex-agent) offer)
  (gate-request gate "cancel" (list (oid offer))))
