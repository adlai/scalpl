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
    (let ((timestamp
           (destructuring-bind (dd mmm yyyy hhmmss zone)
               (cdr (split-sequence #\Space (rss:pub-date item)))
             (parse-rfc3339-timestring
              (format nil "~A-~2,'0D-~AT~A~A" yyyy
                      (short-month-index mmm) dd hhmmss zone)))))
      (assert (string= (rss:pub-date item) (to-rfc1123-timestring timestamp)))
      (make-instance 'trade :market (find-market market *mpex*)
                     :timestamp timestamp :direction "soldorp"
                     :volume (parse-integer (remove #\` amount))
                     :price (parse-price (subseq price 1) 8)))))
(defmethod trades-since ((market mpex-market) &optional since)
  (remove market (mapcar #'parse-trade
                         (rss:items (rss:parse-rss-stream (get-request "rss"))))
          :test-not #'eq :key #'market))

;;;
;;; Private Data API
;;;

(defmethod placed-offers ((gate mpex-agent)) (error "FIXME"))

(defmethod account-balances ((gate mpex-agent)) (error "FIXME"))

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
