(defpackage #:scalpl.poloniex
  (:nicknames #:poloniex)
  (:export #:*poloniex* #:poloniex-gate)
  (:use #:cl #:chanl #:anaphora #:local-time #:scalpl.util #:scalpl.exchange))

(in-package #:scalpl.poloniex)

(defun hmac-sha512 (message secret)
  (let ((hmac (ironclad:make-hmac (string-octets secret) 'ironclad:sha512)))
    (ironclad:update-hmac hmac (string-octets message))
    (ironclad:octets-to-integer (ironclad:hmac-digest hmac))))

(defgeneric make-signer (secret)
  (:method ((signer function)) signer)
  (:method ((string string))
    (lambda (message) (format () "~(~128,'0X~)" (hmac-sha512 message string))))
  (:method ((stream stream)) (make-signer (read-line stream)))
  (:method ((path pathname)) (with-open-file (data path) (make-signer data))))

(defgeneric make-key (key)
  (:method ((key string)) key)
  (:method ((stream stream)) (read-line stream))
  (:method ((path pathname))
    (with-open-file (stream path)
      (make-key stream))))

;;; General Parameters
(defparameter +base-path+ "https://poloniex.com/")
(defparameter +public-stub+ "public?command=")
(defparameter +private-path+ (concatenate 'string +base-path+ "tradingApi"))

(defun public-request (method &rest parameters)
  (decode-json
   (http-request (apply 'concatenate
                        'string +base-path+ +public-stub+ method
                        (when parameters
                          `("&" ,(urlencode-params parameters)))))))

(defun auth-request (method key signer &optional parameters)
  (push (cons "command" method) parameters)
  (let* ((nonce (format () "~D" (+ (timestamp-millisecond (now))
                                   (* 1000 (timestamp-to-unix (now))))))
         (data (urlencode-params (acons "nonce" nonce parameters)))
         (sig (funcall signer data)))
    (decode-json
     (http-request +private-path+ :method :post :content data
                   :additional-headers `(("Key" . ,key) ("Sign" . ,sig))))))

(defun get-info (&aux assets)
  (awhen (public-request "returnTicker")
    (flet ((ensure-asset (name)
             (or (find name assets :key #'name :test #'string=)
                 (aprog1 (make-instance 'asset :name name :decimals 8)
                   (push it assets)))))
      (values (mapcar (lambda (data &aux (pair (car data)))
                        (make-instance
                         'market :name pair :decimals 8
                         :primary (ensure-asset (subseq (string pair) 4))
                         :counter (ensure-asset (subseq (string pair) 0 3))))
                      (remove "0" it :test-not #'string=
                              :key (lambda (x) (cdr (assoc :|isFrozen| (cdr x))))))
              assets))))

(defvar *poloniex* (make-instance 'exchange :name :poloniex :sensitivity 1))

(defmethod fetch-exchange-data ((exchange (eql *poloniex*)))
  (with-slots (markets assets) exchange
    (setf (values markets assets) (get-info))))

(defclass poloniex-gate (gate) ((exchange :allocation :class :initform *poloniex*)))

(defmethod shared-initialize ((gate poloniex-gate) names &key pubkey secret)
  (multiple-value-call #'call-next-method gate names
                       (mvwrap pubkey make-key) (mvwrap secret make-signer)))

(defmethod gate-post ((gate (eql *poloniex*)) key secret request)
  (destructuring-bind (command . options) request
    (alet (auth-request command key secret options)
      (if (eq (caar it) :|error|) (list (warn (cdar it)) (cdar it)) (list it)))))

;;; private data API
(defmethod account-balances ((gate poloniex-gate))
  (aif (gate-request gate "returnBalances")
       (mapcan (lambda (pair &aux (asset (find-asset (car pair) :poloniex)))
		 (let ((amount (parse-float (cdr pair) :type 'number)))
		   (unless (zerop amount)
		     `((,asset . ,(cons-aq* asset amount))))))
	       it)
       (error "communication breakdown!")))

(defmethod placed-offers ((gate poloniex-gate))
  (mapcan (lambda (pair)
	    (let ((market (find-market (car pair) :poloniex)))
	      (mapcar (lambda (order)
			(flet ((key (key) (cdr (assoc key order))))
			  (let ((bidp (string= "buy" (key :|type|)))
				(rate (* (expt 10 (decimals market))
					 (parse-float (key :|rate|) :type 'number)))
				(amount (parse-float (key :|amount|)
						     :type 'number)))
			    (make-instance
			     'placed :market market :volume amount
			     :given (if bidp (cons-aq (counter market)
						      (* rate amount))
					(cons-aq (primary market) amount))
			     :oid (parse-integer (key :|orderNumber|))
			     :price (* rate (if bidp -1 1))))))
		      (cdr pair))))
	  (remove () (gate-request gate "returnOpenOrders"
				   '((:|currencyPair| . :all)))	; kludge
		  :key #'cdr)))
