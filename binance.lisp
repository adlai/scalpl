(defpackage #:scalpl.binance
  (:nicknames #:binance)
  (:use #:cl #:base64 #:chanl #:anaphora #:local-time #:scalpl.util
        #:scalpl.actor #:scalpl.net #:scalpl.exchange #:scalpl.qd)
  (:export #:*binance* #:binance-gate #:binance-market
           #:category #:recent-ticker
           #:linear-market #:inverse-market #:option-market))

(in-package #:scalpl.binance)

;;; General Parameters
(defparameter *base-domain* "api.binance.com")
(defparameter *base-url* (format () "https://~A" *base-domain*))

(defvar *binance* (make-instance 'exchange :name :binance :sensitivity 1))

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

(defun v3-url (stem) (format () "~A/api/v3/~A" *base-url* stem))

(defun binance-request (path &rest args)
  (multiple-value-bind (body status headers uri)
      (apply #'http-request (v3-url path) args)
    (case status
      (200 (decode-json body))
      ((429) (warn "RATE LIMIT HIT~%URI: ~A~%~A" uri headers)
       (sleep (or (cdr (assoc :retry-after headers)) 1))
       (values () status body))
      (t (let ((reply (decode-json body)))
	   (warn "Binance ~D~@[ [~A]~]~%URI: ~A" status 
		 (getjso "code" reply) uri)
	   (values () status reply))))))

(defun public-request (path parameters)
  (binance-request path :parameters
		   (loop for (key . value) in parameters
                         collect (cons (string-downcase (string key))
                                       (princ-to-string value)))))

(defun auth-request (verb path key signer &optional params)
  (let* ((time (format () "~D" (+ (timestamp-millisecond (now))
                                  (* 1000 (timestamp-to-unix (now))))))
         (payload (concatenate 'string (urlencode-params params)
			       "&timestamp=" time))
	 (signature (funcall signer payload)))
    (funcall 'binance-request (concatenate 'string path "?" payload
					   "&signature=" signature)
	     :method verb :additional-headers `(("X-MBX-APIKEY" . ,key)))))

(defclass binance-market (market)
  ((exchange :initform *binance* :allocation :class)
   (category :initarg :category :allocation :class :reader category)
   (fee :initarg :fee :reader fee)
   (epsilon :initarg :epsilon :reader epsilon)
   (delivery :initarg :delivery :reader delivery)
   (recent-ticker :accessor recent-ticker)))

;; (defclass linear-market (binance-market)
;;   ((category :initform "linear" :allocation :class)))
;; (defclass inverse-market (binance-market)
;;   ((category :initform "inverse" :allocation :class)))
;; (defclass option-market (binance-market)
;;   ((category :initform "option" :allocation :class)
;;    (delivery-date :reader delivery-date) (strike :reader strike)))
(defclass spot-market (binance-market)
  ((category :initform "spot" :allocation :class)))

(defmethod category ((market tracked-market))
  (category (slot-value market 'scalpl.exchange::%market)))

(defun get-info (&aux assets markets)
  (flet ((symbol-parser (json)
           (with-json-slots 
               ((name "symbol") (primary "baseAsset") (counter "quoteAsset")
		(primary-decimals "baseAssetPrecision")
		(counter-decimals "quoteAssetPrecision")
                (decimals "quotePrecision") filters status)
               json
             (flet ((asset (name decimals)
                      (or (find name assets :key #'name :test #'string=)
                          (aprog1 (make-instance 'asset :name name
                                                        :decimals decimals)
                            (push it assets)))))
               (when (and (string= status "TRADING"))
		 (let ((price-filter (cdr (assoc "PRICE_FILTER" filters
						 :key #'cdr :test #'string=)))
		       (lot-filter (cdr (assoc "LOT_SIZE" filters
					       :key #'cdr :test #'string=))))
                   (let ((tick (parse-float (getjso "tickSize" price-filter)
					    :type 'rational))
			 (epsilon (parse-float (getjso "minQty" lot-filter)
					       :type 'rational)))
                     (push (make-instance
                            'spot-market :name name :fee 0.1 :tick tick
                            :epsilon epsilon :decimals decimals
                            :primary (asset primary primary-decimals)
                            :counter (asset counter counter-decimals))
                           markets))))))))
    (with-json-slots (symbols)
        (public-request "exchangeInfo" ())
      (mapcar #'symbol-parser symbols))
    (values markets assets)))

(defmethod fetch-exchange-data ((exchange (eql *binance*)))
  (with-slots (markets assets) exchange
    (setf (values markets assets) (get-info))))

(defclass binance-gate (gate)
  ((exchange :initform *binance*)
   (websocket-cache :initform (make-hash-table :test #'equalp)
                    :reader websocket-cache)))

(defmethod gate-post ((gate (eql *binance*)) key secret request)
  (destructuring-bind ((verb method) . parameters) request
    (multiple-value-bind (ret code status)
        (auth-request verb method key secret parameters)
      `(,ret ,(awhen1 (acase code
                        ((0)) ; C-style, no error
                        ((10006))     ; what is this, now?
                        ((10027) ;; (mapc 'kill (pooled-threads))
                         ) ; "I missed the part where it's my problem"
                        ;; simply killing all pooled threads will not quite
                        ;; work, because it hits the debugger when the gate
                        ;; attempts to kill itself...
                        ;; TODO: lots of DEFINE-CONDITION ... see
                        ((20001 30032 30037)) ; filled before cancellation
                        ;; ((30076) it)          ; failures to replace
                        (t status))     ; by default, echo the error text
                (typecase it
                  (string #|(break)|# (warn "#~D ~A" code it))))))))

(defmethod shared-initialize ((gate binance-gate) names &key pubkey secret)
  (multiple-value-call #'call-next-method gate names
                       (mvwrap pubkey make-key) (mvwrap secret make-signer)))

;;;
;;; Public Data API
;;;

(defmethod get-book ((market spot-market) &key count)
  (with-slots (name decimals) market
    (with-json-slots (bids asks)
	(public-request "depth"
			`(("symbol" . ,name)
                          ,@(when count 
			      `(("limit" . ,(format () "~D" count))))))
      (flet ((side (type list)
               (loop for (price size) in list
                     collect (make-instance
                              type :market market
                              :volume (parse-float size :type 'rational)
                              :price (parse-price price decimals)))))
        (values (side 'ask asks) (side 'bid bids))))))

(defmethod trades-since ((market spot-market) &optional from)
  (with-slots (name) market
    (flet ((parse (trade)
             (with-json-slots 
		 ((id "a") (time "T") (qty "q") (price "p") (sellp "m"))
		 trade
               (let* ((price (parse-float price :type 'rational))
                      (volume (parse-float qty :type 'rational))
                      (cost (* price volume)))
                 (make-instance 'trade 
				:market market :txid id
				:direction (if sellp "sell" "buy")
                                :timestamp (multiple-value-bind (sec ms)
					       (floor time 1000)
					     (unix-to-timestamp 
					      sec :nsec (* ms (expt 10 6))))
                                :cost cost :volume volume :price price)))))
      (awhen (public-request "aggTrades"
                             `(("symbol" . ,name)))
        (let ((trades (nreverse (mapcar #'parse it))))
          (if (null from) trades
              (cdr (member (txid from) trades :key 'txid :test '=))))))))

;; ;;;
;; ;;; Private Data API
;; ;;;

(defmethod market-placed ((gate binance-gate) (market tracked-market))
  (market-placed gate (slot-value market 'scalpl.exchange::%market)))

(defmethod market-placed ((gate binance-gate) (market binance-market) &aux offers)
  (let ((url "openOrders"))
    (multiple-value-bind (result error)
	(gate-request gate `(:get ,url) `(("symbol" . ,(name market))))
      (unless error
	(dolist (json result)
	  (with-json-slots (side price (oid "orderId") (qty "origQty")) json
            (let* ((price (parse-price price (decimals market)))
		   (volume (parse-float qty :type 'rational))
		   (oid (format () "~D" oid)))
              (pushnew (make-instance 'offered :oid oid :market market
					       :volume volume :price
					       (if (string= side "Sell")
						   price (- price)))
                       offers :test #'string= :key 'oid))))))
    offers))

(defmethod placed-offers ((gate binance-gate) &optional market)
  (if market (market-placed gate market)
      (loop for market in (markets *binance*)
            nconc (market-placed gate market))))


(defmethod account-balances ((gate binance-gate) &aux balances)
  ;; (declare (optimize debug))
  (awhen (gate-request gate '(:get "account"))
    (dolist (coin (getjso "balances" it) balances)
      (with-json-slots (asset free locked) coin
	(let ((total (+ (parse-float free) (parse-float locked))))
	  (unless (zerop total)
	    (push (cons-aq* (find-asset asset *binance*) total)
                  balances)))))))

;;; This horror can be avoided via the actor-delegate mechanism.
(defmethod market-fee ((gate binance-gate) (market binance-market))
  (awhen (gate-request gate '(:get "account/commission")
                       `(("symbol" . ,(name market))))
    (* 100 (parse-float (reduce 'getjso '("maker" "standardCommission")
				:from-end t :initial-value it)))))

(defmethod parse-timestamp ((exchange (eql *binance*)) (timestamp string))
  (multiple-value-bind (unix milliseconds)
      (floor (parse-integer timestamp) 1000)
    (unix-to-timestamp unix :nsec (* 1000000 milliseconds))))

;; (defgeneric build-execution (market &rest args)
;;   (:method ((market tracked-market) &rest args)
;;     (apply 'build-execution
;;            (slot-value market 'scalpl.exchange::%market) args))
;;   (:method ((market binance-market) &rest args)
;;     (if (string= "inverse" (category market))
;;         (destructuring-bind
;;             (oid txid side cost price volume fee time) args
;;           (make-instance 'execution :direction side :market market
;;                                     :txid txid :price price :cost cost
;;                                     :net-cost cost :volume (abs volume)
;;                                     :net-volume (abs (+ volume fee))
;;                                     :timestamp time :oid oid))
;;         (destructuring-bind
;;             (oid txid side cost price volume fee time) args
;;           (make-instance 'execution :direction side :market market
;;                                     :txid txid :price price
;;                                     :cost (abs volume)
;;                                     :net-cost (abs (+ volume fee))
;;                                     :volume cost :net-volume cost
;;                                     :timestamp time :oid oid)))))

;; (defun parse-execution (raw)
;;   (with-json-slots ((oid "orderId") (txid "execId") (amt "execQty")
;;                     symbol side (price "execPrice") (time "execTime")
;;                     (execost "execValue") (exefee "execFee"))
;;       raw
;;     (unless (zerop (length side))
;;       (flet ((adjust (value) (parse-float value)))
;;         (build-execution (find-market symbol :binance)
;;                          oid txid side (adjust amt) (adjust price)
;;                          (adjust execost) (adjust exefee)
;;                          (parse-timestamp *binance* time))))))

;; (defun raw-executions (gate &key kind pair from count)
;;   (macrolet ((params (&body body)
;;                `(append ,@(loop for (val key exp) in body
;;                              collect `(when ,val `((,,key . ,,exp)))))))
;;     (awhen (gate-request gate '(:get "execution/list")
;;                          (params (kind "category" kind)
;;                                  (pair "symbol" pair) (count "limit" count)
;;                                  (from "startTime"
;;                                        (princ-to-string
;;                                         (+ (* 1000 (timestamp-to-unix from))
;;                                            (floor (nsec-of from) 1000000))))))
;;       (sort (getjso "list" it) #'string< :key (getjso "execTime")))))

;; (defmethod execution-since ((gate binance-gate) (market binance-market) since)
;;   (awhen (raw-executions gate :kind (category market)
;;                               :pair (name market) :count "100"
;;                               :from (if since (timestamp since)
;;                                         (timestamp- (now) 3 :hour)))
;;     (mapcar #'parse-execution
;;             (if (null since) it
;;                 (subseq it (1+ (position (txid since) it
;;                                          :test #'string=
;;                                          :key (getjso "execId"))))))))

;; (defgeneric post-limit (gate market price size &optional reduce-only)
;;   (:method (gate (market tracked-market) price size &optional reduce-only)
;;     (post-limit gate (slot-value market 'scalpl.exchange::%market)
;;                 price size reduce-only))
;;   (:method ((gate binance-gate) (market binance-market) price size
;;             &optional reduce-only &aux (now (now)))
;;     (let ((link-id (format () "~12,'0X-~23,'0X"
;;                            (+ (timestamp-millisecond now)
;;                               (* 1000 (timestamp-to-unix now)))
;;                            (random (expt 16 23)))))
;;       (gate-request gate '(:post "order/create")
;;                     (with-slots (name category epsilon) market
;;                       `(("orderType" . "Limit") ("category" . ,category)
;;                         ("symbol" . ,name) ("price" . ,price)
;;                         ("side" . ,(if (plusp size) "Buy" "Sell"))
;;                         ("qty" . ,(format () "~V$"
;;                                           (abs (floor (log epsilon 10)))
;;                                           (abs size)))
;;                         ("timeInForce" . "PostOnly") ("orderLinkId" . ,link-id)
;;                         ("reduceOnly" . ,(if reduce-only "true" "false"))))))))

;; (defun amend-limit (gate market oid price size) ; &optional considered harmful?
;;   (gate-request gate '(:post "order/amend")
;;                 (with-slots (name category epsilon) market
;;                   `(("category" . ,category) ("symbol" . ,name)
;;                     ("orderId" .,oid) ("price" . ,price)
;;                     ("qty" . ,(format () "~V$"
;;                                       (abs (floor (log epsilon 10)))
;;                                       (abs size)))))))

;; (defclass placed (offered) ((unique-request-identifier :initarg :urid)))

;; (defmethod post-offer ((gate binance-gate) (offer offer))
;;   (with-slots (market volume price) offer
;;     (let ((factor (expt 10 (decimals market))))
;;       (multiple-value-bind (json complaint)
;;           (post-limit gate market
;;                       (multiple-value-bind (int dec)
;;                           (floor (abs (/ (floor price 1/2) 2)) factor)
;;                         (format nil "~D.~V,'0D"
;;                                 int (max 1 (decimals market)) (* 10 dec)))
;;                       (if (string= (category market) "inverse")
;;                           (floor (* volume (if (minusp price) 1
;;                                                (- (/ price factor)))))
;;                           (/ volume (if (minusp price)
;;                                         (/ (abs price) factor)
;;                                         -1))))
;;         (with-json-slots ((oid "orderId") (urid "orderLinkId")) json
;;           (if (and json oid urid)
;;               (change-class offer 'placed :oid oid :urid urid)
;;               (warn "Failed placing: ~S~%~A" offer complaint)))))))

;; (defmethod amend-offer ((gate binance-gate) (old offered) (new offer))
;;   (with-slots (market oid) old
;;     (with-slots (volume price) new
;;       (let* ((factor (expt 10 (decimals market))))
;;         (multiple-value-bind (json complaint)
;;             (amend-limit gate market oid
;;                          (unless (= (realpart price) (realpart (price old)))
;;                            (multiple-value-bind (int dec)
;;                                (floor (abs (/ (floor price 1/2) 2)) factor)
;;                              (format () "~D.~V,'0D" int
;;                                      (max 1 (decimals market)) (* 10 dec))))
;;                          (if (string= (category market) "inverse")
;;                              (floor (* volume (if (minusp price) 1
;;                                                   (/ price factor))))
;;                              (/ volume (if (minusp price)
;;                                            (/ (abs price) factor)
;;                                            -1))))
;;           (if complaint
;;               (unless (string= "Order does not exist" complaint)
;;                 (warn (format () "~A was not modified to ~A" old new)))
;;               (with-json-slots ((returned-oid "orderId")) json
;;                 (if (string= returned-oid oid) ; how can this test fail?
;;                     (reinitialize-instance     ; not rhetoric, enumerate
;;                      old :volume volume :price price))))))))) ; or don't

(defmethod cancel-offer ((gate binance-gate) (offer offered))
  (multiple-value-bind (ret err)
      (gate-request gate '(:delete "order")
                    (with-slots (name) (market offer)
                      `(("symbol" . ,name) ("orderId" . ,(oid offer)))))
    (or ret (= (getjso "code" err) -2011) (values nil err))))

;; (defclass binance-prioritizer (prioritizer) ())

;; (defmethod prioriteaze ((ope binance-prioritizer) target placed
;;                         &aux to-add (excess placed))
;;   (flet ((frob (add pop &aux (max (max (length add) (length pop))))
;;            (with-slots (expt) ope
;;              (let* ((n (expt (random (expt max (/ expt))) expt))
;;                     (add (nth (floor n) add))
;;                     (pop (nth (- max (ceiling n)) pop)))
;;                (if (and add pop)
;;                    (amend-offer (slot-reduce ope supplicant gate) pop add)
;;                    (if add (ope-place ope add) (ope-cancel ope pop)))))))
;;     (aif (dolist (new target (sort to-add #'< :key #'price))
;;            (aif (find (price new) excess :key #'price :test #'=)
;;                 (setf excess (remove it excess)) (push new to-add)))
;;          (frob it (reverse excess))   ; which of these is worst?
;;          (if excess (frob () excess)  ; choose the lesser weevil
;;              (and target placed (= (length target) (length placed))
;;                   (loop for new in target and old in placed
;;                      when (sufficiently-different? new old)
;;                      collect new into news and collect old into olds
;;                      finally (when news (frob news olds))))))))
