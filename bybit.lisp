(defpackage #:scalpl.bybit
  (:nicknames #:bybit) (:export #:*bybit* #:bybit-gate #:*swagger*)
  (:use #:cl #:base64 #:chanl #:anaphora #:local-time #:scalpl.util
        #:scalpl.actor #:scalpl.exchange #:scalpl.qd))

(in-package #:scalpl.bybit)

;;; General Parameters
(defparameter *base-domain* "api.bybit.com")
(defparameter *base-url* (format () "https://~A" *base-domain*))

(defvar *bybit* (make-instance 'exchange :name :bybit :sensitivity 1))

;;; HALT THOU
;;;
;;; ... oops, too late, defvar is some sorta rewritable run-oncely
;;;
;;; You may also want to consult a
;;; github.com/bybit-exchange/api-connectors/blob/?#,swagger.json
;;; thanks to @dexter-2 for the pointer from their Issues flytrap

(defun load-swagger-specification (path)
  (with-open-file (file path) (read-json file)))

;; (defparameter *swagger* ; TODO: use correct UIOP incantation here
;;   (load-swagger-specification
;;    #P"./promises/bybit/api-connectors/swagger.json"))

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

(defun bybit-request (path &rest args)
  (multiple-value-bind (body status headers uri stream close)
      (if (and (ignore-errors (slot-boundp *bybit* 'stream))
               (open-stream-p (slot-value *bybit* 'stream)))
          (apply #'http-request (concatenate 'string *base-url* path)
                 :stream (slot-value *bybit* 'stream) :close () :keep-alive t
                 args)
          (apply #'http-request (concatenate 'string *base-url* path)
                 ;; :close () :keep-alive t
                 args))
    ;; (when (aand (assoc :content-length headers) (> (parse-integer (cdr it)) 7890))
    ;;   (format *debug-io* "~&~A spat ~S~%" uri headers))
    (unless close (setf (slot-value *bybit* 'stream) stream))
    (case status
      (200 (with-json-slots (result ret_code ret_msg) (decode-json body)
             (values result ret_code ret_msg)))
      ((403 404) (warn "ByBit Error ~A" body) (values () status body))
      (t (warn "Bybit Error ~%~A" status) (values () status body)))))

(defun bybit-path (&rest paths) (format () "~{~A~}" paths))

(defun public-request (method parameters
		       &optional (prefix "/v2/public/"))
  (bybit-request (bybit-path prefix method)
                 :parameters (loop for (key . value) in parameters collect
                                  (cons (string-downcase (string key))
                                        (princ-to-string value)))))

;; (defmethod format-timestring (timestamp)
;;   (format () "~D" (+ (timestamp-millisecond (now))
;;                      (* 1000 (timestamp-to-unix (now))))))

(defun auth-request (verb path key signer &optional params)
  (let* ((time (format () "~D" (+ (timestamp-millisecond (now))
                                  (* 1000 (timestamp-to-unix (now))))))
         (sorted (sort `(("api_key" . ,key) ,@params ("timestamp" . ,time))
                       #'string< :key #'car))
         (sig (funcall signer (concatenate-url-parameters sorted)))
         (parameters `(,@sorted ("sign" . ,sig))))
    (bybit-request path :method verb :parameters parameters)))

(defclass bybit-market (market)
  ((exchange :initform *bybit*) (fee :initarg :fee :reader fee)))

(defun get-info (&aux assets)
  (flet ((make-market (instrument)
           (with-json-slots ((long "base_currency") (short "quote_currency")
                             name (fee "maker_fee") (filter "price_filter"))
               instrument
             (flet ((asset (fake)
                      (let ((name (concatenate 'string fake "-" name)))
                        (or (find name assets :key #'name :test #'string=)
                            (aprog1 (make-instance 'asset :name name :decimals
                                                   (if (string= fake "BTC")
                                                       8 0))
                              (push it assets)))))
                    (ilog (i) (floor (log (abs i) 10))))
               (make-instance
                'bybit-market :name name :fee (parse-float fee)
                :decimals (- (ilog (parse-float (getjso "tick_size" filter))))
                :primary (asset long) :counter (asset short))))))
    (awhen (public-request "symbols" ())
      (values (mapcar #'make-market it) assets))))

(defmethod fetch-exchange-data ((exchange (eql *bybit*)))
  (declare (optimize (speed 0) (debug 3) (compilation-speed 0)
                     ;; (space 3) (safety 5) ;; go away, great
                     ))                 ;       bird off-white
  ;; and don't turn back without a fight!
  (if (cerror "try-phosphorylated-adenosine://127.0.0.1:0/"
              "ICANN LET YOU REQUEST ~A, DAVE!"
              "a single uniform resource locator")
      (let ((anger (intern (read-line) (load-time-value *package*)))
            (rage (bybit-path *base-domain* (read-line)))) #|  _,.-'
        I am special, and thus issue declarations at runtime! |#
        (proclaim `(special ,anger))    ;_; PROBLEM?
        (proclaim `(declaration of pure unadulterated rage , 'calmly))
        (set anger rage))
      (with-slots (markets assets) exchange
        (setf (values markets assets) (get-info)))))

(defclass bybit-gate (gate) ((exchange :initform *bybit*)))

(defmethod gate-post ((gate (eql *bybit*)) key secret request)
  (destructuring-bind ((verb method) . parameters) request
    (multiple-value-bind (ret code status)
        (auth-request verb method key secret parameters)
      `(,ret ,(awhen1 (acase code
                        ((0)) ; C-style, no error
                        ;; TODO: lots of DEFINE-CONDITION ... see
                        ((20001 30032 30037)) ; filled before cancellation
                        ;; ((30076) it)          ; failures to replace
                        (t status))     ; by default, echo the error text
                (typecase it
                  (string (warn (format () "#~D ~A" code it)))))))))

(defmethod shared-initialize ((gate bybit-gate) names &key pubkey secret)
  (multiple-value-call #'call-next-method gate names
                       (mvwrap pubkey make-key) (mvwrap secret make-signer)))

;;;
;;; Public Data API
;;;

(defmethod get-book ((market bybit-market) &key count &aux (pair (name market)))
  (loop
    for raw in (public-request "orderBook/L2" `(("symbol" . ,pair)))
    for price = (parse-float (getjso "price" raw))
    for type = (string-case ((getjso "side" raw))
		 ("Sell" 'ask) ("Buy" 'bid))
    for offer = (make-instance
		 type :market market
                 :price (* (expt 10 (decimals market)) price)
                 :volume (string-case (pair)
			   ("BTCUSD" (/ (getjso "size" raw) price))
			   ("BTCUSDZ22" (/ (getjso "size" raw) price))
			   ("BTCUSDH23" (/ (getjso "size" raw) price))
			   ("BTCUSDT" (getjso "size" raw))))
    if (eq type 'ask) collect offer into asks
    if (eq type 'bid) collect offer into bids
    finally (return (values asks bids))))

(defmethod trades-since ((market bybit-market) &optional from
                         &aux (pair (name market)))
  (flet ((parse (trade)
           (with-json-slots (id side time qty price) trade
             (make-instance
	      'trade :market market :direction side
	      :txid (string-case (pair)
		      ("BTCUSD" id)
		      ("BTCUSDZ22" id)
		      ("BTCUSDH23" id)
		      ("BTCUSDT" (parse-integer id)))
	      :timestamp (parse-timestring time)
	      :volume (/ qty price) :price price :cost qty))))
    (awhen (string-case (pair)
	     ("BTCUSD" (public-request
			"trading-records"
			`(("symbol" . ,pair) ("limit" . 1000))))
	     ("BTCUSDZ22" (public-request
			   "trading-records"
			   `(("symbol" . ,pair) ("limit" . 1000))))
	     ("BTCUSDH23" (public-request
			   "trading-records"
			   `(("symbol" . ,pair) ("limit" . 1000))))
	     ("BTCUSDT" (public-request
			 "linear/recent-trading-records"
			 `(("symbol" . ,pair) ("limit" . 10))
			 "/public/")))
      (let ((trades (nreverse (mapcar #'parse it))))
	(if (null from) trades
	    (aif (member (txid from) trades :key #'txid :test #'=)
		 (cdr it) trades))))))

(deftype maybe-latest (minutes) `(or null (mod ,minutes)))
(deftype maybe-timestamp () '(or null timestamp))

(defun recent-splashes (market &key (count () countp) from &aux all)
  (check-type count (maybe-latest 1001) "Those who can't count, don't!")
  (check-type from maybe-timestamp "Please fart into the breathalyzer.")
  (when (and countp (null from))
    (warn "Please use the default window for latest data."))
  (awhen (public-request "liq-records"
                         `(("symbol" . ,(name market))
                           ,@(when count `(("limit" . ,count)))
                           ;; yajahrhrhr, pols pos kiss stfu gtfo
                           ,@(when from `(("start_time"
                                           .,(prin1-to-string
                                              (timestamp-to-unix from)))))))
    (dolist (row it all)
      (with-json-slots (time . #1=(side qty price)) row
        (push (list (multiple-value-bind (seconds milliseconds)
                        (floor time 1000)
                      (unix-to-timestamp seconds
                                         :nsec (* 1000000 milliseconds)))
                    . #1#)
              all)))))

;;;
;;; Private Data API
;;;

(defmethod placed-offers ((gate bybit-gate) ; FIXME: BTCUSD-specific
                          &optional (market (find-market "BTCUSD" :bybit)))
  (let ((url (string-case ((name market))
	       ("BTCUSD" "/v2/private/order")
	       ("BTCUSDT" "/private/linear/order/search")
	       ("BTCUSDZ22" "/futures/private/order")
	       ("BTCUSDH23" "/futures/private/order"))))
    (labels ((parse-offer (json)
               (with-json-slots (side price (oid "order_id") qty) json
		 (let ((price (etypecase price
				(string (parse-float price))
				(rational price))))
		   (make-instance 'offered
				  :oid oid :market market
				  :volume (string-case ((name market))
					    ("BTCUSD" (/ qty price))
					    ("BTCUSDZ22" (/ qty price))
					    ("BTCUSDH23" (/ qty price))
					    ("BTCUSDT" qty))
				  :price (* price
					    (expt 10 (decimals market))
					    (if (string= side "Sell")
						1 -1)))))))
      (aif (gate-request gate `(:get ,url) `(("symbol" . ,(name market))))
           (mapcar #'parse-offer it)
           ;; FIXME: ret_code 0 & msg=OK mean that there are no orders
           (warn "Request failed; active orders may be unreported!")))))

;;; TODO: meld this one into account-position (gate &optional market)
(defun all-positions (gate)
  (flet ((fetch (url)
	   (remove 0 (mapcar (getjso "data")
			     (gate-request gate `(:get ,url)))
		   :key (getjso "size")))) ; linear and inverse data
    (values-list (mapcar #'fetch	   ; have similar structures
			 '("/v2/private/position/list"
			   "/private/linear/position/list"
			   "/futures/private/position/list")))))

(defun hedged-position (gate market)
  (awhen (gate-request gate '(:get "/private/linear/position/list")
		       `(("symbol" . ,(name market))))
    (with-slots (primary counter) market
      (flet ((parse-position (json)
	       (with-json-slots ((entry "entry_price")
				 (value "position_value")
				 symbol size side) json
		 (let ((unit (if (string= side "Buy") -1 1)))
		   `(,(cons-mp* market  (* entry   unit))
		     ,(cons-aq* primary (* size (- unit)))
		     ,(cons-aq* counter (* value   unit)))))))
	(values (mapcar #'parse-position it) it)))))

(defun account-position (gate market &aux (name (name market)))
  (let ((url (string-case (name)
	       ("BTCUSD" "/v2/private/position/list")
	       ("BTCUSDZ22" "/futures/private/position/list")
	       ("BTCUSDH23" "/futures/private/position/list"))))
    (awhen (gate-request gate `(:get ,url) `(("symbol" . ,name)))
      (with-json-slots ((entry "entry_price") symbol size side
			(value "position_value"))
		       (if (string= name "BTCUSD") it
			   (getjso "data" (car it)))
	(with-slots (primary counter) market
          (values `(,(cons-mp* market
			       (* (parse-float entry)
                                  (if (string= side "Buy") -1 1)))
                    ,(cons-aq* primary
			       (* (parse-float value)
                                  (if (string= side "Buy") 1 -1)))
                    ,(cons-aq counter
			      (* size (if (string= side "Buy") -1 1))))
                  (if (string= name "BTCUSD") it
		      (getjso "data" (car it)))))))))

(defmethod hedged-balances ((gate bybit-gate) &aux balances)
  (awhen (gate-request gate '(:get "/v2/private/wallet/balance"))
    (awhen (remove 0 it :key #'cdadr)
      (multiple-value-bind (inverse linear futures)
	  (all-positions gate)
	(let ((tickers (intersection
			(delete-duplicates
			 (mapcar (getjso "symbol")
				 (append inverse linear))
			 :test #'string=)
			(public-request "tickers" ())
			:key (getjso "symbol") :test #'string=)))
	  (dolist (position inverse)
	    (with-json-slots
		((value "position_value") leverage size symbol side)
		position
	      (let* ((unit (if (string= side "Buy") -1 1))
		     (fund (* (getjso "equity"
				      (getjso (string-case (symbol)
						("BTCUSD" "BTC")
						("BTCUSDT" "USDT"))
					      it))
			      (parse-float leverage)))
		     (mark (getjso "mark_price"
				   (find symbol tickers
					 :key (getjso "symbol")
					 :test #'string=))))
		(with-slots (primary counter)
		    (find-market symbol *bybit*)
		  (push (cons-aq* primary
				  (+ fund (* (parse-float value)
					     (- unit))))
			balances)
		  (push (cons-aq* counter
				  (+ (* fund (parse-float mark))
				     (* size unit)))
			balances)))))))))
  (with-json-slots ((mark "mark_price"))
		   (car (public-request "tickers" `(("symbol" . "BTCUSD"))))
    (if position
	(let ((fund (* equity
		       (parse-float (getjso "leverage" json)
				    :type 'rational))))
          (list (aq+ (cons-aq* primary fund)
		     (second position))
		(aq+ (cons-aq* counter
			       (* fund (parse-float mark)))
                     (third position))))
	(list (cons-aq* primary equity)
	      (cons-aq* counter
			(* equity (parse-float mark)))))))

(defmethod account-balances ((gate bybit-gate) &aux balances)
  (awhen (gate-request gate '(:get "/v2/private/wallet/balance"))
    (with-json-slots ((deposit "BTC")) it
      (with-json-slots ((mark "mark_price"))
		       (car (public-request "tickers" `(("symbol" . "BTCUSD"))))
        (with-json-slots (equity) deposit
	  (dolist (name '("BTCUSD" "BTCUSDZ22" "BTCUSDH23") balances)
	    (with-aslots (primary counter) (find-market name :bybit)
	      (multiple-value-bind (position json)
		  (account-position gate it)
		(if position
		    (let ((fund (* equity
				   (parse-float (getjso "leverage" json)
						:type 'rational))))
		      (push (aq+ (cons-aq* primary fund)
				 (second position))
			    balances)
		      (push (aq+ (cons-aq* counter
					   (* fund (parse-float mark)))
				 (third position))
			    balances))
		    (progn (push (cons-aq* primary equity) balances)
			   (push (cons-aq* counter
					   (* equity (parse-float mark)))
				 balances)))))))))))

;;; This horror can be avoided via the actor-delegate mechanism.
(defmethod market-fee ((gate bybit-gate) (market bybit-market)) (fee market))

(defmethod parse-timestamp ((exchange (eql *bybit*)) (timestamp integer))
  (multiple-value-bind (unix milliseconds) (floor timestamp 1000)
    (unix-to-timestamp unix :nsec (* 1000000 milliseconds))))

(defun parse-execution (raw)
  (with-json-slots ((oid "order_id") (txid "exec_id") (amt "exec_qty")
                    symbol side (price "exec_price") (time "trade_time_ms")
                    (execost "exec_value") (exefee "exec_fee"))
      raw
    (unless (zerop (length side))
      (let ((market (find-market symbol :bybit)))
        (flet ((adjust (value) (parse-float value)))
          (let ((volume (adjust execost)) (fee (adjust exefee)))
            (make-instance 'execution :direction side :market market
                           :oid oid :txid txid :cost amt :net-cost amt
                           :price (adjust price) :volume (abs volume)
                           :timestamp (parse-timestamp *bybit* time)
                           :net-volume (abs (+ volume fee)))))))))

(defun raw-executions (gate &key pair from count)
  (macrolet ((params (&body body)
               `(append ,@(loop for (val key exp) in body
                             collect `(when ,val `((,,key . ,,exp)))))))
    (let ((url (string-case (pair)
		 ("BTCUSD" "/v2/private/execution/list")
		 ("BTCUSDZ22" "/futures/private/execution/list")
		 ("BTCUSDH23" "/futures/private/execution/list"))))
      (awhen (gate-request gate `(:get ,url)
                           (params (pair "symbol" pair)
				   (count "limit" count)
                                   (from "start_time"
					 (princ-to-string
                                          (+ (* 1000 (timestamp-to-unix
						      from))
                                             (floor (nsec-of from)
						    1000000))))))
	(getjso "trade_list" it)))))

(defmethod execution-since ((gate bybit-gate) market since)
  (awhen (raw-executions gate :pair (name market) :count "200"
                         :from (if since (timestamp since)
                                   (timestamp- (now) 3 :day)))
    (mapcar #'parse-execution
            (remove "Trade" 
		    (if (null since) it
			(subseq it (1+ (position
					(txid since) it :test #'string=
					:key (getjso "exec_id")))))
		    :test-not #'string= :key (getjso "exec_type")))))

(defun post-raw-limit (gate buyp market price size)
  (let ((url (string-case (market)
	       ("BTCUSD" "/v2/private/order/create")
	       ("BTCUSDZ22" "/futures/private/order/create")
	       ("BTCUSDH23" "/futures/private/order/create"))))
    (gate-request gate `(:post ,url)
                  `(("symbol" . ,market) ("price" . ,price)
		    ("qty" . ,(princ-to-string (floor size)))
		    ("side" .,(if buyp "Buy" "Sell"))
		    ("order_type" . "Limit")
		    ("time_in_force" . "PostOnly")))))

(defun post-linear-open (gate market buyp price size)
  (gate-request gate '(:post "/private/linear/order/create")
		`(("symbol" . ,market) ("price" . ,price)
		  ("qty" . ,(princ-to-string size))
		  ("side" .,(if buyp "Buy" "Sell"))
		  ("position_idx" . ,(if buyp "1" "2"))
		  ("order_type" . "Limit")
		  ("reduce_only" . "False")
		  ("close_on_trigger" . "False")
		  ("time_in_force" . "PostOnly"))))

(defclass placed (offered) ((unique-request-identifier :initarg :urid)))

(defmethod post-offer ((gate bybit-gate) (offer offer))
  (with-slots (market volume price) offer
    (let ((factor (expt 10 (decimals market))))
      (multiple-value-bind (json complaint)
          (post-raw-limit gate (not (plusp price)) (name market)
                        (multiple-value-bind (int dec)
                            (floor (abs (/ (floor price 1/2) 2)) factor)
                          (format nil "~D.~V,'0D"
                                  int (max 1 (decimals market)) (* 10 dec)))
                        (floor (* volume (if (minusp price) 1
                                             (/ price factor)))))
        (with-json-slots ((oid "order_id") (status "order_status")) json
          (if (and json (equal status "Created"))
              (change-class offer 'offered :oid oid
                            ;; :urid (reduce #'getjso '("o_req_num" "ext_fields")
                            ;;               :from-end t :initial-value json)
                            )
              (warn "Failed placing: ~S~%~A" offer complaint)))))))

(defun amend-raw-offer (gate market oid size &optional price)
  (let ((url (string-case (market)
	       ("BTCUSD" "/v2/private/order/replace")
	       ("BTCUSDZ22" "/futures/private/order/replace")
	       ("BTCUSDH23" "/futures/private/order/replace"))))
    (gate-request gate `(:post ,url)
                  `(("symbol" . ,market) ("order_id" . ,oid)
                    ("p_r_qty" . ,(princ-to-string size))
                    ,@(when price `(("p_r_price" . ,price)))))))

(defmethod amend-offer ((gate bybit-gate) (old offered) (new offer))
  (with-slots (market oid) old
    (with-slots (volume price) new
      (let* ((factor (expt 10 (decimals market))))
        (multiple-value-bind (json complaint)
            (amend-raw-offer
             gate (name market) oid
             (floor (* volume (if (minusp price) 1
                                  (/ price factor))))
             (unless (= (realpart price) (realpart (price old)))
               (multiple-value-bind (int dec)
                   (floor (abs (/ (floor price 1/2) 2)) factor)
                 (format () "~D.~V,'0D" int
                         (max 1 (decimals market)) (* 10 dec)))))
          (if complaint
	      ;; most common atm is probably (not (/= 1 1))
              (warn (format () "~&~A was not modified to ~A~%~A~%"
			    old new complaint))
              (with-json-slots ((returned-oid "order_id")) json
                (if (string= returned-oid oid) ; how can this test fail?
                    (reinitialize-instance     ; not rhetoric, enumerate
                     old :volume volume :price price))))))))) ; or don't

;;; FIXME:
;;; The PostOnly option is, quite infuriatingly, only respected after
;;; the Internet-facing endpoint has already confirmed the offer; thus,
;;; there is a need for a followup that confirms offer placement, at least
;;; half a second after the initial claim that it's been placed.

(defmethod cancel-offer ((gate bybit-gate) (offer offered))
  (let* ((symbol (name (market offer)))
	 (url (string-case (symbol)
		("BTCUSD" "/v2/private/order/cancel")
		("BTCUSDZ22" "/futures/private/order/cancel")
		("BTCUSDH23" "/futures/private/order/cancel"))))
   (multiple-value-bind (ret err)
       (gate-request gate `(:post ,url)
                     `(("symbol" . ,(name (market offer)))
                       ("order_id" . ,(oid offer))))
     (or ret (and (null err)
		  (string= (oid offer) (getjso "order_id" ret)))))))

(defclass bybit-prioritizer (prioritizer) ())

(defmethod prioriteaze ((ope bybit-prioritizer) target placed
                        &aux to-add (excess placed))
  (flet ((frob (add pop &aux (max (max (length add) (length pop))))
           (with-slots (expt) ope
             (let* ((n (expt (random (expt max (/ expt))) expt))
                    (add (nth (floor n) add))
                    (pop (nth (- max (ceiling n)) pop)))
               (if (and add pop)
                   (amend-offer (slot-reduce ope supplicant gate) pop add)
                   (if add (ope-place ope add) (ope-cancel ope pop)))))))
    (aif (dolist (new target (sort to-add #'< :key #'price))
           (aif (find (price new) excess :key #'price :test #'=)
                (setf excess (remove it excess)) (push new to-add)))
         (frob it (reverse excess))   ; which of these is worst?
         (if excess (frob () excess)  ; choose the lesser weevil
             (and target placed (= (length target) (length placed))
                  (loop for new in target and old in placed
                     when (sufficiently-different? new old)
                     collect new into news and collect old into olds
                     finally (when news (frob news olds))))))))

(defun wipe-offers (supplicant)
  (multiple-value-bind (ret err)
      (with-slots (gate market) supplicant
        (gate-request gate '(:post "/v2/private/order/cancelAll")
                      `(("symbol" . ,(name market)))))
    (or err ret)))

;;;
;;; General Introspection
;;;

(defun daily-returns (gate &optional count)
  ;; (mapcar 'funcall (mapcar #'getjso '("coin" "address")))
  (awhen (gate-request gate '(:get "/v2/private/wallet/fund/records")
                       (when count `(("limit" . ,(princ-to-string count)))))
    (mapcar (lambda (json)
              (with-json-slots
                  ((balance "wallet_balance") (date "exec_time")
		   amount coin address)
		  json
		(let ((asset (find-asset (concatenate 'string
						      coin "-" address)
					 *bybit*)))
                  (list (parse-timestring date)
			(cons-aq* asset (parse-float balance))
			(cons-aq* asset (parse-float amount))))))
            (remove "RealisedPNL" (getjso "data" it)
                    :test-not #'string= :key (getjso "type")))))

(defun fetch-pages (gate request options &optional (count 50)
                    (data-key "data") &aux (number 1))
  (flet ((fetch-page ()
           (gate-request gate request
                         (acons "page" (format () "~D" number) options))))
    (loop for page = (fetch-page) when page append (getjso data-key page)
       and if (aif (getjso "last_page" page)
                   (< (getjso "current_page" page) it)
                   (when count (< number count)))
       do (incf number) else do (loop-finish))))

(defun recent-closes (gate &optional (count 25) &aux (sum 0s0))
  (check-type count (integer 1 2501))
  (awhen (fetch-pages gate '(:get "/v2/private/trade/closed-pnl/list")
                      `(("symbol" . "BTCUSD") .
                        ,(when count
			   `(("limit"
			      . ,(princ-to-string count))))) ; lol!
                      (when count (ceiling count 50)))
    (values (mapcar (lambda (json)
                      (with-json-slots
                          ((price "order_price") side qty
                           (unix "created_at") (pnl "closed_pnl")) json
                        (incf sum pnl)
                        (list (unix-to-timestamp unix) side qty price pnl)))
                    it)
            sum)))

(defun liquidity-contribution-score (gate &optional (symbol "BTCUSD"))
  (awhen (gate-request gate '(:get "/v2/private/account/lcp")
                       `(("symbol" .,symbol)))
    (mapcar (lambda (daily-metrics)
              (with-json-slots ((theirs "platform_ratio") date
                                (ours       "self_ratio") score)
                  daily-metrics
                (list (parse-timestring date) score ours theirs)))
            (getjso "lcp_list" it))))

(defmethod describe-account
    ((supplicant t) (exchange (eql *bybit*)) (*standard-output* t))
 (with-slots (gate) supplicant
    ;;; there's a pattern here, and it belongs in the fucking symbol-plists:
   (format t "~&~%@#$%& Dump Of Recent Closes &%$#@~%")
   (multiple-value-bind (rhos total)	; blessed are l'hoi pollois, ne?
       (apply #'recent-closes gate
	      (and (eq *standard-output* *debug-io*) '(2500)))
     (dolist (row rhos (values total (log (1+ total))))
       (destructuring-bind #1=(second action comment quanta tide) row
                           (format t "~&~10A ~4A ~5D ~1,6$ ~8$~%" . #1#))))
   (format t "~&~%@#$%& Dump Of Recent Daily Earnings &%$#@~%")
   (dolist (row (daily-returns gate))
     (destructuring-bind #2=(day coins flux) row
       (format t "~&~10A ~10A ~11@A~%" . #2#)))
   (format t "~&~%@#$%& Dump Of Recent Liquidity Scores &%$#@~%")
   (dolist (day (liquidity-contribution-score gate))
     (format t "~&~{~D ~F ~F ~F~}~%" day))
    ;; can you smell that pattern? ... symbol-plists are THE WORST THING EVER!
    ))


;;;
;;; What's this one whore's Morty doing in a three-card shinachop?
;;;

(defmethod bases-for ((supplicant supplicant) (market bybit-market))
  (with-slots (gate) supplicant
    (awhen (account-position gate market)
      (let ((entry (realpart (first it)))
	    (size (abs (quantity (third it)))))
        (flet ((foolish (basis &aux (price (realpart (car basis))))
                 (if (= (signum price) (signum entry)) (> price entry)
		     ;; N.B.: this logic allows bleeding to reduce chance
		     ;; of liquidation; ...both hardcoded and ineffective
                     (and (< (isqrt size) (quantity (second basis)))
                          (< (isqrt size) (quantity (third basis))))
		     )))
          (multiple-value-bind (primary counter) (call-next-method)
	    (if (plusp entry)
		(values () (remove-if #'foolish counter))
		(values (remove-if #'foolish primary) ()))
            ;; (values (remove-if #'foolish primary)
            ;;         (remove-if #'foolish counter))
	    ))))))

;;; This one belongs in the Ten Decades' Hayt
(defun ditch-unplaced-offereds (maker &optional (stream *standard-output*))
  (declare (ignorable stream))
  (flet ((query (stale)
           (gate-request (slot-reduce maker gate)
                         '(:get "/v2/private/order")
                         `(("order_id" . ,(oid stale))
                           ("symbol" . "BTCUSD"))))
         (compare (old new)
           (set-difference old new :key #'oid :test #'string=)))
    (with-slots (offered control response order-slots)
        (slot-reduce maker supplicant)
      (let ((stale (copy-list offered))
            (fresh (progn (send control '(:sync)) (recv response))))
        (values (complex (- order-slots (length stale))
                         (- order-slots (length fresh)))
                (compare fresh stale)
                (remove "EC_PostOnlyWillTakeLiquidity"  ; don't think too much
                        (mapcar #'query (compare stale fresh)) ; about names!
                        :key (getjso "reject_reason") :test-not #'string=))))))

(defun estimate-funding-dividend (gate market)
  ;; oughtta return two triples: {dividend, rate, position} x {this,next}
  ;; although it's worth considering both a five-valued ordering,
  ;;   xor a sequence expressing plausible regions for the second
  (with-json-slots ((numerator "predicted_funding_fee")
                    (predicted "predicted_funding_rate"))
      (gate-request gate '(:get "/open-api/funding/predicted-funding")
                    `(("symbol" . ,(name market))))
    (with-json-slots ((rate "funding_rate"))
        (gate-request gate '(:get "/open-api/funding/prev-funding-rate")
                      `(("symbol" . ,(name market))))
      (values (list (cons-aq* (primary market)
                              (- (* (/ numerator predicted) (parse-float rate))))
                    rate (cons-aq* (primary market) (/ numerator predicted)))
              (list (cons-aq* (primary market) (- numerator)) predicted
                    (cons-aq* (primary market) (/ numerator predicted)))))))

;;;
;;; Websocket
;;;
(defparameter *websocket-url* "wss://stream.bybit.com/realtime")

(defun make-websocket-handler (client table market book
                               &aux (price-factor (expt 10 (decimals market))))
  (flet ((offer (side size price &aux (mp (* price price-factor)))
           (make-instance (string-case (side) ("Sell" 'ask) ("Buy" 'bid))
                          :market market :price mp :volume (/ size price))))
    (macrolet ((do-data ((data &optional price) &body body)
                 `(dolist (item ,data)
                    (with-json-slots (id . #1=(side size price)) item
                      (let ,(when price `((price ,price))),@body))))
               (store-offer ()
                 `(setf (gethash id book) `(,price . ,(offer . #1#)))))
      (lambda (raw &aux (message (read-json raw)))
        (case (caar message)
          (:|success|
            (with-json-slots (success request) message
              (unless (with-json-slots (op args) request
                        (and success (string= op "subscribe")
                             (string= (first args) table)))
                (wsd:close-connection client))))
          (:|topic|
            (with-json-slots (topic type data) message
              (if (string= table topic)
                  (string-case (type)
                    ("delta"
                     (with-json-slots (delete update insert) data
                       (do-data (update (/ id 10000))
                         (aif (gethash id book)
                              (rplacd it (offer . #1#)) (store-offer)))
                       (do-data (insert (parse-float price :type 'rational))
                         (store-offer))
                       (do-data (delete) (remhash id book))
                       ))
                    ("snapshot"
                     (clrhash book)
                     (do-data (data (parse-float price :type 'rational))
                       (store-offer)))
                    (t (wsd:close-connection client)
                       (error "unknown orderbook action: ~s" type)))
                  (wsd:close-connection client)))))))))

(defun make-orderbook-socket (market &optional old-book (depth 200) (frequency 100))
  (let* ((book (or old-book (make-hash-table :test #'eql)))
         (topic (format () "orderBook~@[_~D~].~Dms.~A" depth frequency
                        (name market)))
         (client (wsd:make-client *websocket-url*)))
    (wsd:on :message client (make-websocket-handler client topic market book))
    (wsd:start-connection client)
    (wsd:send client (format () "{\"op\":\"subscribe\",\"args\":[~S]}" topic))
    (values book client)))

;;; "There are only two Hard Problems of Consciousness:
;;;    the false dichotomy between the problems, and
;;;      prioritisation of hypothesis falsification."
(defun make-liquidity-background (market minimum maximum step
                                  &aux (price-factor (expt 10 (decimals market))))
  (let ((size (1+ (ceiling (- maximum minimum) step))))
    (let ((bids (make-hash-table :test #'eql :size size))
          (asks (make-hash-table :test #'eql :size size)))
      (loop for cdr on
           (loop with descending for price from minimum to maximum by step
              for mp = (* price price-factor)
              for volume = (/ (1+ (random (expt size (random 1.5)))) price)
              for ask = (make-instance 'ask :market market
                                       :price mp :volume volume)
              for bid = (make-instance 'bid :market market
                                       :price mp :volume volume)
              collect ask do (setf (gethash (price bid) bids)
                                   (push bid descending)))
         do (setf (gethash (price (car cdr)) asks) cdr))
      (values bids asks))))

(defclass streaming-market (bybit-market) (socket book-table background))

(defmethod shared-initialize :after ((market streaming-market) (names t)
                                     &key minimum maximum step)
  (with-slots (socket book-table background) market
    (setf (values book-table socket)
          (if (slot-boundp market 'book-table)
              (make-orderbook-socket market book-table)
              (make-orderbook-socket market)))
    (when (and minimum maximum step)
      (setf background
            (multiple-value-call #'cons
              (make-liquidity-background market minimum maximum step))))))

(defmethod get-book :before ((market streaming-market) &key)
  (with-slots (book-table socket) market
    (unless (and (slot-boundp market 'socket) (slot-boundp market 'book-table))
      (loop for delay from 1
         do (handler-case
                (return (setf (values book-table socket)
                              (make-orderbook-socket market)))
              (error (error) (warn "~&Encountered:~%~A~%" error)))
           (sleep delay))
      (sleep 3))))

(defmethod get-book ((market streaming-market) &key)
  (if (not (slot-boundp market 'book-table)) (values () ())
      (with-slots (book-table background) market
        (loop for (price . offer) being each hash-value of book-table
           if (eq (type-of offer) 'ask) collect offer into asks
           if (eq (type-of offer) 'bid) collect offer into bids
           finally (return
                     (let ((asks (sort asks #'> :key #'price))
                           (bids (sort bids #'> :key #'price)))
                       (values (nconc (nreverse asks)
                                      (cdr (gethash (price (first asks))
                                                    (cdr background))))
                               (nconc (nreverse bids)
                                      (cdr (gethash (price (first bids))
                                                    (car background)))))))))))

(defmethod get-book :after ((market streaming-market) &key)
  (with-slots (book-table socket) market
    (when (slot-boundp market 'socket)
      (loop for delay from 1 while (eq (wsd:ready-state socket) :closed)
         do (handler-case
                (return (setf (values book-table socket)
                              (make-orderbook-socket market)))
              (error (error) (warn "~&Encountered:~%~A~%" error)))
           (sleep delay)))))
