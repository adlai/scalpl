(defpackage #:scalpl.bitmex
  (:nicknames #:bitmex) (:export #:*bitmex* #:bitmex-gate #:swagger)
  (:use #:cl #:base64 #:chanl #:anaphora #:local-time #:scalpl.util
        #:scalpl.actor #:scalpl.exchange))

(in-package #:scalpl.bitmex)

;;; General Parameters
(defparameter *base-domain* "www.bitmex.com")
(defparameter *base-url* (format () "https://~A" *base-domain*))
(setf cl+ssl:*make-ssl-client-stream-verify-default* ())

(defvar *bitmex* (make-instance 'exchange :name :bitmex :sensitivity 1))

(defclass bitmex-market (market)
  ((exchange :initform *bitmex*) (fee :initarg :fee :reader fee)
   (symbol-index :initarg :index)))

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
    (case status
      ((500 502 504) (values () status body))
      (t (awhen (getjso :x-ratelimit-remaining headers)
           (sleep (+ 1 (random 1.0) (dbz-guard (/ (1- (parse-integer it)))))))
         (if (= status 200) (values (decode-json body) 200 ())
             (values () status (getjso "error" (decode-json body))))))))

(defun bitmex-path (&rest paths) (format () "/api/~{~A~}" paths))

(defun swagger (&key) (bitmex-request (bitmex-path "explorer/swagger.json")))
(defparameter *swagger* (swagger))

(defun public-request (method parameters)
  (bitmex-request
   (apply #'bitmex-path "v1/" method
          (and parameters `("?" ,(net.aserve:uridecode-string
                                  (urlencode-params parameters)))))))

(defun auth-request (verb method key signer &optional params)
  (let* ((data (urlencode-params params))
         (path (apply #'bitmex-path "v1/" method
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
  (awhen (public-request "instrument" '(("count" . 500)))
    (flet ((make-market (instrument index)
             (with-json-slots
                 ((tick "tickSize") (lot "lotSize") (fee "takerFee")
                  (name "symbol") (long "rootSymbol") (short "quoteCurrency")
                  multiplier state)
                 instrument
               (flet ((asset (fake &optional (decimals 0))
                        (let ((name (concatenate 'string fake "-" name)))
                          (or (find name assets :key #'name :test #'string=)
                              (aprog1 (make-instance 'asset :name name
                                                     :decimals decimals)
                                (push it assets)))))
                      (ilog (i) (floor (log (abs i) 10))))
                 (when (string= state "Open")
                   (make-instance
                    'bitmex-market :name name :fee fee
                    :decimals (- (ilog tick)) :index index
                    :primary (asset long (ilog multiplier))
                    :counter (asset short (ilog lot))))))))
      (values (loop for definition in it and index from 0
                 for market = (make-market definition index)
                 when market collect it)
              assets))))

(defmethod fetch-exchange-data ((exchange (eql *bitmex*)))
  (with-slots (markets assets) exchange
    (setf (values markets assets) (get-info))))

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

(defmethod trades-since ((market bitmex-market)
                         &optional (since (timestamp- (now) 1 :minute))
                         &aux (pair (name market)))
  (flet ((parse (trade)
           (with-json-slots (side timestamp size price) trade
             (make-instance 'trade :market market :direction side
                            :timestamp (parse-timestring timestamp)
                            :volume (/ size price) :price price :cost size))))
    (alet (mapcar #'parse
                  (public-request
                   "trade" `(("symbol" . ,pair) ("count" . 100)
                             ("startTime" .,(format-timestring
                                             () (timestamp- (now) 1 :minute)
                                             :timezone +utc-zone+)))))
      (if (not since) it (remove (timestamp since) it
                                 :test #'timestamp> :key #'timestamp)))))

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

(defmethod account-positions ((gate bitmex-gate) &aux positions)
  (awhen (remove-if-not (getjso "isOpen")
                        (gate-request gate '(:get "position") ()))
    (dolist (position it (values positions it))
      (with-json-slots ((entry "avgEntryPrice") symbol
                        (size "currentQty") (cost "posCost"))
          position
        (with-aslots (primary counter) (find-market symbol :bitmex)
          (push (list it (cons-mp* it (* entry (- (signum size))))
                      (cons-aq primary (- cost))
                      (cons-aq counter (- size)))
                positions))))))

(defmethod account-balances ((gate bitmex-gate) &aux balances)
  ;; tl;dr - transubstantiates position into 'balances' of long + short
  (flet ((collect (a b) (push a balances) (push b balances)))
    (let ((positions (account-positions gate))
          (instruments (public-request "instrument/active" ()))
          (deposit (gate-request gate '(:get "user/wallet") ())))
      (when deposit
        (dolist (instrument instruments balances)
          (with-json-slots (symbol (mark "markPrice") state) instrument
            (unless (string= state "Unlisted")
              (with-aslots (primary counter) (find-market symbol :bitmex)
                (let ((fund (/ (getjso "amount" deposit) 1/5 ; idk
                               (expt 10 (decimals primary)))))
                  (aif (find it positions :key #'car)
                       (collect (aq+ (cons-aq* primary fund) (third it))
                         (aq+ (cons-aq* counter (* fund mark)) (fourth it)))
                       (collect (cons-aq* primary fund)
                         (cons-aq* counter (* fund mark)))))))))))))

;;; This horror can be avoided via the actor-delegate mechanism.
(defmethod market-fee ((gate bitmex-gate) (market bitmex-market)) (fee market))
(defmethod market-fee ((gate bitmex-gate) market)
  (fee (slot-reduce market scalpl.exchange::%market)))

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
                          (from "startTime" from)
                          (end "endTime" (subseq (princ-to-string end) 0 19))))))

(defmethod parse-timestamp ((exchange (eql *bitmex*)) (timestamp string))
  (parse-rfc3339-timestring timestamp))

(defmethod execution-since ((gate bitmex-gate) market since)
  (awhen (raw-executions gate :pair (name market)
                         :from (if since (timestamp since)
                                   (timestamp- (now) 5 :day)))
    (mapcan #'parse-execution
            (if (null since) it
                (subseq it (1+ (position (txid since) it
                                         :test #'string= :key #'cdar)))))))

(defun post-raw-limit (gate buyp market price size)
  (gate-request gate '(:post "order")
                `(("symbol" . ,market) ("price" . ,price)
                  ("orderQty" . ,(princ-to-string
                                  (* (if buyp 1 -1) (floor size))))
                  ("execInst" . "ParticipateDoNotInitiate"))))

(defmethod post-offer ((gate bitmex-gate) offer)
  (with-slots (market volume price) offer
    (let ((factor (expt 10 (decimals market))))
      (with-json-slots ((oid "orderID") (status "ordStatus") text)
          (post-raw-limit gate (not (plusp price)) (name market)
                          (multiple-value-bind (int dec)
                              (floor (abs (/ (floor price 1/2) 2)) factor)
                            (format nil "~D.~V,'0D"
                                    int (max 1 (decimals market)) (* 10 dec)))
                          (floor (* volume (if (minusp price) 1
                                               (/ price factor)))))
        (if (equal status "New") (change-class offer 'placed :oid oid)
            (unless (search "ParticipateDoNotInitiate" text)
              (warn "Failed placing: ~S~%~A" offer text)))))))

(defmethod cancel-offer ((gate bitmex-gate) (offer placed))
  (multiple-value-bind (ret err)
      (gate-request gate '(:delete "order") `(("orderID" . ,(oid offer))))
    (unless (string= err "Not Found")
      (string-case ((if ret (getjso "ordStatus" (car ret)) ""))
        ("Canceled") ("Filled")
        (t (warn "~A ~A" offer err))))))

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

(defmethod stub ((supplicant supplicant) (market bitmex-market))
  "\"Do not go gentle into that good night,
Old age should burn and rave at close of day;
Rage, rage against the dying of the light.

Though wise men at their end know dark is right,
Because their words had forked no lightning they
Do not go gentle into that good night.

Good men, the last wave by, crying how bright
Their frail deeds might have danced in a green bay,
Rage, rage against the dying of the light.

Wild men who caught and sang the sun in flight,
And learn, too late, they grieved it on its way,
Do not go gentle into that good night.

Grave men, near death, who see with blinding sight
Blind eyes could blaze like meteors and be gay,
Rage, rage against the dying of the light.

And you, my father, there on the sad height,
Curse, bless, me now with your fierce tears, I pray.
Do not go gentle into that good night.
Rage, rage against the dying of the light.\"

;; from Poems of Dylan Thomas, as published by New Directions Press
;; Dylan Marlais Thomas, born 1914-10-27 in South Wales, and died a
;; few decades hence on the streets of New York City.
"
  (cerror "Market orders should be confirmed manually!" ()))

;;;
;;; Websocket
;;;

(defparameter *websocket-url* (format () "wss://~A/realtime" *base-domain*))

(defun make-websocket-handler (client topic market book
                               &aux (next-expected :info)
                                 (price-factor (expt 10 (decimals market))))
  (flet ((offer (side size price &aux (mp (* price price-factor)))
                (make-instance (string-case (side) ("Sell" 'ask) ("Buy" 'bid))
                               :market market :price mp
                               :volume (/ size price))))
    (lambda (raw &aux (message (read-json raw)))
      (case next-expected
        (:info
         (if (string= (getjso "info" message)
                      "Welcome to the BitMEX Realtime API.")
             (setf next-expected :subscribe)
             (wsd:close-connection client)))
        (:subscribe
         (if (and (getjso "success" message)
                  (string= (getjso "subscribe" message) topic))
             (setf next-expected :table)
             (wsd:close-connection client)))
        (:table
         (with-json-slots (table action data) message
           (macrolet ((do-data ((&rest slots) &body body)
                        `(dolist (row data)
                           (with-json-slots ,slots row ,@body))))
             (flet ((build ()
                      (do-data (id side size price)
                        (setf (gethash id book)
                              (cons price (offer side size price))))))
               (string-case (table)
                 ("orderBookL2"
                  (if (zerop (hash-table-count book))
                      (when (string= action "partial") (build))
                      (string-case (action)
                        ("update"
                         (do-data (id side size)
                           (let ((cons (gethash id book)))
                             (rplacd cons (offer side size (car cons))))))
                        ("insert"
                         (do-data (id side size price)
                           (setf (gethash id book)
                                 (cons price (offer side size price)))))
                        ("delete" (do-data (id) (remhash id book)))
                        ("partial" (clrhash book) (build))
                        (t (wsd:close-connection client)
                           (error "unknown orderbook action: ~s" action)))))
                 (t (wsd:close-connection client)))))))))))

(defun connect-websocket-client (topic)
  (wsd:start-connection
   (wsd:make-client (format () "~A?subscribe=~A" *websocket-url* topic))))

(defun make-orderbook-socket (market)
  (let* ((book (make-hash-table :test #'eq))
         (topic (format () "orderBookL2:~A" (name market)))
         (client (connect-websocket-client topic)))
    (wsd:on :message client (make-websocket-handler client topic market book))
    (values book client)))

(defclass streaming-market (bitmex-market) (socket book-table))
(defmethod shared-initialize :after ((market streaming-market) (names t) &key)
  (with-slots (socket book-table) market
    (setf (values book-table socket) (make-orderbook-socket market))))

(defmethod get-book ((market streaming-market) &key)
  (with-slots (book-table) market
    ;; thanks to Clozure CL's warning about the unused variable,
    ;; an opportunity arises for mature optimization: #'cl:merge
    (loop for (price . offer) being each hash-value of book-table
       if (eq (type-of offer) 'ask) collect offer into asks
       if (eq (type-of offer) 'bid) collect offer into bids
       finally (return (values (sort asks #'< :key #'price)
                               (sort bids #'< :key #'price))))))

;;;
;;; Trollbox
;;;

(defun raw-chats (channel &optional count start (reverse t))
  (public-request "chat" `(,@(when channel `(("channelID" . ,channel)))
                           ,@(when start `(("start" . ,start)))
                           ,@(when count `(("count" . ,count)))
                           ("reverse" . ,(if reverse 1 0)))))

(defun dump-recent-chats (&optional (count 20) channel)
  (assert (<= 0 count 500) (count) "Count must be within [0,500]")
  (dolist (chat (nreverse (raw-chats channel count)))
    (with-json-slots (id date user message (lang "channelID")) chat
      (format t "~&~20A ~24A ~8D~:[ ~D~;~*~]~%~<   ~@;~A~:>~%"
              user date id channel lang (list message)))))

;;;
;;; Bulk Placement and Cancellation
;;;

(defun offer-alist (offer)
  (with-slots (market price given taken) offer
    (let ((buyp (minusp price)) (factor (expt 10 (decimals market))))
      (multiple-value-bind (int dec)
          (floor (abs (/ (floor price 1/2) 2)) factor)
        `(("symbol" . ,(name market))
          ("price" . ,(format () "~D.~V,'0D" int
                              (max 1 (decimals market)) dec))
          ("orderQty" . ,(princ-to-string
                          (* (- (signum price))
                             (scaled-quantity (if buyp given taken)))))
          ("execInst" . "ParticipateDoNotInitiate"))))))

(defun post-bulk (gate &rest offers)
  (let ((orders (format () "[~{~A~^,~}]"
                        (reduce 'mapcar '(json:encode-json-alist-to-string
                                          offer-alist)
                                :from-end t :initial-value offers))))
    (awhen (gate-request gate '(:post "order/bulk") `(("orders" . ,orders)))
      (loop for data in it for status = (getjso "ordStatus" data)
         when (equal status "New") collect
           (with-json-slots
               (symbol side price (oid "orderID") (size "orderQty")) data
             (let ((market (find-market symbol :bitmex))
                   (aksp (string-equal side "Sell")))
               (make-instance 'placed :oid oid :market market
                              :volume (/ size price)
                              :price (* price (if aksp 1 -1)
                                        (expt 10 (decimals market))))))))))

(defun cancel-bulk (gate &rest offers)
  (gate-request gate '(:delete "order")
                `(("orderID" . ,(mapcar #'oid offers)))))

;;;
;;; Rate Limiting, Naval Grazing, and other unsorted mercantilities
;;;

;;; macros for patterns occuring strictly fewer than thrice
;;; are considered premature optimization

(defun quote-fill-ratio (gate)
  (mapcar 'float
          (remove '() (mapcar (getjso "quoteFillRatioMavg7")
                              (gate-request
                               gate '(:get "user/quoteFillRatio") ())))))

(defun quote-value-ratio (gate &key symbols &aux piss dung)
  ;; #.(reduce 'getjso '("summary" :|get| "/user/quoteValueRatio" "paths")
  ;;           :from-end t :initial-value *swagger*) ; nice short docstring :)
  "Please read the exchange's official documentation for these statistics:

https://www.bitmex.com/app/tradingRules#Quote-Value-Ratio-Threshold"
  (awhen (gate-request gate '(:get "user/quoteValueRatio") ())
    (dolist (hourly-frame it (values piss dung))
      (with-json-slots ((ratio "QVR") (weight "volumeXBT") symbol
                        (count "quoteCount") account timestamp) hourly-frame
        (if dung (assert (= (car dung) account)) (push account dung))
        (push (list timestamp weight count ratio) piss)
        (when symbols (pushnew (find-market symbol :bitmex) (cdr dung)))))))

;;;
;;; LION TAMER
;;;

(defun annualize-current-day (gate active-symbol &aux (condom 5) (solar 365.25))
    (awhen (gate-request gate '(:get "user/walletHistory")
                         `(("count" . ,condom) ("reverse" . t)))
      (assert (= (length it) condom))
      (with-json-slots ((balance "walletBalance")
                        (symbol "address")
                        (profit "amount"))
          (find "RealisedPNL" it :test #'string= :key (getjso "transactType"))
        (assert (string= symbol active-symbol))
        (aprog1 (expt (1+ (/ profit balance)) solar)
          (format t "~&~5,2@F%~%" it)))))

(defconstant -when-in-the-
  "\"AF\" Afghanistan (AF)
\"AX\" Eh[ngstrm]land Islands (AX)
\"AL\" Albania (AL)
\"DZ\" Algeria (DZ)
\"AS\" American Samoa (AS)
\"AD\" Andorra (AD)
\"AO\" Angola (AO)
\"AI\" Anguilla (AI)
\"AQ\" Antarctica (AQ)
\"AG\" Antigua and Barbuda (AG)
\"AR\" Argentina (AR)
\"AM\" Armenia (AM)
\"AW\" Aruba (AW)
\"AU\" Australia (AU)
\"AT\" Austria (AT)
\"AZ\" Azerbaijan (AZ)
\"BS\" Bahamas (BS)
\"BH\" Bahrain (BH)
\"BD\" Bangladesh (BD)
\"BB\" Barbados (BB)
\"BY\" Belarus (BY)
\"BE\" Belgium (BE)
\"BZ\" Belize (BZ)
\"BJ\" Benin (BJ)
\"BM\" Bermuda (BM)
\"BT\" Bhutan (BT)
\"BO\" Bolivia (BO)
\"BQ\" Bonaire, Sint Eustatius and Saba (BQ)
\"BA\" Bosnia and Herzegovina (BA)
\"BW\" Botswana (BW)
\"BV\" Bouvet Island (BV)
\"BR\" Brazil (BR)
\"IO\" British Indian Ocean Territory (IO)
\"BN\" Brunei Darussalam (BN)
\"BG\" Bulgaria (BG)
\"BF\" Burkina Faso (BF)
\"BI\" Burundi (BI)
\"KH\" Cambodia (KH)
\"CM\" Cameroon (CM)
\"CA\" Canada (CA)
\"CV\" Cape Verde (CV)
\"KY\" Cayman Islands (KY)
\"CF\" Central African Republic (CF)
\"TD\" Chad (TD)
\"CL\" Chile (CL)
\"CN\" China (CN)
\"CX\" Christmas Island (CX)
\"CC\" Cocos (Keeling) Islands (CC)
\"CO\" Colombia (CO)
\"KM\" Comoros (KM)
\"CG\" Congo (CG)
\"CD\" Congo, the Democratic Republic of the (CD)
\"CK\" Cook Islands (CK)
\"CR\" Costa Rica (CR)
\"CI\" Cote D'Ivoire (CI)
\"HR\" Croatia (HR)
\"CU\" Cuba (CU)
\"CW\" Curasao (CW)
\"CY\" Cyprus (CY)
\"CZ\" Czech Republic (CZ)
\"DK\" Denmark (DK)
\"DJ\" Djibouti (DJ)
\"DM\" Dominica (DM)
\"DO\" Dominican Republic (DO)
\"EC\" Ecuador (EC)
\"EG\" Egypt (EG)
\"SV\" El Salvador (SV)
\"GQ\" Equatorial Guinea (GQ)
\"ER\" Eritrea (ER)
\"EE\" Estonia (EE)
\"SZ\" Eswatini (SZ)
\"ET\" Ethiopia (ET)
\"FK\" Falkland Islands (Malvinas) (FK)
\"FO\" Faroe Islands (FO)
\"FJ\" Fiji (FJ)
\"FI\" Finland (FI)
\"FR\" France (FR)
\"GF\" French Guiana (GF)
\"PF\" French Polynesia (PF)
\"TF\" French Southern Territories (TF)
\"GA\" Gabon (GA)
\"GM\" Gambia (GM)
\"GE\" Georgia (GE)
\"DE\" Germany (DE)
\"GH\" Ghana (GH)
\"GI\" Gibraltar (GI)
\"GR\" Greece (GR)
\"GL\" Greenland (GL)
\"GD\" Grenada (GD)
\"GP\" Guadeloupe (GP)
\"GU\" Guam (GU)
\"GT\" Guatemala (GT)
\"GG\" Guernsey (GG)
\"GN\" Guinea (GN)
\"GW\" Guinea-Bissau (GW)
\"GY\" Guyana (GY)
\"HT\" Haiti (HT)
\"HM\" Heard Island and Mcdonald Islands (HM)
\"VA\" Holy See (Vatican City State) (VA)
\"HN\" Honduras (HN)
\"HK\" Hong Kong (HK)
\"HU\" Hungary (HU)
\"IS\" Iceland (IS)
\"IN\" India (IN)
\"ID\" Indonesia (ID)
\"IR\" Iran, Islamic Republic of (IR)
\"IQ\" Iraq (IQ)
\"IE\" Ireland (IE)
\"IM\" Isle of Man (IM)
\"IL\" Israel (IL)
\"IT\" Italy (IT)
\"JM\" Jamaica (JM)
\"JP\" Japan (JP)
\"JE\" Jersey (JE)
\"JO\" Jordan (JO)
\"KZ\" Kazakhstan (KZ)
\"KE\" Kenya (KE)
\"KI\" Kiribati (KI)
\"XK\" Kosovo (XK)
\"KW\" Kuwait (KW)
\"KG\" Kyrgyzstan (KG)
\"LA\" Lao People's Democratic Republic (LA)
\"LV\" Latvia (LV)
\"LB\" Lebanon (LB)
\"LS\" Lesotho (LS)
\"LR\" Liberia (LR)
\"LY\" Libya (LY)
\"LI\" Liechtenstein (LI)
\"LT\" Lithuania (LT)
\"LU\" Luxembourg (LU)
\"MO\" Macao (MO)
\"MG\" Madagascar (MG)
\"MW\" Malawi (MW)
\"MY\" Malaysia (MY)
\"MV\" Maldives (MV)
\"ML\" Mali (ML)
\"MT\" Malta (MT)
\"MH\" Marshall Islands (MH)
\"MQ\" Martinique (MQ)
\"MR\" Mauritania (MR)
\"MU\" Mauritius (MU)
\"YT\" Mayotte (YT)
\"MX\" Mexico (MX)
\"FM\" Micronesia, Federated States of (FM)
\"MD\" Moldova, Republic of (MD)
\"MC\" Monaco (MC)
\"MN\" Mongolia (MN)
\"ME\" Montenegro (ME)
\"MS\" Montserrat (MS)
\"MA\" Morocco (MA)
\"MZ\" Mozambique (MZ)
\"MM\" Myanmar (MM)
\"NA\" Namibia (NA)
\"NR\" Nauru (NR)
\"NP\" Nepal (NP)
\"NL\" Netherlands (NL)
\"NC\" New Caledonia (NC)
\"NZ\" New Zealand (NZ)
\"NI\" Nicaragua (NI)
\"NE\" Niger (NE)
\"NG\" Nigeria (NG)
\"NU\" Niue (NU)
\"NF\" Norfolk Island (NF)
\"KP\" North Korea (KP)
\"MK\" North Macedonia, Republic of (MK)
\"MP\" Northern Mariana Islands (MP)
\"NO\" Norway (NO)
\"OM\" Oman (OM)
\"PK\" Pakistan (PK)
\"PW\" Palau (PW)
\"PS\" Palestinian Territory, Occupied (PS)
\"PA\" Panama (PA)
\"PG\" Papua New Guinea (PG)
\"PY\" Paraguay (PY)
\"PE\" Peru (PE)
\"PH\" Philippines (PH)
\"PN\" Pitcairn (PN)
\"PL\" Poland (PL)
\"PT\" Portugal (PT)
\"PR\" Puerto Rico (PR)
\"QA\" Qatar (QA)
\"RE\" Reunion (RE)
\"RO\" Romania (RO)
\"RU\" Russian Federation (RU)
\"RW\" Rwanda (RW)
\"BL\" Saint Bart`elemy (BL)
\"SH\" Saint Helena (SH)
\"KN\" Saint Kitts and Nevis (KN)
\"LC\" Saint Lucia (LC)
\"MF\" Saint Martin (French part) (MF)
\"PM\" Saint Pierre and Miquelon (PM)
\"VC\" Saint Vincent and the Grenadines (VC)
\"WS\" Samoa (WS)
\"SM\" San Marino (SM)
\"ST\" Sao Tome and Principe (ST)
\"SA\" Saudi Arabia (SA)
\"SN\" Senegal (SN)
\"RS\" Serbia (RS)
\"SC\" Seychelles (SC)
\"SL\" Sierra Leone (SL)
\"SG\" Singapore (SG)
\"SX\" Sint Maarten (Dutch part) (SX)
\"SK\" Slovakia (SK)
\"SI\" Slovenia (SI)
\"SB\" Solomon Islands (SB)
\"SO\" Somalia (SO)
\"ZA\" South Africa (ZA)
\"GS\" South Georgia and the South Sandwich Islands (GS)
\"KR\" South Korea (KR)
\"SS\" South Sudan (SS)
\"ES\" Spain (ES)
\"LK\" Sri Lanka (LK)
\"SD\" Sudan (SD)
\"SR\" Suriname (SR)
\"SJ\" Svalbard and Jan Mayen (SJ)
\"SE\" Sweden (SE)
\"CH\" Switzerland (CH)
\"SY\" Syrian Arab Republic (SY)
\"TW\" Taiwan (TW)
\"TJ\" Tajikistan (TJ)
\"TZ\" Tanzania, United Republic of (TZ)
\"TH\" Thailand (TH)
\"TL\" Timor-Leste (TL)
\"TG\" Togo (TG)
\"TK\" Tokelau (TK)
\"TO\" Tonga (TO)
\"TT\" Trinidad and Tobago (TT)
\"TN\" Tunisia (TN)
\"TR\" Turkey (TR)
\"TM\" Turkmenistan (TM)
\"TC\" Turks and Caicos Islands (TC)
\"TV\" Tuvalu (TV)
\"UG\" Uganda (UG)
\"UA\" Ukraine (UA)
\"AE\" United Arab Emirates (AE)
\"GB\" United Kingdom (GB)
\"UM\" United States Minor Outlying Islands (UM)
\"US\" United States of America (US)
\"UY\" Uruguay (UY)
\"UZ\" Uzbekistan (UZ)
\"VU\" Vanuatu (VU)
\"VE\" Venezuela (VE)
\"VN\" Vietnam (VN)
\"VG\" Virgin Islands, British (VG)
\"VI\" Virgin Islands, U.S. (VI)
\"WF\" Wallis and Futuna (WF)
\"EH\" Western Sahara (EH)
\"YE\" Yemen (YE)
\"ZM\" Zambia (ZM)
\"ZW\" Zimbabwe (ZW)
"
  "... course of unconventional warfare,
my true love said to me:"
  ;; just recompile, and make a new constant!
  )
