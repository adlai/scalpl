;;;; qd.lisp - quick and dirty. kraken's api... wow

(defpackage #:scalpl.qd
  (:use #:cl #:anaphora #:st-json #:local-time #:scalpl.util #:scalpl.connection))

(in-package #:scalpl.qd)

;;;
;;; Actor
;;;

;;; TODO: Incorporate weak references and finalizers into the whole CSPSM model
;;; so they get garbage collected when there are no more references to the
;;; output channels

;;; actor afterthought - maybe actors should be explicit, channels just an
;;; implementation detail? "API methods" on the actor object serve as the
;;; client, and state machine on the channel side serves as the server

(defclass actor ()
  ((thread :documentation "Thread performing this actor's behavior")
   (control :documentation "Channel for controlling this actor")
   (default :documentation "Default command in absence of control messages"
            :initarg :default :initform (error "Must supply default state"))
   (children :allocation :class :initform nil
             :documentation "Children of this actor")
   (channels :allocation :class :initform nil
             :documentation "Channel slot names")))

(defmethod initialize-instance :before ((actor actor) &key)
  (flet ((initialize (slot)
           (setf (slot-value actor slot)
                 (make-instance 'chanl:channel))))
    (initialize 'control)
    (mapc #'initialize (slot-value actor 'channels))))

(defgeneric perform (actor command)
  (:method ((actor actor) (command function))
    (funcall command actor)))

(defgeneric christen (actor)
  (:documentation "Generates a name for `actor', after slot initialization")
  (:method ((actor actor)) (format nil "actor for ~A" actor)))

(defun kill-actor (actor)
  ;; todo: timeout for chanl:kill ?
  (chanl:send (slot-value actor 'control) nil))

(defmethod reinitialize-instance :before ((actor actor) &key kill)
  (when kill (kill-actor actor)))

(defmethod shared-initialize :after ((actor actor) slot-names &key)
  (when (or (not (slot-boundp actor 'thread))
            (eq :terminated (chanl:task-status (slot-value actor 'thread))))
    (setf (slot-value actor 'thread)
          (chanl:pexec (:name (christen actor)
                        :initial-bindings `((*read-default-float-format* double-float)))
            (perform actor (chanl:recv (slot-value actor 'control)))))))

;;;
;;; Rate Gate
;;;

(defclass gate ()
  (key signer thread
   (in :initform (make-instance 'chanl:channel))))

(defun gate-loop (gate)
  (with-slots (key signer in) gate
    ;;                   / command structure \
    (destructuring-bind (command method &rest options) (chanl:recv in)
      (typecase command
        (symbol (setf (slot-value gate command) (apply method options)))
        (chanl:channel
         (chanl:send command
                     (multiple-value-list (post-request method key signer options))))))))

(defmethod initialize-instance :before ((gate gate) &key key secret)
  (setf (slot-value gate 'key) (make-key key)
        (slot-value gate 'signer) (make-signer secret)))

(defmethod shared-initialize :after ((gate gate) names &key)
  (when (or (not (slot-boundp gate 'thread))
            (eq :terminated (chanl:task-status (slot-value gate 'thread))))
    (setf (slot-value gate 'thread)
          (chanl:pexec (:name "qdm-preα gate"
                        :initial-bindings `((*read-default-float-format* double-float)))
            (loop (gate-loop gate))))))

(defun gate-request (gate path &optional options)
  (let ((out (make-instance 'chanl:channel)))
    (chanl:send (slot-value gate 'in)
                (list* out path options))
    (values-list (chanl:recv out))))

(defclass asset ()
  ((name :initarg :name :reader name-of)
   (decimals :initarg :decimals)))

(defun get-assets ()
  (mapcar-jso (lambda (name data)
                (with-json-slots (altname decimals) data
                  (make-instance 'asset :name name :decimals decimals)))
              (get-request "Assets")))

(defun find-asset (designator &optional (assets *assets*))
  (find designator assets :key 'name-of :test 'string-equal))

(defparameter *assets* (get-assets))

(defclass market ()
  ((name :initarg :name :reader name-of)
   (decimals :initarg :decimals)
   (quote :initarg :quote)
   (base :initarg :base)))

(defun get-markets ()
  (aprog1 (make-hash-table :test #'equalp)
    (mapjso (lambda (name data)
              (with-json-slots (altname pair_decimals quote base) data
                (let ((market (make-market :name name :base base :quote quote
                                           :decimals pair_decimals)))
                  (setf (gethash name it) market
                        (gethash altname it) market))))
            (get-request "AssetPairs"))))

(defun find-market (designator &optional (markets *markets*))
  (gethash designator markets))

(defvar *markets* (get-markets))

(defun open-orders (gate)
  (mapjso* (lambda (id order) (setf (getjso "id" order) id))
           (getjso "open" (gate-request gate "OpenOrders"))))

(defun cancel-order (gate oid)
  (gate-request gate "CancelOrder" `(("txid" . ,oid))))

(defun cancel-pair-orders (gate pair)
  (mapjso (lambda (id order)
            (declare (ignore id))
            (when (string= pair (getjso* "descr.pair" order))
              (cancel-order gate order)))
          (open-orders gate)))

(defparameter *validate* nil)

(define-condition volume-too-low () ())

(defun post-limit (gate type pair price volume decimals &optional options)
  (let ((price (/ price (expt 10d0 decimals))))
    (multiple-value-bind (info errors)
        (gate-request gate "AddOrder"
                      `(("ordertype" . "limit")
                        ("type" . ,type)
                        ("pair" . ,pair)
                        ("volume" . ,(format nil "~F" volume))
                        ("price" . ,(format nil "~F" price))
                        ,@(when options `(("oflags" . ,options)))
                        ,@(when *validate* `(("validate" . "true")))
                        ))
      (if errors
          (dolist (message errors)
            (if (search "volume" message)
                (if (search "viqc" options)
                    (return
                      ;; such hard code
                      (post-limit gate type pair price (+ volume 0.01) 0 options))
                    ;; (signal 'volume-too-low)
                    (return
                      (post-limit gate type pair price (* volume price) 0
                                  (apply #'concatenate 'string "viqc"
                                         (when options '("," options))))))
                (format t "~&~A~%" message)))
          (progn
            ;; theoretically, we could get several order IDs here,
            ;; but we're not using any of kraken's fancy forex nonsense
            (setf (getjso* "descr.id" info) (car (getjso* "txid" info)))
            (getjso "descr" info))))))

;;;
;;; Offers
;;;

(defclass offer ()
  ((pair :initarg :pair)
   (volume :initarg :volume :accessor offer-volume)
   (price :initarg :price :reader offer-price)))

(defclass placed (offer)
  ((id :initarg :id :reader offer-id)
   (text :initarg :text :reader offer-text)))

(defclass bid (offer) ())
(defclass ask (offer) ())
(defmethod initialize-instance :after ((bid bid) &key)
  (with-slots (price) bid (setf price (- price))))

(defun post-offer (gate offer)
  (with-slots (pair volume price) offer
    (flet ((post (type options)
             (awhen (post-limit gate type pair (abs price) volume
                                (slot-value (find-market pair) 'decimals)
                                options)
               (with-json-slots (id order) it
                 (change-class offer 'placed :id id :text order)))))
      (if (< price 0)
          (post "buy" "viqc")
          (post "sell" nil)))))

(defun cancel-offer (gate offer)
  (multiple-value-bind (ret err) (cancel-order gate (offer-id offer))
    (or ret (search "Unknown order" (car err)))))

;;; TODO: deal with partially completed orders
(defun ignore-offers (open mine &aux them)
  (dolist (offer open (nreverse them))
    (aif (find (offer-price offer) mine :test #'= :key #'offer-price)
         (let ((without-me (- (offer-volume offer) (offer-volume it))))
           (setf mine (remove it mine))
           (unless (< without-me 0.001)
             (push (make-instance 'offer :pair (slot-value offer 'pair)
                                  :price (offer-price offer)
                                  :volume without-me)
                   them)))
         (push offer them))))

(defun placed-offers (gate)
  (mapcar-jso (lambda (id data)
                (with-json-slots (descr vol oflags) data
                  (with-json-slots (pair type price order) descr
                    (let* ((decimals (slot-value (find-market pair) 'decimals))
                           (price-int (parse-price price decimals))
                           (volume (read-from-string vol)))
                      (make-instance 'placed
                                     :id id :text order :pair pair
                                     :price (if (string= type "buy") (- price-int) price-int)
                                     :volume (if (not (search "viqc" oflags)) volume
                                                 (/ volume price-int (expt 10 decimals))))))))
              (open-orders gate)))

;;;
;;; TRADES
;;;

(defclass trades-tracker ()
  ((pair :initarg :pair)
   (control :initform (make-instance 'chanl:channel))
   (buffer :initform (make-instance 'chanl:channel))
   (output :initform (make-instance 'chanl:channel))
   (delay :initarg :delay :initform 27)
   (trades :initform nil)
   last updater worker))

(defun kraken-timestamp (timestamp)
  (multiple-value-bind (sec rem) (floor timestamp)
    (local-time:unix-to-timestamp sec :nsec (round (* (expt 10 9) rem)))))

(defun trades-since (pair &optional since)
  (with-json-slots (last (trades pair))
      (get-request "Trades" `(("pair" . ,pair)
                              ,@(when since `(("since" . ,since)))))
    (values (mapcar (lambda (trade)
                      (destructuring-bind (price volume time side kind data) trade
                        (let ((price  (read-from-string price))
                              (volume (read-from-string volume)))
                          (list (kraken-timestamp time)
                                ;; FIXME - "cost" later gets treated as precise
                                volume price (* volume price)
                                (concatenate 'string side kind data)))))
                    trades)
            last)))

(defgeneric vwap (tracker &key since type depth &allow-other-keys)
  (:method ((tracker trades-tracker) &key since depth type)
    (let ((trades (slot-value tracker 'trades)))
      (when since
        (setf trades (remove since trades :key #'car :test #'timestamp>=)))
      (when type
        (setf trades (remove (ccase type (buy #\b) (sell #\s)) trades
                             :key (lambda (trade) (char (fifth trade) 0))
                             :test-not #'char=)))
      (when depth
        (setf trades (loop for trade in trades collect trade
                           sum (second trade) into sum
                           until (>= sum depth))))
      (/ (reduce #'+ (mapcar #'fourth trades))
         (reduce #'+ (mapcar #'second trades))))))

(defun trades-worker-loop (tracker)
  (with-slots (control buffer output trades) tracker
    (chanl:select
      ((chanl:recv control command)
       ;; commands are (cons command args)
       (case (car command)
         ;; max - find max seen trade size
         (max (chanl:send output (reduce #'max (mapcar #'second trades))))
         ;; vwap - find vwap over recent trades
         (vwap (chanl:send output (apply #'vwap tracker (cdr command))))
         ;; pause - wait for any other command to restart
         (pause (chanl:recv control))))
      ((chanl:recv buffer raw-trades)
       (unless trades (push (pop raw-trades) trades))
       (setf trades
             (reduce (lambda (acc next &aux (prev (first acc)))
                       (if (and (> 0.3
                                   (local-time:timestamp-difference (first next)
                                                                    (first prev)))
                                (string= (fifth prev) (fifth next)))
                           (let* ((volume (+ (second prev) (second next)))
                                  (cost (+ (fourth prev) (fourth next)))
                                  (price (/ cost volume)))
                             (cons (list (first prev)
                                         volume price cost
                                         (fifth prev))
                                   (cdr acc)))
                           (cons next acc)))
                     raw-trades :initial-value trades)))
      (t (sleep 0.2)))))

(defun trades-updater-loop (tracker)
  (with-slots (pair buffer delay last) tracker
    (multiple-value-bind (raw-trades until)
        (handler-case (trades-since pair last)
          (unbound-slot () (trades-since pair)))
      (setf last until)
      (chanl:send buffer raw-trades)
      (sleep delay))))

(defmethod shared-initialize :after ((tracker trades-tracker) (slots t) &key)
  (with-slots (pair updater worker) tracker
    (when (or (not (slot-boundp tracker 'updater))
              (eq :terminated (chanl:task-status updater)))
      (setf updater
            (chanl:pexec
                (:name (concatenate 'string "qdm-preα trades updater for " pair)
                       :initial-bindings `((*read-default-float-format* double-float)))
              (loop (trades-updater-loop tracker)))))
    (when (or (not (slot-boundp tracker 'worker))
              (eq :terminated (chanl:task-status worker)))
      (setf worker
            (chanl:pexec (:name (concatenate 'string "qdm-preα trades worker for " pair))
              ;; TODO: just pexec anew each time...
              ;; you'll understand what you meant someday, right?
              (loop (trades-worker-loop tracker)))))))

;;;
;;; ORDER BOOK
;;;

(defclass book-tracker ()
  ((pair :initarg :pair)
   (control :initform (make-instance 'chanl:channel))
   (book-output :initform (make-instance 'chanl:channel))
   (delay :initarg :delay :initform 8)
   bids asks updater worker))

(defun book-worker-loop (tracker)
  (with-slots (control bids asks book-output) tracker
    (handler-case
        (chanl:select
          ((chanl:recv control command)
           ;; commands are (cons command args)
           (case (car command)
             ;; pause - wait for any other command to restart
             (pause (chanl:recv control))))
          ((chanl:send book-output (cons bids asks)))
          (t (sleep 0.2)))
      (unbound-slot ()))))

;;; TODO: verify the number of decimals!
(defun parse-price (price-string decimals)
  (let ((dot (position #\. price-string)))
    (parse-integer (remove #\. price-string)
                   :end (+ dot decimals))))

(defun get-book (pair)
  (let ((decimals (slot-value (find-market pair) 'decimals)))
    (with-json-slots (bids asks)
        (getjso pair (get-request "Depth" `(("pair" . ,pair))))
      (flet ((parser (class)
               (lambda (raw-order)
                 (destructuring-bind (price amount timestamp) raw-order
                   (declare (ignore timestamp))
                   (make-instance class :pair pair
                                  :price (parse-price price decimals)
                                  :volume (read-from-string amount))))))
        (values (mapcar (parser 'ask) asks)
                (mapcar (parser 'bid) bids))))))

(defun book-updater-loop (tracker)
  (with-slots (bids asks delay pair offers) tracker
    (setf (values asks bids) (get-book pair))
    (sleep delay)))

(defmethod shared-initialize :after ((tracker book-tracker) (names t) &key)
  (with-slots (updater worker pair) tracker
    (when (or (not (slot-boundp tracker 'updater))
              (eq :terminated (chanl:task-status updater)))
      (setf updater
            (chanl:pexec
                (:name (concatenate 'string "qdm-preα book updater for " pair)
                       :initial-bindings `((*read-default-float-format* double-float)))
              (loop (book-updater-loop tracker)))))
    (when (or (not (slot-boundp tracker 'worker))
              (eq :terminated (chanl:task-status worker)))
      (setf worker
            (chanl:pexec (:name (concatenate 'string "qdm-preα book worker for " pair))
              ;; TODO: just pexec anew each time...
              ;; you'll understand what you meant someday, right?
              (loop (book-worker-loop tracker)))))))

;;;
;;; EXECUTION TRACKING
;;;

(defclass execution-tracker ()
  ((gate :initarg :gate)
   (delay :initform 30)
   (trades :initform nil)
   (control :initform (make-instance 'chanl:channel))
   (buffer :initform (make-instance 'chanl:channel))
   (since :initform (timestamp- (now) 6 :hour) :initarg :since)
   worker updater))

(defun raw-trades-history (tracker &key since until ofs)
  (macrolet ((check-bound (bound)
               `(setf ,bound
                      (ctypecase ,bound
                        (null nil)      ; (typep nil nil) -> nil
                        (string ,bound)
                        (timestamp
                         (princ-to-string (timestamp-to-unix ,bound)))
                        (jso (getjso "txid" ,bound))))))
    (check-bound since)
    (check-bound until))
  (gate-request (slot-value tracker 'gate) "TradesHistory"
                (append (when since `(("start" . ,since)))
                        (when until `(("end" . ,until)))
                        (when ofs `(("ofs" . ,ofs))))))

;;; TODO: We have the fees paid for each order in the data from the exchange,
;;; so we should be able to calculate the _net_ price for each trade, and use
;;; that for profitability calculations, rather than fee at time of calculation.
(defun trades-history-chunk (tracker &key until since)
  (with-slots (delay) tracker
    (with-json-slots (count trades)
        (apply #'raw-trades-history tracker
               (append (when until `(:until ,until))
                       (when since `(:since ,since))))
      (let* ((total (parse-integer count))
             (chunk (make-array (list total) :fill-pointer 0)))
        (flet ((process (trades-jso)
                 (mapjso (lambda (tid data)
                           (map nil (lambda (key)
                                      (setf (getjso key data)
                                            (read-from-string (getjso key data))))
                                '("price" "cost" "fee" "vol"))
                           (with-json-slots (txid time) data
                             (setf txid tid time (kraken-timestamp time)))
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
             (with-json-slots (count trades)
                 (apply #'raw-trades-history tracker
                        :until until :ofs (princ-to-string (fill-pointer chunk))
                        (when since `(:since ,since)))
               (let ((next-total (parse-integer count)))
                 (assert (= total next-total))
                 (process trades)))))))))

(defun execution-worker-loop (tracker)
  (with-slots (trades control buffer) tracker
    ;; this is an example of where you need STATE MACHINES
    (if (< (length trades) 50)          ; FIXME WOW
        (push (chanl:recv buffer) trades)
        (chanl:select
          ((chanl:recv control channel) (chanl:send channel trades))
          ((chanl:recv buffer trade) (push trade trades))
          (t (sleep 0.2))))))

(defun execution-updater-loop (tracker)
  (with-slots (since gate buffer) tracker
    (loop
       for trade across (trades-history-chunk tracker :since since)
       do (chanl:send buffer trade)
       finally (when trade (setf since trade)))))

(defmethod shared-initialize :after ((tracker execution-tracker) slots &key)
  (with-slots (delay worker updater) tracker
    (when (or (not (slot-boundp tracker 'worker))
              (eq :terminated (chanl:task-status worker)))
      (setf worker
            (chanl:pexec (:name "qdm-preα execution worker")
              (loop (execution-worker-loop tracker)))))
    (when (or (not (slot-boundp tracker 'updater))
              (eq :terminated (chanl:task-status updater)))
      (setf updater
            (chanl:pexec (:name "qdm-preα execution updater"
                          :initial-bindings '((*read-default-float-format* double-float)))
              (loop
                 (sleep delay)
                 (execution-updater-loop tracker)))))))

;;;
;;;  ENGINE
;;;

(defclass ope-supplicant ()
  ((gate :initarg :gate)
   (placed :initform nil :initarg :placed)
   (control :initarg :control)
   (response :initarg :response)
   (balance-tracker :initarg :balance-tracker)
   thread))

;;; receives messages in the control channel, outputs from the gate
(defun ope-supplicant-loop (ope)
  (with-slots (gate control response placed balance-tracker) ope
    (let ((command (chanl:recv control)))
      (destructuring-bind (car . cdr) command
        (chanl:send response
                    (case car
                      (placed placed)
                      (filter (ignore-offers cdr placed))
                      (offer (when (with-slots (pair price volume) cdr
                                     (>= (asset-balance balance-tracker
                                                        (slot-value (find-market pair)
                                                                    (if (< price 0) 'quote 'base)))
                                         (reduce #'+
                                                 (mapcar #'offer-volume
                                                         (remove (- (signum price)) placed
                                                                 :key (lambda (offer)
                                                                        (signum (offer-price offer)))))
                                                 :initial-value volume)))
                               (awhen1 (post-offer gate cdr) (push it placed))))
                      (cancel (awhen1 (cancel-offer gate cdr)
                                (setf placed (remove cdr placed))))))))))

(defmethod shared-initialize :after ((supplicant ope-supplicant) slots &key)
  (with-slots (thread) supplicant
    (when (or (not (slot-boundp supplicant 'thread))
              (eq :terminated (chanl:task-status thread)))
      (setf thread
            (chanl:pexec (:name "qdm-preα ope supplicant")
              (loop (ope-supplicant-loop supplicant)))))))

(defclass ope ()
  ((input :initform (make-instance 'chanl:channel))
   (output :initform (make-instance 'chanl:channel))
   (next-bids :initform (make-instance 'chanl:channel))
   (next-asks :initform (make-instance 'chanl:channel))
   (prioritizer-response :initform (make-instance 'chanl:channel))
   (control :initform (make-instance 'chanl:channel))
   (response :initform (make-instance 'chanl:channel))
   (book-channel :initarg :book-channel)
   supplicant prioritizer scalper))

(defun ope-placed (ope)
  (with-slots (control response) ope
    (chanl:send control '(placed))
    (let ((all (sort (copy-list (chanl:recv response)) #'< :key #'offer-price)))
      (flet ((split (sign)
               (remove sign all :key (lambda (x) (signum (offer-price x))))))
        ;;       bids       asks
        (values (split 1) (split -1))))))

;;; response: placed offer if successful, nil if not
(defun ope-place (ope offer)
  (with-slots (control response) ope
    (chanl:send control (cons 'offer offer))
    (chanl:recv response)))

;;; response: {count: "1"} if successful, nil if not
(defun ope-cancel (ope offer)
  (with-slots (control response) ope
    (chanl:send control (cons 'cancel offer))
    (chanl:recv response)))

(defun ope-filter (ope book)
  (with-slots (control response) ope
    (chanl:send control (cons 'filter book))
    (chanl:recv response)))

;;; receives target bids and asks in the next-bids and next-asks channels
;;; sends commands in the control channel through #'ope-place
;;; sends completion acknowledgement to prioritizer-response channel
(defun ope-prioritizer-loop (ope)
  (with-slots (next-bids next-asks prioritizer-response) ope
    (flet ((place (new) (ope-place ope new)))
      (flet ((update (target placed &aux percents cutoff)
               (fresh-line)
               (format-timestring t (now) :format '((:hour 2) #\: (:min 2) #\: (:sec 2)))
               (dolist (old placed (setf cutoff (third (sort percents #'>))))
                 (awhen (find (offer-price old) target :key #'offer-price :test #'=)
                   (push (/ (abs (- (offer-volume it) (offer-volume old)))
                            (offer-volume old))
                         percents)))
               (dolist (old placed (mapcar #'place target))
                 (format-timestring t (now) :format '(".." (:sec 2)))
                 (force-output)
                 (aif (aand1 (find (offer-price old) target
                                   :key #'offer-price :test #'=)
                             (< (/ (abs (- (offer-volume it) (offer-volume old)))
                                   (offer-volume old))
                                (or cutoff 0)))
                      (setf target (remove it target))
                      (dolist (new (remove (offer-price old) target
                                           :key #'offer-price :test #'<)
                               (ope-cancel ope old))
                        (if (place new) (setf target (remove new target))
                            (return (ope-cancel ope old))))))
               (chanl:send prioritizer-response t)))
        (chanl:select
          ((chanl:recv next-bids to-bid) (update to-bid (nth-value 0 (ope-placed ope))))
          ((chanl:recv next-asks to-ask) (update to-ask (nth-value 1 (ope-placed ope))))
          (otherwise (sleep 0.2)))))))

(defun profit-margin (bid ask fee-percent)
  (* (/ ask bid) (- 1 (/ fee-percent 100))))

;;; Lossy trades

;;; The most common lossy trade execution happens when a limit order rolls
;;; through one or more offers but isn't filled, and thus remains on the
;;; books. If this order is large enough, it'll get outbid by the next round of
;;; the offer placement algorithm, and the outbidding offer will be lossy
;;; relative to the trades previously executed.

;;; How bad is this?

;;; In some situations, the remaining limit order gets traded back rapidly:
;; TUFCXE 12:26:33 buy  €473.22001 0.00020828 €0.09856
;; TJVT4U 12:26:33 buy  €473.22002 0.00004196 €0.01986
;; TRU2YT 12:24:05 buy  €474.04001 0.00108394 €0.51383
;; TOFJR2 12:23:52 sell €474.04000 0.00002623 €0.01243
;; TYWDQU 12:23:51 sell €473.98799 0.00074902 €0.35503
;; TINWGD 12:23:51 sell €473.95313 0.00028740 €0.13621
;; TPY7P4 12:23:51 sell €473.92213 0.00003772 €0.01788

(defun dumbot-offers (book resilience funds max-orders &aux (acc 0) (share 0))
  (do* ((cur book (cdr cur))
        (n 0 (1+ n)))
       ((or (and (> acc resilience) (> n (isqrt max-orders))) (null cur))
        (let* ((sorted (sort (subseq book 1 n) #'> :key (lambda (x) (offer-volume (cdr x)))))
               (n-orders (min max-orders n))
               (relevant (cons (car book) (subseq sorted 0 (1- n-orders))))
               (total-shares (reduce #'+ (mapcar #'car relevant))))
          (mapcar (lambda (order)
                    (with-slots (pair price) (cdr order)
                      (make-instance 'offer :pair pair :price (1- price)
                                     :volume (* funds (/ (car order) total-shares)))))
                  (sort relevant #'< :key (lambda (x) (offer-price (cdr x)))))))
    ;; TODO - no side effects
    ;; TODO - use a callback for liquidity distribution control
    (with-slots (volume) (car cur)
      (push (incf share (* 4/3 (incf acc volume))) (car cur)))))

(defun ope-scalper-loop (ope)
  (with-slots (input output book-channel next-bids next-asks prioritizer-response) ope
    (destructuring-bind (fee base quote resilience) (chanl:recv input)
      ;; Now run that algorithm thingy
      (flet ((filter-book (book) (ope-filter ope book))
             (place (new) (ope-place ope new)))
        ;; The entire with-book operation needs to be turned into a separate
        ;; program entity ("actor"?) which receives updated order books, and
        ;; currently-placed offersets, and produces filtered books
        ;; Whether filtered books are pushed or pulled is TBD
        (macrolet ((with-book (() &body body)
                     `(destructuring-bind (market-bids . market-asks)
                          (chanl:recv book-channel)
                        (let ((other-bids (filter-book market-bids))
                              (other-asks (filter-book market-asks)))
                          ;; NON STOP PARTY PROFIT MADNESS
                          (do* ((best-bid (- (offer-price (car other-bids)))
                                          (- (offer-price (car other-bids))))
                                (best-ask (offer-price (car other-asks))
                                          (offer-price (car other-asks)))
                                (spread (profit-margin (1+ best-bid) (1- best-ask) fee)
                                        (profit-margin (1+ best-bid) (1- best-ask) fee)))
                               ((> spread 1))
                            (ecase (round (signum (* (max 0 (- best-ask best-bid 10))
                                                     (- (offer-volume (car other-bids))
                                                        (offer-volume (car other-asks))))))
                              (-1 (decf (offer-volume (car other-asks))
                                        (offer-volume (pop other-bids))))
                              (+1 (decf (offer-volume (car other-bids))
                                        (offer-volume (pop other-asks))))
                              (0 (pop other-bids) (pop other-asks))))
                          ,@body))))
          ;; Need to rework this flow so the worker (actor calculating priorities) gets
          ;; the entire book at once...
          ;; TODO: properly deal with partial and completed orders
          (with-book ()
            (chanl:send next-bids (dumbot-offers other-bids resilience quote 15))
            (chanl:recv prioritizer-response))
          (with-book ()
            (chanl:send next-asks (dumbot-offers other-asks resilience base 15))
            (chanl:recv prioritizer-response)))))
    (chanl:send output nil)))

(defmethod shared-initialize :after ((ope ope) slots &key gate balance-tracker)
  (with-slots (supplicant prioritizer scalper control response) ope
    (unless (slot-boundp ope 'supplicant)
      (setf supplicant (make-instance 'ope-supplicant :gate gate
                                      :placed (placed-offers gate)
                                      :control control :response response
                                      :balance-tracker balance-tracker)))
    (when (or (not (slot-boundp ope 'prioritizer))
              (eq :terminated (chanl:task-status prioritizer)))
      (setf prioritizer
            (chanl:pexec (:name "qdm-preα ope prioritizer")
              (loop (ope-prioritizer-loop ope)))))
    (when (or (not (slot-boundp ope 'scalper))
              (eq :terminated (chanl:task-status scalper)))
      (setf scalper
            (chanl:pexec (:name "qdm-preα ope scalper"
                          :initial-bindings `((*read-default-float-format* double-float)))
              (loop (ope-scalper-loop ope)))))))

;;;
;;; ACCOUNT TRACKING
;;;

(defclass account-tracker ()
  ((balances :initarg :balances :initform nil)
   (control :initform (make-instance 'chanl:channel))
   (gate :initarg :gate)
   (delay :initform 15)
   (lictor :initarg :lictor)
   (ope :initarg :ope)
   updater worker))

(defun account-worker-loop (tracker)
  (with-slots (balances control) tracker
    (let ((command (chanl:recv control)))
      (destructuring-bind (car . cdr) command
        (typecase car
          ;; ( asset . channel )  <- send asset balance to channel
          (string
           (chanl:send cdr (or (cdr (assoc car balances :test #'string=)) 0)))
          ;; ( slot . value ) <- update slot with new value
          (symbol (setf (slot-value tracker car) cdr)))))))

(defun account-updater-loop (tracker)
  (with-slots (gate control delay) tracker
    (chanl:send control
                (cons 'balances
                      (mapcar-jso (lambda (asset balance)
                                    (cons asset (read-from-string balance)))
                                  (gate-request gate "Balance"))))
    (sleep delay)))

(defmethod vwap ((tracker account-tracker) &key type pair depth)
  (let ((c (make-instance 'chanl:channel)))
    (chanl:send (slot-value (slot-value tracker 'lictor) 'control) c)
    (let ((trades (remove type (chanl:recv c)
                          :key (lambda (c) (getjso "type" c)) :test #'string/=)))
      (when pair
        (setf trades (remove pair trades
                             :key (lambda (c) (getjso "pair" c)) :test #'string/=)))
      (when depth
        (setf trades (loop for trade in trades collect trade
                           sum (getjso "vol" trade) into sum
                           until (>= sum depth))))
      (/ (reduce '+ (mapcar (lambda (x) (getjso "cost" x)) trades))
         (reduce '+ (mapcar (lambda (x) (getjso "vol" x)) trades))))))

(defmethod shared-initialize :after ((tracker account-tracker) (names t) &key)
  (with-slots (updater worker lictor gate ope) tracker
    (unless (slot-boundp tracker 'lictor)
      (setf lictor (make-instance 'execution-tracker :gate gate))
      ;; if this tracker has no trades, we can't calculate vwap
      ;; crappy solution... ideally w/condition system?
      (sleep 3))
    (unless (slot-boundp tracker 'ope)
      (setf ope (make-instance 'ope :gate gate :balance-tracker tracker)))
    (when (or (not (slot-boundp tracker 'updater))
              (eq :terminated (chanl:task-status updater)))
      (setf updater
            (chanl:pexec (:name "qdm-preα account updater"
                          :initial-bindings `((*read-default-float-format* double-float)))
              (loop (account-updater-loop tracker)))))
    (when (or (not (slot-boundp tracker 'worker))
              (eq :terminated (chanl:task-status worker)))
      (setf worker
            (chanl:pexec (:name "qdm-preα account worker")
              ;; TODO: just pexec anew each time...
              ;; you'll understand what you meant someday, right?
              (loop (account-worker-loop tracker)))))))

(defun asset-balance (tracker asset &aux (channel (make-instance 'chanl:channel)))
  (with-slots (control) tracker
    (chanl:send control (cons asset channel))
    (chanl:recv channel)))

(defun gapps-rate (from to)
  (getjso "rate" (read-json (drakma:http-request
                             "http://rate-exchange.appspot.com/currency"
                             :parameters `(("from" . ,from) ("to" . ,to))
                             :want-stream t))))

(defclass fee-tracker ()
  ((pair :initarg :pair :initform "XXBTZEUR")
   (gate :initarg :gate)
   (delay :initform 67)
   fee thread))

(defun pair-fee (gate pair)
  (awhen (gate-request gate "TradeVolume" `(("pair" . ,pair)))
    (read-from-string (getjso "fee" (getjso pair (getjso "fees" it))))))

(defun fee-tracker-loop (tracker)
  (with-slots (pair gate delay fee) tracker
    (setf fee (pair-fee gate pair))
    (sleep delay)))

(defmethod shared-initialize :after ((tracker fee-tracker) names &key)
  (with-slots (thread pair gate fee) tracker
    (setf fee (pair-fee gate pair))
    (when (or (not (slot-boundp tracker 'thread))
              (eq :terminated (chanl:task-status thread)))
      (setf thread
            (chanl:pexec
                (:name (concatenate 'string "qdm-preα fee tracker for " pair)
                 :initial-bindings `((*read-default-float-format* double-float)))
              ;; TODO: just pexec anew each time...
              ;; you'll understand what you meant someday, right?
              (loop (fee-tracker-loop tracker)))))))

(defclass maker ()
  ((pair :initarg :pair :initform "XXBTZEUR")
   (fund-factor :initarg :fund-factor :initform 1)
   (resilience-factor :initarg :resilience :initform 1)
   (targeting-factor :initarg :targeting :initform 3/5)
   (control :initform (make-instance 'chanl:channel))
   (fee-tracker :initarg :fee-tracker)
   (trades-tracker :initarg :trades-tracker)
   (book-tracker :initarg :book-tracker)
   (account-tracker :initarg :account-tracker)
   thread))

(defun %round (maker)
  (declare (optimize (debug 3)))
  (with-slots (fund-factor resilience-factor targeting-factor pair
               fee-tracker trades-tracker book-tracker account-tracker)
      maker
    ;; whoo!
    (chanl:send (slot-value trades-tracker 'control) '(max))
    ;; Get our balances
    (let (;; TODO: split into base resilience and quote resilience
          (resilience (* resilience-factor
                         (chanl:recv (slot-value trades-tracker 'output))))
          ;; TODO: doge is cute but let's move on
          (doge/btc (with-slots (control output) trades-tracker
                      (chanl:send control `(vwap :since ,(timestamp- (now) 4 :hour)))
                      (chanl:recv output))))
      (flet ((symbol-funds (symbol) (asset-balance account-tracker symbol))
             (total-of (btc doge) (+ btc (/ doge doge/btc)))
             (factor-fund (fund factor) (* fund fund-factor factor)))
        (let* ((market (find-market pair))
               (fee (slot-value fee-tracker 'fee))
               (total-btc (symbol-funds (slot-value market 'base)))
               (total-doge (symbol-funds (slot-value market 'quote)))
               (total-fund (total-of total-btc total-doge))
               (investment (/ total-btc total-fund))
               (btc (factor-fund total-btc (* investment targeting-factor)))
               (doge (factor-fund total-doge (- 1 (* investment targeting-factor)))))
          ;; report funding
          ;; FIXME: modularize all this decimal point handling
          (flet ((asset-decimals (kind)
                   (slot-value (find-asset (slot-value market kind)) 'decimals))
                 (depth-profit (depth)
                   (* 100 (1- (profit-margin (vwap account-tracker :type "buy"
                                                   :pair pair :depth depth)
                                             (vwap account-tracker :type "sell"
                                                   :pair pair :depth depth)
                                             fee)))))
            ;; time, total, base, quote, invested, risked, risk bias, pulse
            (format t "~&~A ~V$ ~V$ ~V$ ~$% ~$% ~@$ ~6@$ ~6@$ ~6@$ ~6@$~%"
                    (format-timestring nil (now)
                                       :format '((:hour 2) #\:
                                                 (:min 2) #\:
                                                 (:sec 2)))
                    (asset-decimals 'base)  total-fund
                    (asset-decimals 'base)  total-btc
                    (asset-decimals 'quote) total-doge
                    (* 100 investment)
                    (* 100 (/ (total-of btc doge) total-fund))
                    (* 100 (/ (total-of (- btc) doge) total-fund))
                    (depth-profit 17.15)
                    (depth-profit 2.45)
                    (depth-profit 0.35)
                    (depth-profit 0.05)))
          (force-output)
          (with-slots (ope) account-tracker
            (chanl:send (slot-value ope 'input) (list fee btc doge resilience))
            (when (< (* investment (1+ (- investment))) 1/18)
              (macrolet ((urgent (class side)
                           `(make-instance ',class :pair pair :volume (* total-fund 1/8)
                                           :price (1- (abs (slot-value (cadr (slot-value book-tracker ',side)) 'price))))))
                ;; theoretically, this could exceed available volume, but
                ;; that's highly unlikely with a fund-factor below ~3/2
                (describe (ope-place ope (if (> investment 1/2) (urgent ask asks) (urgent bid bids))))))
            (chanl:recv (slot-value ope 'output))))))))

(defun dumbot-loop (maker)
  (with-slots (control) maker
    (chanl:select
      ((chanl:recv control command)
       ;; commands are (cons command args)
       (case (car command)
         ;; pause - wait for any other command to restart
         (pause (chanl:recv control))
         (stream (setf *standard-output* (cdr command)))))
      (t (%round maker)))))

(defmethod shared-initialize :after ((maker maker) (names t) &key gate)
  (with-slots (pair fee-tracker trades-tracker book-tracker account-tracker thread) maker
    ;; FIXME: wtf is this i don't even
    (unless (slot-boundp maker 'trades-tracker)
      (setf trades-tracker (make-instance 'trades-tracker :pair pair))
      (sleep 12))
    (unless (slot-boundp maker 'book-tracker)
      (setf book-tracker (make-instance 'book-tracker :pair pair))
      (sleep 12))
    (unless (slot-boundp maker 'account-tracker)
      (setf account-tracker (make-instance 'account-tracker :gate gate))
      (sleep 12))
    ;; FIXME: ...
    (unless (slot-boundp maker 'fee-tracker)
      (setf fee-tracker (make-instance 'fee-tracker :pair pair
                                       :gate (slot-value account-tracker 'gate))))
    ;; stitchy!
    (setf (slot-value (slot-value account-tracker 'ope) 'book-channel)
          (slot-value book-tracker 'book-output))
    (when (or (not (slot-boundp maker 'thread))
              (eq :terminated (chanl:task-status thread)))
      (setf thread
            (chanl:pexec
                (:name (concatenate 'string "qdm-preα " pair)
                 :initial-bindings `((*read-default-float-format* double-float)))
              ;; TODO: just pexec anew each time...
              ;; you'll understand what you meant someday, right?
              (loop (dumbot-loop maker)))))))

(defun pause-maker (maker)
  (with-slots (control) maker
    (chanl:send control '(pause))))

(defun reset-the-net (maker &optional (revive t))
  (flet ((ensure-death (list)
           (let ((thread (reduce #'slot-value list :initial-value maker)))
             (tagbody
                (if (eq :terminated (chanl:task-status thread)) (go end)
                    (chanl:kill (chanl:task-thread thread)))
              loop
                (if (eq :terminated (chanl:task-status thread)) (go end) (go loop))
              end))))
    (mapc #'ensure-death
          `((thread)
            (account-tracker gate thread)
            (account-tracker ope scalper)
            (account-tracker ope prioritizer)
            (account-tracker ope supplicant thread)
            (account-tracker ope scalper)
            (account-tracker worker)
            (account-tracker updater)
            (account-tracker lictor worker)
            (account-tracker lictor updater)
            (trades-tracker updater)
            (trades-tracker worker)
            (book-tracker updater)
            (book-tracker worker)
            (fee-tracker thread))))
  (when revive
    (mapc 'reinitialize-instance
          (list (slot-value maker 'book-tracker)
                (slot-value maker 'account-tracker)
                (slot-value maker 'trades-tracker)
                (slot-value (slot-value maker 'account-tracker) 'gate)
                (slot-value (slot-value maker 'account-tracker) 'lictor)
                (slot-value (slot-value maker 'account-tracker) 'ope)
                (slot-value (slot-value (slot-value maker 'account-tracker) 'ope) 'supplicant)
                (slot-value maker 'fee-tracker)
                maker))))

(defvar *maker*
  (make-instance 'maker
                 :gate (make-instance 'gate
                                      :key #P "secrets/kraken.pubkey"
                                      :secret #P "secrets/kraken.secret")))
