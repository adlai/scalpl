;;;; exchange.lisp

(defpackage #:scalpl.exchange
  (:use #:cl #:chanl #:anaphora #:local-time #:scalpl.util #:scalpl.actor)
  (:export #:exchange #:name #:assets #:markets #:parse-timestamp
           #:*exchanges* #:find-exchange #:fetch-exchange-data
           #:asset #:find-asset #:asset-quantity
           #:quantity #:scaled-quantity #:cons-aq #:cons-aq* #:aq+ #:aq-
           #:market #:decimals #:primary #:counter #:find-market
           #:offer #:bid #:ask #:placed #:market-order
           #:taken #:given
           #:volume #:price #:placed #:oid #:consumed-asset
           #:gate #:gate-post #:gate-request
           #:thread #:control #:updater #:worker #:output ; coming soon: actors!
           #:trade #:cost #:direction #:txid
           #:trades-tracker #:trades #:trades-since #:vwap
           #:book-tracker #:bids #:asks #:get-book
           #:tracked-market
           #:placed-offers #:account-balances #:market-fee
           #:execution #:fee #:net-cost #:net-volume
           #:execution-tracker #:execution-since #:bases
           #:post-offer #:cancel-offer))

(in-package #:scalpl.exchange)

;;; TODO
;;; This file should lay out the interface that each exchange client needs to
;;; implement. Each exchange client should instantiate the exchange class and
;;; specialize methods on generic functions, which should be defined here.

;;; TODO
;;; Some method for implemented exchange APIs to "register" the exchange, so
;;; we don't have to mess with a different package per exchange.

;;;
;;; Exchanges
;;;

(defvar *exchanges* (make-hash-table))

(defun find-exchange (name) (gethash name *exchanges*))

(defclass exchange ()
  ((name    :initarg :name    :reader name)
   (assets  :initarg :assets  :reader assets)
   (markets :initarg :markets :reader markets)
   ;; FIXME: broken af
   (market-timestamp-sensitivity :initarg :sensitivity)))

(defmethod initialize-instance :after ((exchange exchange) &key)
  (with-slots (name) exchange
    (setf (gethash name *exchanges*) exchange)))

(defgeneric fetch-exchange-data (exchange)
  (:documentation "Fetch metadata about assets and markets on this exchange.
Since fetching this information requires network access, it's performed on a
need-to-use basis, rather than upon initial loading of the exchange API.")
  (:method :after (exchange)
    (with-slots (assets markets) exchange
      (dolist (thing (append assets markets))
        (setf (slot-value thing 'exchange) exchange)))))

(defmethod slot-unbound ((class t) (exchange exchange) slot)
  (if (member slot '(assets markets))
      (progn (fetch-exchange-data exchange)
             (slot-value exchange slot))
      (call-next-method)))

(defmethod print-object ((exchange exchange) stream)
  (print-unreadable-object (exchange stream :type nil :identity nil)
    (princ (name exchange) stream)))

;;; FIXME these are de facto exchange dispatch hacked onto timestamp wtf
(defgeneric parse-timestamp (exchange timestamp)
  (:method ((exchange exchange) (sec integer)) (unix-to-timestamp sec))
  (:method ((exchange exchange) (timestamp real))
    (multiple-value-bind (sec nsec) (floor timestamp)
      (unix-to-timestamp sec :nsec (round (* (expt 10 9) nsec)))))
  (:method ((exchange exchange) (timestamp string))
    ;; asdfjkshanueodaoseitcgaeosundantheoudvwe;ercgu
    (parse-timestamp exchange (parse-float timestamp :type 'double-float))))

;;;
;;; Assets
;;;

(defvar *asset-id-counter* 0)
(defvar *asset-registry* (make-hash-table))

(defclass asset ()
  ((name     :initarg :name     :reader name)
   (decimals :initarg :decimals :reader decimals)
   (exchange :initarg :exchange :reader exchange)
   (index    :initform (incf *asset-id-counter*))))

(defmethod print-object ((asset asset) stream)
  (print-unreadable-object (asset stream :type nil :identity nil)
    (format stream "~A on ~A" (name asset) (exchange asset))))

(defgeneric find-asset (designator exchange)
  (:method (designator (assets list))
    (find designator assets :key 'name :test 'string-equal))
  (:method (designator (exchange exchange))
    (find-market designator (assets exchange)))
  (:method (designator (registry hash-table))
    (gethash designator registry)))

(defmethod initialize-instance :after ((asset asset) &key)
  (setf (gethash (slot-value asset 'index) *asset-registry*) asset))

;;;
;;; Asset Quantities
;;;

(deftype asset-quantity ()
  "A precise quantity of a given asset"
  '(complex unsigned-byte))

(defun asset (aq) (find-asset (imagpart aq) *asset-registry*))
(defun quantity (aq) (realpart aq))
(defun scaled-quantity (aq) (/ (realpart aq) (expt 10 (decimals (asset aq)))))
(defun cons-aq (asset quantity)
  (complex (round quantity) (slot-value asset 'index)))
(defun cons-aq* (asset quantity)
  (cons-aq asset (* quantity (expt 10 (decimals asset)))))

;;; might not be useful, but let's have it around
(defun aq+ (aq1 aq2 &rest aqs)
  (assert (eq (asset aq1) (asset aq2)))
  (if aqs (reduce #'aq+ aqs :initial-value (aq+ aq1 aq2))
      (cons-aq (asset aq1) (+ (quantity aq1) (quantity aq2)))))

;;; ditto... but let's not go any further, yet
(defun aq- (aq1 &optional aq2 &rest aqs)
  (cond (aqs (aq- aq1 (reduce #'aq+ aqs :initial-value aq2)))
        (aq2 (assert (eq (asset aq1) (asset aq2)))
             (cons-aq (asset aq1) (- (quantity aq1) (quantity aq2))))
        (t   (cons-aq (asset aq1) (- (quantity aq1))))))

;;;
;;; Markets
;;;

(defclass market ()
  ((name     :initarg :name     :reader name)
   (decimals :initarg :decimals :reader decimals)
   (exchange :initarg :exchange :reader exchange)
   (counter  :initarg :counter  :reader counter)
   (primary  :initarg :primary  :reader primary)))

(defmethod print-object ((market market) stream)
  (print-unreadable-object (market stream :type nil :identity nil)
    (format stream "~A on ~A" (name market) (exchange market))))

(defgeneric find-market (designator exchange)
  (:method (designator (markets list))
    (find designator markets :key 'name :test 'string-equal))
  (:method (designator (exchange exchange))
    (with-slots (markets) exchange
      (find-market designator markets))))

;;;
;;; Offers
;;;

;;; FIXME: slots 'give' and 'take' containing amount & asset, each

(defclass offer ()
  ((market :initarg :market :reader market)
   (taken  :initarg :taken  :reader taken)
   (given  :initarg :given  :reader given)
   (volume :initarg :volume :accessor volume)
   (price  :initarg :price  :reader price)))

(defclass placed (offer)
  ((oid    :initarg :oid    :reader oid)))

(defclass bid (offer) ()) (defclass ask (offer) ())

(defmethod initialize-instance ((bid bid) &rest keys &key price)
  (apply #'call-next-method bid :price (- price) keys))

(defmethod shared-initialize :after ((offer offer) names &key)
  (with-slots (market taken given volume price) offer
    ((lambda (primary counter)
       (when (or (eq names t) (find 'given names))
         (setf given (ecase (signum price) (+1 primary) (-1 counter))))
       (when (or (eq names t) (find 'taken names))
         (setf taken (ecase (signum price) (+1 counter) (-1 primary)))))
     (cons-aq* (primary market) volume)
     (with-slots (counter decimals) market
       (cons-aq* counter (* volume (/ (abs price) (expt 10 decimals))))))))

(defmethod print-object ((offer offer) stream)
  (print-unreadable-object (offer stream :type t)
    (with-slots (volume price market) offer
      (with-slots (name decimals) (consumed-asset offer)
        (let ((market-decimals (slot-value market 'decimals)))
          (format stream "~V$ ~A @ ~V$" decimals volume name market-decimals
                  (/ (abs price) (expt 10 market-decimals))))))))

(defclass market-order (offer) ())
(defmethod initialize-instance ((order market-order) &rest keys &key consume)
  (apply #'call-next-method order
         :price (ecase consume (primary 1) (counter -1))
         keys))

(defmethod print-object ((order market-order) stream)
  (print-unreadable-object (order stream :type t)
    (with-slots (volume market) order
      (with-slots (name decimals) (consumed-asset order)
        (format stream "~V$ ~A" decimals volume name)))))

(defgeneric consumed-asset (offer)
  (:method ((bid bid)) (counter (market bid)))
  (:method ((ask ask)) (primary (market ask)))
  (:method ((offer offer))
    (with-slots (market price) offer
      (slot-value market (if (> price 0) 'primary 'counter)))))

;; (defmethod update-instance-for-different-class :after ((offer offer) (placed placed) &key)
;;   (format t "~&@~A ~A" (format-timestring nil (now) :format '((:sec 2))) placed))

;;;
;;; Rate Gate
;;;

(defclass gate (actor)
  ((pubkey :initarg :pubkey :initform (error "gate requires API pubkey"))
   (secret :initarg :secret :initform (error "gate requires API secret"))
   (input  :initarg :input  :initform (make-instance 'channel))
   (output :initarg :output :initform (make-instance 'channel))
   (cache  :initform nil))
  (:default-initargs :name "gate"))

(defgeneric gate-post (gate pubkey secret request)
  (:documentation "Attempts to perform `request' with the provided credentials.")
  #+nil                                 ; for automatic multiple value wrapping
  (:method :around (gate pubkey secret request)
    (multiple-value-list (call-next-method))))

(defmethod perform ((gate gate))
  (with-slots (input output . #1=(pubkey secret cache)) gate
    (when (send-blocks-p output) (setf cache (recv input)))
    (send (slot-value gate 'output) (gate-post gate . #1#))))

(defun gate-request (gate path &optional options)
  (with-slots (input output) gate
    (send input (cons path options))
    (values-list (recv output))))

;;;
;;; Public Data API
;;;

;;; should trade direction be represented as:
;;; a boolean slot?
;;; slots for (consumed|earned)×(volume|asset)?
;;; distinct buy and sell mixin classes/objects?

(defclass trade ()
  ((market    :initarg :market    :reader market)
   (volume    :initarg :volume    :reader volume)
   (price     :initarg :price     :reader price) ; aq?
   (cost      :initarg :cost      :reader cost)
   (taken     :initarg :taken     :reader taken)
   (given     :initarg :given     :reader given)
   (txid      :initarg :txid      :reader txid)
   (timestamp :initarg :timestamp :reader timestamp)
   (direction :initarg :direction :reader direction)))

(defmethod shared-initialize :after ((trade trade) names &key)
  (with-slots (market taken given volume price direction) trade
    ((lambda (primary counter)
       (let ((butterfingerp (char-equal (char direction 0) #\s)))
         (when (or (eq names t) (find 'given names))
           (setf given (if butterfingerp primary counter)))
         (when (or (eq names t) (find 'taken names))
           (setf taken (if butterfingerp counter primary)))))
     (cons-aq* (primary market) volume)
     (with-slots (counter decimals) market
       (cons-aq* counter (* volume (/ (abs price) (expt 10 decimals))))))))

(defmethod print-object ((trade trade) stream)
  (print-unreadable-object (trade stream :identity nil)
    (with-slots (market volume price timestamp direction) trade
      (with-slots (primary decimals) market
        (format stream "~A ~A at ~V$: ~V$" timestamp
                direction decimals price (decimals primary) volume)))))

(defgeneric trades-since (market &optional since))

(defclass trades-tracker ()
  ((market  :initarg :market :reader market)
   (delay   :initarg :delay :initform 27)
   (control :initform (make-instance 'channel))
   (buffer  :initform (make-instance 'channel))
   (output  :initform (make-instance 'channel))
   (trades  :initform nil)
   updater worker))

(defgeneric vwap (tracker &key type depth &allow-other-keys)
  (:method :around ((tracker t) &key)
    (handler-case (call-next-method)
      (division-by-zero () 0)))
  (:method ((tracker trades-tracker) &key since depth type)
    (let ((trades (slot-value tracker 'trades)))
      (when since (setf trades (remove since trades
                                       :key #'timestamp :test #'timestamp>=)))
      (when type
        (setf trades (remove (ccase type (:buy #\b) (:sell #\s)) trades
                             :key (lambda (trade) (char (direction trade) 0))
                             :test-not #'char-equal)))
      (when depth
        (setf trades (loop for trade in trades collect trade
                           sum (volume trade) into sum
                           until (>= sum depth))))
      (handler-case
          (/ (reduce #'+ (mapcar #'cost trades))
             (reduce #'+ (mapcar #'volume trades)))
        (division-by-zero () 0)))))

;;; look at us, thinking of the future already
;;; we also dispatch on trades-tracker so that exchange-specific methods can
;;; dispatch on just the tracker argument, rather than both prev and next
;;; face it, sometimes you just gotta (eval '(expt +evil+ 1/2))

(defgeneric trades-mergeable? (trades-tracker prev next)
  (:method-combination and)
  (:method and ((tracker t) (prev null) (next t)))
  (:method and ((tracker trades-tracker) (prev trade) (next trade))
    (with-slots (market-timestamp-sensitivity)
        (slot-reduce tracker market exchange) ; !
      (and (string= (direction prev) (direction next))
           (> market-timestamp-sensitivity
              (timestamp-difference (timestamp next) (timestamp prev)))))))

;; (defgeneric same-trades? (trades-tracker prev next)
;;   (:method-combination and)
;;   (:method and ()))

(defgeneric merge-trades (trades-tracker prev next)
  (:method ((tracker trades-tracker) (prev trade) (next trade))
    (let* ((volume (+ (volume prev) (volume next)))
           (cost   (+ (cost   prev) (cost   next)))
           (price (/ cost volume)))
      (make-instance 'trade :market (market prev)
                     :txid (ignore-errors (txid next)) ;hack
                     :timestamp (timestamp next) :cost cost
                     :volume volume :price price
                     :direction (direction prev)))))

(defmethod timestamp ((object null)))   ; such lazy

(defun trades-worker-loop (tracker)
  (with-slots (control buffer output trades market) tracker
    (let ((last (car trades)))
      (select
        ((recv control command)
         ;; commands are (cons command args)
         (case (car command)
           ;; should this really be a 'command' and not just an operation
           ;; calculated by another actor on (slot-value tracker 'trades)?
           ;; max - find max seen trade size
           (max (send output (reduce #'max (mapcar #'volume trades)
                                     :initial-value 0)))
           ;; set symbol value
           (set (setf (symbol-value (second command)) (third command)))
           ;; get symbol value
           (get (send (third command) (symbol-value (second command))))
           ;; pause - wait for any other command to restart
           (pause (recv control))))
        ((send buffer last))
        ((recv buffer next)
         (if (trades-mergeable? tracker last next)
             (setf (car trades) (merge-trades tracker last next))
             (push next trades)))
        (t (sleep 0.2))))))

(defun trades-updater-loop (tracker)
  (with-slots (market buffer delay) tracker
    (dolist (trade (trades-since market (recv buffer))) (send buffer trade))
    (sleep delay)))

(defmethod shared-initialize :after ((tracker trades-tracker) (slots t) &key)
  (with-slots (updater worker market) tracker
    (when (or (not (slot-boundp tracker 'updater))
              (eq :terminated (task-status updater)))
      (setf updater
            (pexec
                (:name (concatenate 'string "qdm-preα trades updater for " (name market))
                 :initial-bindings `((*read-default-float-format* double-float)))
              (loop (trades-updater-loop tracker)))))
    (when (or (not (slot-boundp tracker 'worker))
              (eq :terminated (task-status worker)))
      (setf worker
            (pexec (:name (concatenate 'string "qdm-preα trades worker for " (name market))
                          :initial-bindings `((*read-default-float-format* double-float)))
              ;; TODO: just pexec anew each time...
              ;; you'll understand what you meant someday, right?
              (loop (trades-worker-loop tracker)))))))

;;;
;;; ORDER BOOK
;;;

(defclass book-tracker ()
  ((market :initarg :market :reader market)
   (control :initform (make-instance 'channel))
   (output :initform (make-instance 'channel))
   (delay :initarg :delay :initform 8)
   bids asks updater worker))

(defun book-worker-loop (tracker)
  (with-slots (control bids asks output) tracker
    (handler-case
        (select
          ((recv control command)
           ;; commands are (cons command args)
           (case (car command)
             ;; pause - wait for any other command to restart
             (pause (recv control))))
          ((send output (cons bids asks)))
          (t (sleep 0.2)))
      (unbound-slot ()))))

(defgeneric get-book (market &key))

(defun book-updater-loop (tracker)
  (with-slots (bids asks delay market offers) tracker
    (setf (values asks bids) (get-book market))
    (sleep delay)))

(defmethod shared-initialize :after ((tracker book-tracker) (names t) &key)
  (with-slots (updater worker market) tracker
    (when (or (not (slot-boundp tracker 'updater))
              (eq :terminated (task-status updater)))
      (setf updater
            (pexec
                (:name (concatenate 'string "qdm-preα book updater for " (name market))
                       :initial-bindings `((*read-default-float-format* double-float)))
              (loop (book-updater-loop tracker)))))
    (when (or (not (slot-boundp tracker 'worker))
              (eq :terminated (task-status worker)))
      (setf worker
            (pexec (:name (concatenate 'string "qdm-preα book worker for " (name market)))
              ;; TODO: just pexec anew each time...
              ;; you'll understand what you meant someday, right?
              (loop (book-worker-loop tracker)))))))

;;;
;;; Putting things together
;;;

(defclass tracked-market (market)
  ((%market :initarg :market :initform (error "must specify market"))
   book-tracker trades-tracker))

(defmethod update-instance-for-different-class :before
    ((prev market) (new tracked-market) &key)
  (setf (slot-value new '%market) (shallow-copy prev)))

(defmethod shared-initialize :after ((market tracked-market) (names t) &key)
  (with-slots (%market book-tracker trades-tracker book trades) market
    (if (slot-boundp market 'book-tracker)
        (reinitialize-instance book-tracker)
        (setf book-tracker (make-instance 'book-tracker :market %market)))
    (if (slot-boundp market 'trades-tracker)
        (reinitialize-instance trades-tracker)
        (setf trades-tracker (make-instance 'trades-tracker :market %market)))))

;;;
;;; Private Data API
;;;

(defgeneric placed-offers (gate))
(defgeneric account-balances (gate))

(defgeneric market-fee (gate market)
  (:method :around ((gate gate) (market market))
    (actypecase (call-next-method)
      (number `(,it . ,it)) (cons it) (null))))

;;;
;;; EXECUTION TRACKING
;;;

(defclass execution (trade)
  ((oid :initarg :oid :reader oid)
   (fee :initarg :fee :reader fee)
   (net-cost :initarg :net-cost :reader net-cost)
   (net-volume :initarg :net-volume :reader net-volume)))

(defmethod shared-initialize :after ((execution execution) names &key)
  (with-slots (market taken given net-volume net-cost direction) execution
    ((lambda (primary counter)
       (let ((butterfingerp (char-equal (char direction 0) #\s)))
         (when (or (eq names t) (find 'given names))
           (setf given (if butterfingerp primary counter)))
         (when (or (eq names t) (find 'taken names))
           (setf taken (if butterfingerp counter primary)))))
     (cons-aq* (primary market) net-volume)
     (cons-aq* (counter market) net-cost))))

(defclass execution-tracker ()
  ((gate :initarg :gate)
   ;; TODO: is this the right model for a general execution tracking API?
   ;; bitfinex requires tracking distinct markets, kraken doesn't
   ;; keep your eyes open, for now we'll specify a single market per tracker
   (market :initarg :market :reader market)
   (delay :initform 30 :initarg :delay)
   (trades :initform nil) (bases :initform nil)
   (control :initform (make-instance 'channel))
   (buffer :initform (make-instance 'channel))
   worker updater))

(defgeneric execution-since (gate market since))

(defun execution-worker-loop (tracker)
  (with-slots (control buffer trades bases) tracker
    (let ((last (car trades)))
      (select
        ((recv control command)
         ;; commands are (cons command args)
         (case (car command)
           ;; pause - wait for any other command to restart
           (pause (recv control))
           ;; set symbol value
           (set (setf (symbol-value (second command)) (third command)))
           ;; get symbol value
           (get (send (third command) (symbol-value (second command))))))
        ((send buffer last))
        ((recv buffer next)
         (with-slots (taken given price market) next
           (swhen (getf bases (asset given))
             (loop for basis = (pop it) while basis
                for acc = (cdr basis) then (aq+ acc (cdr basis))
                until (> (quantity acc) (quantity given))
                finally (when basis (push (cons (car basis) (aq- acc given)) it))))
           (push (cons (round (* price (expt 10 (decimals market)))) taken)
                 (getf bases (asset taken))))
         (pushnew next trades :key #'txid :test #'equal))
        (t (sleep 0.2))))))

;;; this function should return two values:
;;; first and foremost - the vwab price
;;; it can get consed & pushed onto the basis list outside, by the worker
;;; second - the remaining basis for the consumed asset
;;; which also gets inserted into the bases set by the worker
;;; when there's no basis to be consumed, or insufficient, 2nd value nil
;;; does EXACTLY what we need!

;;; who calls this function?
;;; execution-worker, to compute the remaining basis for the given asset
;;; ope-spreader (TODO), to check whether a certain offer is profitable

;;; what should it be fed?
;;; bases - does it need both given and taken, or just given?
;;; p×aq - this looks suspiciously familiar...............

(defun vwab (lictor asset depth)
  (loop for basis in (getf (slot-value lictor 'bases) asset)
     sum (quantity (cdr basis)) into acc until (> acc depth)))

(defun execution-updater-loop (tracker)
  (with-slots (gate market buffer delay) tracker
    (dolist (trade (execution-since gate market (recv buffer)))
      (send buffer trade))
    (sleep delay)))

(defmethod shared-initialize :after ((tracker execution-tracker) (slots t) &key)
  (with-slots (updater worker market) tracker
    (when (or (not (slot-boundp tracker 'updater))
              (eq :terminated (task-status updater)))
      (setf updater
            (pexec
                (:name (concatenate 'string "qdm-preα execution updater for " (name market))
                 :initial-bindings `((*read-default-float-format* double-float)))
              (loop (execution-updater-loop tracker)))))
    (when (or (not (slot-boundp tracker 'worker))
              (eq :terminated (task-status worker)))
      (setf worker
            (pexec (:name (concatenate 'string "qdm-preα execution worker for " (name market))
                          :initial-bindings `((*read-default-float-format* double-float)))
              ;; TODO: just pexec anew each time...
              ;; you'll understand what you meant someday, right?
              (loop (execution-worker-loop tracker)))))))

;;; TODO: We have the fees paid for each order in the data from the exchange,
;;; so we should be able to calculate the _net_ price for each trade, and use
;;; that for profitability calculations, rather than fee at time of calculation.

(defmethod vwap ((tracker execution-tracker) &key type depth net)
  (let ((trades (slot-value tracker 'trades)))
    (when type
      (setf trades (remove type (slot-value tracker 'trades)
                           :key #'direction :test #'string-not-equal)))
    (when depth
      (setf trades (loop for trade in trades collect trade
                      sum (volume trade) into sum
                      until (>= sum depth))))
    (/ (reduce '+ (mapcar (if net #'net-cost #'cost) trades))
       (reduce '+ (mapcar (if net #'net-volume #'volume) trades)))))

;;;
;;; Action API
;;;

(defgeneric post-offer (gate offer))
(defgeneric cancel-offer (gate offer))
