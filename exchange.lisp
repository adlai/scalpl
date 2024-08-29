(defpackage #:scalpl.exchange
  (:use #:cl #:chanl #:anaphora #:local-time
	#:scalpl.util #:scalpl.actor #:scalpl.net)
  (:export #:enable-pretty-printer-abuse
           #:exchange #:name #:assets #:markets #:parse-timestamp
           #:*exchanges* #:find-exchange #:fetch-exchange-data
           #:gate #:gate-post #:gate-request #:output #:input #:cache

           #:asset #:find-asset #:asset-quantity
           #:quantity #:scaled-quantity #:cons-aq #:cons-aq* #:aq+ #:aq-
           #:market #:tick #:decimals #:primary #:counter #:find-market
           #:scaled-price #:cons-mp #:cons-mp* #:scalp #:aq/ #:aq*

           #:offer #:bid #:ask #:offered #:taken #:given
           #:volume #:price #:placed #:oid #:consumed-asset
           #:gate #:gate-post #:gate-request #:output #:input #:cache
           #:trade #:cost #:direction #:txid #:tracked-market

           #:agent-trunk

           #:trades-tracker #:trades #:trades-since #:vwap #:book
           #:book-tracker #:bids #:asks #:get-book #:get-book-keys
           #:balance-tracker #:balances #:sync #:print-book #:asset-funds

           #:placed-offers #:account-balances #:market-fee
           #:execution #:fee #:net-cost #:net-volume #:fee-tracker
           #:execution-tracker #:execution-since #:bases #:bases-without
           #:post-offer #:cancel-offer #:supplicant #:lictor #:treasurer
           #:order-slots #:response #:supplicate #:bases-for #:reserved
           #:squash-reservations #:sufficiently-different?

           #:describe-account #:respawn-syncer))

(in-package #:scalpl.exchange)

;;; TODO
;;; This file MUST build the interface that each exchange client needs to
;;; implement. Each client file then instantiates an exchange class, and
;;; specializes methods on generic functions that SHOULD be defined here.

;;;
;;; Exchanges
;;;

;;; TODO
;;; Consider whether overloading the exchange registry into the one that
;;; already exists for the units and markets; for extra overachievement,
;;; hook into the package mechanism for isolating each exchange's code.
;;; The compile- and load-time magic must, in such a situation, occur in
;;; a (:method initialize-instance :after (exchange)) near the beginning
;;; of each exchange's file.

(defvar *exchanges* (make-hash-table))

(defun find-exchange (name &aux (str (format () "scalpl.~(~A~)" name)))
  (symbol-macrolet ((get (gethash name *exchanges*)))
    (or get (awhen (asdf:find-system str) (asdf:load-system it) get))))

(defclass exchange ()
  ((name    :initarg :name    :reader name)
   ;; (stream  :initarg :stream  :reader stream)
   (assets  :initarg :assets  :reader assets)
   (markets :initarg :markets :reader markets)
   ;; FIXME: broken af; only necessary for exchanges that
   ;; have trouble publishing unambiguous ordering during
   ;; interesting times in noisy markets
   (market-timestamp-sensitivity :initarg :sensitivity)))

(defmethod initialize-instance :after ((exchange exchange) &key)
  (with-slots (name) exchange
    (setf (gethash name *exchanges*) exchange)))

(defgeneric fetch-exchange-data (exchange)
  (:documentation "fetch info about an exchange's assets and markets")
  ;; Since these methods require network access, they are ideally only
  ;; invoked at load-time, rather than upon initial compilation.
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
;;; Unit Registry
;;;

;;; https://en.wikipedia.org/wiki/Scalar_(physics)#Physical_quantity

(defvar *unit-registry* '(()))          ; Mathematicians HATE him!

(defclass registered-unit ()
  ((index :initform (length *unit-registry*) :reader index)))

(defmethod initialize-instance :after ((unit registered-unit) &key)
  (push (cons (index unit) unit) *unit-registry*))

(defun find-unit (index) (cdr (assoc index *unit-registry*)))

(defun physical-quantity-p (thing)      ; Find out why, from that
  (and (complexp thing) (find-unit (imagpart thing)))) ; ... ...

;;; (upgraded-complex-part-type
;;;  `(unsigned-byte ,(integer-length most-positive-fixnum)))
;;; NO CODE USING 'CL:SATISFIES IS EVER SATISFACTORY!
(deftype physical-quantity () '(satisfies physical-quantity-p))

(defun pprint-physical-quantity (stream pq) ; one ! deft ! prick.
  (with-slots (decimals name) (find-unit (imagpart pq))
    (format stream "~A ~A" (decimals:format-decimal-number
                            (/ (realpart pq) (expt 10 decimals))
                            :round-magnitude (- decimals)
                            :show-trailing-zeros t)
            name)))                     ; PRETTYNESS IS STRENGTH

(defvar *physical-quantity-pprint-dispatch* ; CARS EAT PEOPLE
  (aprog1 (copy-pprint-dispatch ())     ; and 1985 actually is a howto manual!
    (set-pprint-dispatch 'physical-quantity #'pprint-physical-quantity 1 it)))

(defun enable-pretty-printer-abuse (&optional (right-margin 67))
  (setf *print-pretty* t *print-right-margin* right-margin
        *print-pprint-dispatch* *physical-quantity-pprint-dispatch*))

;;;
;;; Assets
;;;

(defclass asset (registered-unit)
  ((name     :initarg :name     :reader name)
   (decimals :initarg :decimals :reader decimals)
   (exchange :initarg :exchange :reader exchange)))

(defmethod print-object ((asset asset) stream)
  (print-unreadable-object (asset stream :type nil :identity nil)
    (format stream "~A on ~A" (name asset) (exchange asset))))

(defgeneric find-asset (designator exchange)
  (:method (designator (assets list))
    (find designator assets :key 'name :test 'string-equal))
  (:method (designator (exchange exchange))
    (find-asset designator (assets exchange)))
  (:method (designator (name symbol))
    (find-asset designator (find-exchange name))))

;;;
;;; Asset Quantities
;;;

(deftype asset-quantity ()
  "A precise quantity of a given asset"
  '(complex unsigned-byte))
;;; the following is guaranteed by ANSI-compliance
;; (subtypep (upgraded-complex-part-type 'unsigned-byte)
;;           (upgraded-complex-part-type 'integer))

(defun asset (aq) (cdr (assoc (imagpart aq) *unit-registry*)))
(defun quantity (aq) (realpart aq))
(defun scaled-quantity (aq) (/ (realpart aq) (expt 10 (decimals (asset aq)))))

;;; what mnemonic, possibly allowing renames, for not conflating these?
(defun cons-aq (asset quantity) (complex (round quantity) (index asset)))
(defun cons-aq* (asset quantity)
  (cons-aq asset (* quantity (expt 10 (decimals asset)))))

(defun aq+ (aq1 aq2 &rest aqs)
  (cond
    (aqs (reduce #'aq+ aqs :initial-value (aq+ aq1 aq2)))
    ((eq (asset aq1) (asset aq2))
     (cons-aq (asset aq1) (+ (quantity aq1) (quantity aq2))))
    ((zerop aq1) aq2) ((zerop aq2) aq1)
    (t (error "assets ~A and ~A don't match" aq1 aq2))))

(defun aq- (aq1 &optional aq2 &rest aqs)
  (cond
    (aqs (aq- aq1 (reduce #'aq+ aqs :initial-value aq2)))
    ((eq (asset aq1) (asset aq2))
     (cons-aq (asset aq1) (- (quantity aq1) (quantity aq2))))
    ((zerop aq1) (cons-aq (asset aq2) (- (quantity aq2))))
    ((null aq2) (cons-aq (asset aq1) (- (quantity aq1)))) ((zerop aq2) aq1)
    (t (error "assets ~A and ~A don't match" aq1 aq2))))

;;;
;;; Markets
;;;

(defclass market (registered-unit)
  ((name     :initarg :name     :reader name)
   (tick     :initarg :tick     :reader tick)
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
      (find-market designator markets)))
  (:method (designator (name symbol))
    (find-market designator (find-exchange name))))

;;;
;;; Market Prices
;;;

(deftype market-price ()
  "A precise price in a given market"
  '(complex unsigned-byte))

(defgeneric market (object)
  (:method (object) (slot-value object 'market))
  (:method ((mp complex)) (cdr (assoc (imagpart mp) *unit-registry*))))
(defmethod price (mp) (abs (realpart mp))) ; STIP QUODLBING gpt\town-dumb
(defun scaled-price (mp) (/ (price mp) (expt 10 (decimals (market mp)))))
(defun cons-mp (market price) (complex (round price) (index market)))
(defun cons-mp* (market price)
  (cons-mp market (* price (expt 10 (decimals market)))))
(defmethod direction ((mp complex)) (if (plusp (realpart mp)) 'ask 'bid))
(defun scalp (mp) (1- mp))              ; this is actually good news!

(defgeneric  aq/ (given taken)
  (:method ((given complex) (taken complex)
            &aux (ag (asset given)) (at (asset taken)))
    ;; ideally, the following becomes declaration followed by typecase
    (assert (eq (exchange ag) (exchange at)) nil "assets from two exchanges")
    (dolist (market (markets (exchange ag))
                    (error "no market between ~A and ~A" (name ag) (name at)))
      (flet ((build (sign numerator denominator)
               (return (cons-mp* market (/ (scaled-quantity numerator) sign
                                           (scaled-quantity denominator))))))
        (with-slots (primary counter) market
          (when (and (eq ag primary) (eq at counter)) (build +1 taken given))
          (when (and (eq ag counter) (eq at primary)) (build -1 given taken)))))))

(defun aq* (mp aq &aux (q (scaled-quantity aq)) (p (scaled-price mp)))
  (with-slots (primary counter) (market mp)
    (cond ((eq (asset aq) primary) (cons-aq* counter (* q p)))
          ((eq (asset aq) counter) (cons-aq* primary (/ q p)))
          (t (error "~A not traded in ~A" (asset aq) (market mp))))))

;;;
;;; Offers
;;;

(defclass offer ()
  ((market :initarg :market :reader market)
   (taken  :initarg :taken  :reader taken)
   (given  :initarg :given  :reader given)
   (volume :initarg :volume :accessor volume)
   (price  :initarg :price  :reader price)))

;;; Originally, this TLA stood for "Offer ID", and designated
;;; the identifier given an offer by the exchange after entry
;;; to the live order book; however, this function must apply
;;; both to offers 1) published by exchanges aggregating from
;;; multiple other participants, and 2) that have no exchange
;;; -given identifier, whether due to rejection, xor locality
(defgeneric oid (offer) (:method ((offer offer)) ""))

(defclass offered (offer)
  ((oid    :initarg :oid    :reader oid)))

(defclass bid (offer) ()) (defclass ask (offer) ())

(defun consumed-asset (offer) (asset (given offer)))

(defmethod initialize-instance ((bid bid) &rest keys &key price)
  (apply #'call-next-method bid :price (- (abs price)) keys))

(defmethod shared-initialize :after ((offer offer) (names t) &key)
  (with-slots (market taken given volume price) offer
    (macrolet ((do-slot (slot aside primary bside counter)
                 `(unless (slot-boundp offer ',slot)
                    (setf ,slot (if (plusp price)
                                    (cons-aq* (,aside market) ,primary)
                                    (cons-aq* (,bside market) ,counter))))))
      (let ((factor (/ (abs price) (expt 10 (decimals market)))))
        (do-slot given primary volume counter (* volume factor))
        (do-slot taken counter (* volume factor) primary volume)))))

(defmethod print-object ((offer offer) stream)
  (print-unreadable-object
        (offer stream :type (not (typep offer 'offered)))
    (with-slots (given price market) offer
      (let ((market-decimals (slot-value market 'decimals))
            *print-readably* *print-escape* *print-circle*)
        (format stream "~@[~A ~]~A @ ~A" (shorten-uid (oid offer))
                given (decimals:format-decimal-number
                       (/ (abs price) (expt 10 market-decimals))
                       :round-magnitude (- market-decimals)
                       :show-trailing-zeros t))))))

;;;
;;; Rate Gate
;;;

(defclass gate (actor)
  ;; Had I a time machine, this would be named "turnstile";
  ;; it does a bit more than merely rate-limiting, credential
  ;; storage, cache invalidation, and occasional race conditions.
  ((exchange :initarg :exchange :initform (error "EI4NI") :reader exchange)
   (pubkey :initarg :pubkey :initform (error "gate requires API pubkey"))
   (secret :initarg :secret :initform (error "gate requires API secret"))
   (input  :initarg :input  :initform (make-instance 'channel))
   (output :initarg :output :initform (make-instance 'channel))
   (cache  :initform nil)))

(defmethod christen ((gate gate) (type (eql 'actor)))
  (subseq (slot-value gate 'pubkey) 0 3))

(defgeneric gate-post (gate pubkey secret request)
  (:documentation "Attempts to perform `request' with the provided credentials.")
  (:method :around (gate pubkey secret request)
    (rplacd request                     ; restart may belong in actors.lisp
            (restart-case (call-next-method gate pubkey secret (cdr request))
              (abort () :report "Abort request, keep gate" '(() :aborted))))))

(defmethod perform ((gate gate) &key)
  (call-next-method gate :blockp ())
  (with-slots (input output . #1=(exchange pubkey secret cache)) gate ; has-a
    (when (send-blocks-p output) (setf cache (recv input)) (gate-post . #1#))
    (send output cache)))

(defun gate-request (gate path &optional options &aux (id (cons path options)))
  (with-slots (input output) gate
    (send input (list* id path options))
    (multiple-value-bind (data excuses condition)
        ;; someday, stop seeking patterns in boilerplate garbage
        (loop for reply = (recv output) until (eq (car reply) id)
           do (send output reply) finally (return (values-list (cdr reply))))
      ;; someday, importantly, is not ever part of the 21st century
      (aif condition (signal it) (values data excuses)))))

;;;
;;; Public Data API
;;;

(defgeneric trades-since (market &optional since)
  ;; (:method :after ((market untrusted) &key) (when check wreck)) ;
  (:documentation "returns this market's recent public executions"))
(defgeneric get-book (market &key)
  ;; (:method :around ((market untrusted) &key) go on, make m'day) ;
  (:documentation "returns this market's open public limit offers"))

;;; should trade direction be represented as:
;;; a boolean slot?
;;; slots for (consumed|earned)*(volume|asset)?
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
       (cons-aq* counter (* volume (abs price)))))))

(defmethod print-object ((trade trade) stream)
  (print-unreadable-object (trade stream :identity nil)
    (with-slots (market volume price timestamp direction) trade
      (with-slots (primary decimals) market
        (format stream "~A ~A at ~V$: ~V$" timestamp
                direction decimals price (decimals primary) volume)))))

(defclass trades-fetcher (actor) ())

(defmethod christen ((fetcher trades-fetcher) (type (eql 'actor)))
  (format nil "trade fetcher ~A" (market fetcher)))

(defmethod perform ((fetcher trades-fetcher) &key)
  (with-slots (market buffer delay) fetcher
    (dolist (trade (aif (recv buffer) (trades-since market it)
                        (trades-since market)))
      ;; this unoptimization is starting to stink of prematurity ... .
      (send buffer trade))
    (sleep delay)))

(defclass trades-tracker (parent)
  ((market :initarg :market :reader market)
   (delay  :initarg :delay  :initform 47)
   (buffer :initform (make-instance 'channel))
   (output :initform (make-instance 'channel))
   (trades :initform nil) fetcher))

(defmethod christen ((tracker trades-tracker) (type (eql 'actor)))
  (format nil "trade tracker ~A" (market tracker)))

(defmethod perform ((tracker trades-tracker) &key)
  (with-slots (buffer trades market output) tracker
    (let ((last (car trades)))
      (select
        ((send buffer last)) ((send output trades)) ;D   ^V^   ]:C
        ((recv buffer next)             ;              batsign
         (if (trades-mergeable? tracker last next)
             (setf (car trades) (merge-trades tracker last next))
             (push next trades)))
        (t (sleep 0.2))))))

(defmethod initialize-instance :after ((tracker trades-tracker) &key)
  (adopt tracker (setf (slot-value tracker 'fetcher)
                       (make-instance 'trades-fetcher :delegates `(,tracker)))))

(defgeneric vwap (tracker &key &allow-other-keys)
  (:method :around ((tracker t) &key)
    (handler-case (float (call-next-method) 0s0) (division-by-zero () 0)))
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
      (/ (reduce #'+ (mapcar #'cost trades))
         (reduce #'+ (mapcar #'volume trades))))))

;;; look, yer sus; thinking of the future already!
;;; dispatch on trades-tracker so that exchange-specific methods can
;;; dispatch on just the tracker argument, rather than both prev and next
;;; face it, sometimes you just gotta (eval '(expt +evil+ 1/2))

(defgeneric trades-mergeable? (trades-tracker prev next)
  (:method-combination and)
  (:method and ((tracker t) (prev null) (next t)))
  (:method and ((tracker trades-tracker) (prev trade) (next trade))
    (with-slots (exchange) (slot-reduce tracker market)
      (when (slot-boundp exchange 'market-timestamp-sensitivity)
        (with-slots (market-timestamp-sensitivity) exchange
          (and (string= (direction prev) (direction next))
               (> market-timestamp-sensitivity
                  (timestamp-difference (timestamp next)
                                        (timestamp prev)))))))))

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
(defmethod timestamp ((timestamp timestamp)) timestamp)

;;;
;;; ORDER BOOK
;;;

(defclass book-fetcher (actor) ())

(defmethod christen ((fetcher book-fetcher) (type (eql 'actor)))
  (format nil "depth fetcher ~A" (market fetcher)))

(defmethod perform ((fetcher book-fetcher) &key)
  (with-slots (buffer delay market get-book-keys) fetcher
    (send buffer (multiple-value-call 'cons
                   (apply #'get-book market get-book-keys)))
    (sleep delay)))

(defclass book-tracker (parent)
  ((market :initarg :market :reader market :initform (error "required"))
   (get-book-keys :initform () :initarg :get-book-keys)
   (delay :initarg :delay :initform 1) (book :initform ())
   (buffer :initform (make-instance 'channel)) fetcher
   (output :initform (make-instance 'channel))))

(defmethod christen ((tracker book-tracker) (type (eql 'actor)))
  (format nil "depth tracker ~A" (market tracker)))

(defmethod perform ((tracker book-tracker) &key)
  (with-slots (buffer book output) tracker
    (select ((recv buffer next)
             (unless (or (null (car next)) (null (cdr next)))
               (setf book (cons (cdr next) (car next)))))
            ((send output book)) (t (sleep 0.2)))))

(defmethod initialize-instance :after ((tracker book-tracker) &key)
  (adopt tracker (setf (slot-value tracker 'fetcher)
                       (make-instance 'book-fetcher :delegates `(,tracker)))))

;;;
;;; Putting things together
;;;

(defclass tracked-market (market parent)
  ((%market :initarg :market) (book :reader book) (trades :reader trades)
   book-tracker trades-tracker))        ; unleash the cspaken!

;;; shameful? yes disgusting? yes preserves dispatch? yes
(defmethod update-instance-for-different-class :after
    ((prev market) (new tracked-market) &key)
  (macrolet ((init (feed &aux (class (intern (format nil "~A-TRACKER" feed))))
               `(adopt new (aprog1 (make-instance ',class :market %market)
                             (setf (slot-value new ',class) it
                                   (slot-value new ',feed)
                                   (slot-value it 'output))))))
    (with-slots (%market book trades delegates) new
      (push (setf %market (shallow-copy prev)) delegates)
      (init book) (init trades))))

;;; tonight we dine in fail
(defmethod get-book ((market tracked-market) &key)
  (get-book (slot-value market '%market)))
(defmethod trades-since ((market tracked-market) &optional since)
  (if (null since) (trades-since (slot-value market '%market))
      (trades-since (slot-value market '%market) since)))

(defmethod vwap ((market tracked-market) &rest keys)
  (apply #'vwap (slot-value market 'trades-tracker) keys))

(defmethod ensure-running ((market market))
  (change-class market 'tracked-market))
(defmethod ensure-running ((market tracked-market)) market)

(defgeneric print-book (book &key &allow-other-keys)
  (:method ((tracker book-tracker) &rest keys)
    (apply #'print-book (recv (slot-value tracker 'output)) keys))
  (:method ((market tracked-market) &rest keys)
    (apply #'print-book (slot-value market 'book-tracker)
           :prefix :military-time keys))
  (:method ((book cons) &key count prefix ours)
    ;; (declare (optimize debug))
    (when (eq prefix :military-time)
      (setf prefix (format-timestring () (now)
                                      :format '((:hour 2) (:min 2)))))
    (destructuring-bind (bids . asks) book
      (flet ((width (side)
               (flet ((vol (noise) (quantity (given noise))))
                 ;; (loop for spark in side repeat (or count ()))
                 (loop for o in side repeat (or count 15)
                    for max = o then (if (> (vol o) (vol max)) o max)
                    ;; ... is this another ANSI compliance test case?
                    finally (return (length (princ-to-string max)))))))
        (let ((ctrl (multiple-value-call #'format
                      () "~~&~~@[~~A ~~]~~~D@A ~~~D@A~~%"
                      (if (atom ours) (values (width bids) (width asks))
                          (destructuring-bind (my-bids . my-asks) ours
                            (values (max (width bids) (width my-bids))
                                    (max (width asks) (width my-asks))))))))
          (flet ((line (bid ask) (format t ctrl prefix bid ask)))
            (if (atom ours)
                (do ((bids bids (rest bids)) (asks asks (rest asks)))
                    ((or (and (null bids) (null asks))
                         (and (numberp count) (= -1 (decf count)))))
                  (line (first bids) (first asks)))
                (macrolet ((side (stash mine book)
                             ;; more fenceposts masquerading as
                             ;; off-by-null errors! ad matai AI
                             `(and ,mine ,book (>= (price (car ,book))
                                                   (price (car ,mine)))
                                   (> (volume (car ,book))
                                      (volume (setf ,stash (pop ,mine)))))))
                  (do ((my-bids (car ours)) (my-asks (cdr ours))
                       (bids bids) (asks asks) (mbid () ()) (mask () ()))
                      ((or (and (null bids) (null asks))
                           (and (numberp count) (= -1 (decf count)))))
                    (let ((bs (side mbid my-bids bids))
                          (as (side mask my-asks asks)))
                      (line (or mbid (car bids)) (or mask (car asks)))
;; this still happens, occasionally...
;;  #<1651667563 82.83 Nis @ 3.38>  #<1651655211 23.89966685 Usdc @ 3.59>
;;    #<BID  105596.76 Nis @ 3.38>  #<1651681181 25.69897127 Usdc @ 3.59>
;;     #<BID  60031.87 Nis @ 3.37>        #<ASK  49.59863812 Usdc @ 3.59>
;; #<1651675524 127.76 Nis @ 3.36>     #<ASK  10086.18000000 Usdc @ 3.60>
;; #<1389509474 730.71 Nis @ 0.09> #<1389508943 32.87402000 Usdc @ 78.99>
;;       #<BID  730.71 Nis @ 0.09>     #<ASK  1000.00000000 Usdc @ 79.00>

                      (or bs (pop bids)) (or as (pop asks))))))
            ;; WHY IS THAT BUG'S LIFE EXPECTANCY LONGER THAN MINE !?
            (flet ((shit (shy nola)     ; so es dreht...
                     (when shy
                       (let ((decimals (decimals (market (first shy)))))
                         (format () "~A ~C ~V$ "
                                 (reduce #'aq+ (mapcar #'given shy)) ; ~A
                                 nola                                ; ~C
                                 decimals                            ; $V
                                 (abs (/ (price (first (last shy)))  ; ~$
                                         (expt 10 decimals))))))))
              (format t "~&Totals:~%")
              (line (shit bids #\>) (shit asks #\<))))))))
  (:method ((tracker book-tracker) &rest keys)
    (apply #'print-book (recv (slot-value tracker 'output)) keys))
  (:method ((market tracked-market) &rest keys)
    (apply #'print-book (slot-value market 'book-tracker) keys)))

;;;
;;; Private Data API
;;;

(defgeneric agent-trunk (agent)          ; CORNELIUS ? BABAR !
  (:method-combination append))

(defgeneric placed-offers (gate &optional market))
(defgeneric account-balances (gate))

(defclass balance-tracker (actor)
  ((fuzz :initarg :fuzz :initform 2)
   (sync :initarg :sync :initform (make-instance 'channel))
   (balances :initarg :balances :initform ())
   (reserved :initarg :reserved :initform ())
   (abbrev :allocation :class :initform "funds")))

(defmethod perform ((tracker balance-tracker) &key)
  (with-slots (sync fuzz balances reserved) tracker
    (let ((target (recv sync)))         ; back and forth
      (when (zerop (random fuzz))       ; in the absence of memory...
        (awhen1 (with-slots (gate) (car (slot-reduce tracker delegates))
                  (account-balances gate)) ; maybify API failability
          (setf balances                   ; sub-clude the reserved aqs
                (loop for aq in it for asset = (asset aq) collect
                     (aif (remove asset reserved ;key #'cxbtc
                                  :test-not #'eq :key #'asset)
                          (aq- aq (reduce   ;_; should (zerop (aq+))?
                                   #'aq+ it ; like maybe patterns!?
                                   :initial-value (cons-aq asset 0)))
                          aq)))))
      (send target balances))))

(defmethod christen ((tracker balance-tracker) (type (eql 'actor)))
  (slot-reduce tracker gate name))

(defun asset-funds (asset funds)
  (aif (find asset funds :key #'asset) (scaled-quantity it) 0))

(defgeneric squash-reservations (actor)
  (:method ((treasurer balance-tracker) &aux aqs #|(sff t)|#)
    (with-slots (reserved) treasurer
      (dolist (aq reserved aqs)
        (aif (member (asset aq) aqs :key #'asset)
             (rplaca it (aq+ (car it) aq))
             (push aq aqs))))))

(defgeneric market-fee (gate market)
  (:documentation "(bid . ask) fees, in percent")
  (:method ((gate gate) (market tracked-market))
    (market-fee gate (slot-reduce market scalpl.exchange::%market)))
  (:method :around ((gate gate) (market market))
    (actypecase (call-next-method) (number `(,it . ,it)) (cons it) (null)))
  (:method ((gate gate) (market tracked-market))
    (market-fee gate (slot-value market '%market))))

(defclass fee-fetcher (actor)
  ((abbrev :allocation :class :initform "fee fetcher")))

(defmethod christen ((fetcher fee-fetcher) (type (eql 'actor)))
  (with-slots (gate market) fetcher
    (format nil "~A ~A" (name gate) (name market))))

(defclass fee-tracker (parent)
  ((abbrev :allocation :class :initform "fee tracker")
   (delay  :initarg :delay  :initform 127) fee fetcher
   (input  :initarg :input  :initform (make-instance 'channel))
   (output :initarg :output :initform (make-instance 'channel))))

(defmethod christen ((tracker fee-tracker) (type (eql 'actor)))
  (with-slots (gate market) tracker
    (format nil "~A ~A" (name gate) (name market))))

(defmethod perform ((fetcher fee-fetcher) &key)
  (with-slots (market gate delay input) fetcher
    (awhen (market-fee gate market) (send input it))
    (sleep delay)))

(defmethod perform ((tracker fee-tracker) &key)
  (with-slots (input output fee) tracker
    (ignore-errors (select ((recv input new) (setf fee new))
                           ((send output fee)) (t (sleep 1/7))))))

(defmethod initialize-instance :after ((tracker fee-tracker) &key)
  (adopt tracker (setf (slot-value tracker 'fetcher)
                       (make-instance 'fee-fetcher :delegates (list tracker)))))

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

(defgeneric execution-since (gate market since)
  (:method ((gate gate) (market tracked-market) since)
    (execution-since gate (slot-value market '%market) since)))

(defclass execution-fetcher (actor)
  ((abbrev :allocation :class :initform "exhun fetcher")))

(defmethod christen ((fetcher execution-fetcher) (type (eql 'actor)))
  (with-slots (gate market) fetcher
    (format nil "~A ~A" (name gate) (name market))))

(defmethod perform ((fetcher execution-fetcher) &key)
  (with-slots (gate market buffer delay) fetcher
    (dolist (trade (execution-since gate market (recv buffer)) (sleep delay))
      (send buffer trade))))

(defclass execution-tracker (parent)
  ((abbrev :allocation :class :initform "exhun tracker")
   (trades :initform nil :initarg :trades) (bases :initform nil)
   (delay :initform 11) (buffer :initform (make-instance 'channel)) fetcher))

(defmethod christen ((tracker execution-tracker) (type (eql 'actor)))
  (with-slots (gate market) tracker
    (format nil "~A ~A" (name gate) (name market))))

(defmethod execute ((tracker execution-tracker) (command (eql :flush)))
  (setf (slot-reduce tracker bases) nil))

(defmethod execute ((tracker execution-tracker) (command (eql :rebase)))
  (with-slots (bases trades) tracker
    (setf bases nil) (dolist (next (reverse trades)) ; &a-o-k ?
                       (update-bases tracker next))))

(defmethod perform ((tracker execution-tracker) &key)
  (with-slots (buffer trades bases control delegates) tracker
    (select ((recv buffer next) (update-bases tracker next) (push next trades)
             (send (slot-reduce (car delegates) control) (list (oid next))))
            ((recv control command) (execute tracker command))
            ((send buffer (first trades))) (t (sleep 0.2)))))

(defmethod initialize-instance :after ((tracker execution-tracker) &key)
  (adopt tracker
         (setf (slot-value tracker 'fetcher)
               (make-instance 'execution-fetcher :delegates `(,tracker)))))

(defmethod shared-initialize :after
    ((tracker execution-tracker) (names t) &key) (execute tracker :rebase))

;;; This function returns three values:
;;;   1) remaining bases, after deducting the given aq (can be nil)
;;;   2) the vwab price of the given bases
;;;   3) the actual cost of the given bases
;;; What is it fed?
;;;   bases - just the ones for the asset-to-give, from the relevant market
;;;   aq - that you wish to give
;;; Who calls it?
;;;   update-bases, to compute the remaining basis for a given asset
;;;   ope-spreader (TODO), to check whether a certain offer is profitable

(defun bases-without (bases given)
  (handler-case
      (loop for basis = (pop bases) for (bp baq other) = basis
         for acc = baq then (aq+ acc baq) for vwab-sum = other
         then (aq+ vwab-sum other) sum (* (price bp) (quantity baq)) into pwa
         when (> (quantity acc) (quantity given)) return
           (let* ((excess (aq- acc given))
                  (other (cons-aq (asset other)
                                  (* (quantity other)
                                     (/ (quantity excess) (quantity baq)))))
                  (recur (aq- vwab-sum other)))
             (values (cons (list bp excess other) bases)
                     (cons-mp (market bp)
                              (/ (- pwa (* (price bp) (quantity excess)))
                                 (quantity given)))
                     recur))
         when (null bases) return
           (values nil (aq/ vwab-sum acc) vwab-sum))
    ((or division-by-zero arithmetic-error) ())))

(defun update-bases (tracker trade)
  (unless (zerop (volume trade))        ; thx polo
    (with-slots (bases) tracker
      (with-slots (taken given price market) trade
        (swhen (getf bases (asset given)) (setf it (bases-without it given)))
        (setf (getf bases (asset taken))
              (flet ((row-price (row) (realpart (first row))))
                (let ((price (aq/ given taken))
                      (old-bases (getf bases (asset taken))))
                  (aif (assoc price old-bases)
                       (destructuring-bind (old-taken old-given) (cdr it)
                         (merge 'list (remove it old-bases)
                                `((,price ,(aq+ taken old-taken)
                                          ,(aq+ given old-given)))
                                #'> :key #'row-price))
                       (merge 'list old-bases `((,price ,taken ,given))
                              #'> :key #'row-price)))))))))

(defmethod vwap ((tracker execution-tracker) &key type depth)
  (let ((trades (slot-value tracker 'trades)))
    (when type
      (setf trades (remove type trades :key #'direction :test #'string-not-equal)))
    (when depth (setf trades
                      (loop for trade in trades collect trade
                         sum (volume trade) into sum until (>= sum depth))))
    (/ (reduce '+ (mapcar #'net-cost trades))
       (reduce '+ (mapcar #'net-volume trades)))))

(defun basis-offer (basis)
  (destructuring-bind (price taken given) basis
    (make-instance 'offer :price price :market (market price)
                   :taken taken :given given)))

(defmethod print-book ((lictor execution-tracker) &rest keys)
  (with-slots (bases market) lictor
    (with-slots (primary counter) market
      (apply #'print-book
             (cons (mapcar #'basis-offer (getf bases counter))
                   (mapcar #'basis-offer (getf bases primary)))
             keys))))

;;;
;;; Action API
;;;

;;; oughtta #'change-class to 'offered in :around
(defgeneric post-offer (gate offer)
  (:method ((gate gate) (offers list))
    (mapcar (lambda (offer) (post-offer gate offer)) offers))
  (:method ((gate gate) (offer offered))
    ;; TODO: choose [k]not to kill yourself; THEN, rephrase thisstring
    (warn "Tried placing an offer that is already placed: ~A" offer)))

;;; returns nil, unless the offer is definitely no longer active!
;;; could someday return different non-nil values if the offer
;;; was executed, partially or fully, before cancellation
(defgeneric cancel-offer (gate offer)   ; cladographers always wonder...
  (:method ((gate gate) (offer offer))  ; what kind of sadistic dastardly
    (warn "Tried cancelling unplaced offer ~A" offer)) ; pun-laced tripe
  (:method ((gate gate) (offers list))                 ; ] was that ?
    (mapcar (lambda (offer) (cancel-offer gate offer)) offers)))

(defclass supplicant (parent)
  ((gate :initarg :gate) (market :initarg :market :reader market)
   (offered :initform ())	  ; (placed :initform ()) ; need both!
   (response :initform (make-instance 'channel))
   (abbrev :allocation :class :initform "supplicant")
   (treasurer :initarg :treasurer)
   (lictor :initarg :lictor) (fee :initarg :fee)
   (order-slots :initform 40 :initarg :order-slots)))

(defun offers-spending (ope asset)
  (remove asset (slot-value ope 'offered)
          :key #'consumed-asset :test-not #'eq))

(defun balance-guarded-place (ope &rest offers)
  (with-slots (gate offered order-slots treasurer) ope
    (let* ((asset (consumed-asset (first offers)))
           (spending (offers-spending ope asset))
           (mapreduc (reduce #'aq+ (mapcar #'given (append spending offers))))
           (ourfunds (asset-funds asset (slot-reduce treasurer balances))))
      (and (>= ourfunds (scaled-quantity mapreduc))
           (>= order-slots (+ (length offered) (length offers)))
           (atypecase (post-offer gate offers)
             (offered (push it offered))
             ;; The retention of null return values is retained, in optimistic
             ;; anticipation of the day when failures will contain information
             ;; beyond merely the fact of failure itself.
             (list (dolist (offer it) (when offer (push offer offered)))))))))

(defun sufficiently-different? (new old) ; someday dispatch on market
  (declare (optimize (compilation-speed 0) speed))
  (< 0.1 (abs (log (/ (quantity (given new)) (quantity (given old)))))))

;;; FIXME: disambiguate placement from offerage, and redichotomise the book
(defmethod placed-offers ((supplicant supplicant) &optional market)
  (with-slots (gate) supplicant (placed-offers gate market)))

(defmethod execute ((supplicant supplicant) (cons cons) &aux (arg (cdr cons)))
  (with-slots (gate response offered market) supplicant
    (acase (car cons)
      (:cancel (when (cancel-offer gate arg)
                 (setf offered (set-difference offered arg :key #'oid
                                              :test #'string=))))
      (:offer (apply #'balance-guarded-place supplicant arg))
      (:sync (send response
                   (setf offered (placed-offers supplicant market))))
      (t (setf offered (remove it offered :test #'string= :key #'oid))))))

(defmethod christen ((supplicant supplicant) (type (eql 'actor)))
  (with-aslots (gate market) supplicant
    (format nil "~A ~A" (name gate) (name market))))

(defmethod initialize-instance :after ((supp supplicant) &key)
  (macrolet ((init (slot class)
               `(unless (ignore-errors ,slot)
                  (adopt supp (setf ,slot (make-instance
                                           ',class :delegates `(,supp)))))))
    (with-slots (fee lictor treasurer offered market gate) supp
      (adopt supp (ensure-running market)) (adopt supp gate)
      (init   fee          fee-tracker)
      (init  lictor  execution-tracker)
      (init treasurer  balance-tracker)
      (unless (ignore-errors offered)
        (setf offered (placed-offers gate market))))))

(defgeneric bases-for (supplicant asset)
  (:method ((supplicant supplicant) (market market))
    (values (getf (slot-reduce supplicant lictor bases) (primary market))
            (getf (slot-reduce supplicant lictor bases) (counter market))))
  (:method ((supplicant supplicant) (asset asset))
    (with-slots (primary counter %market) (market supplicant)
      (cond ((eq asset counter)
             (nth-value 1 (bases-for supplicant %market)))
            ((eq asset primary)
             (bases-for supplicant %market)))))) ;

(defmethod agent-trunk append ((dowser supplicant))
  (list (slot-reduce dowser market)     ; hyperdimensionally,
        (slot-reduce dowser gate)))     ; going embryological

(defmethod print-book ((supplicant supplicant) &rest keys)
  (with-slots (counter primary) (market supplicant)
    (apply #'print-book
           (acons (mapcar #'basis-offer (bases-for supplicant counter))
                  (mapcar #'basis-offer (bases-for supplicant primary))
                  keys))))

;;;
;;; Here should begin the CLI directory, full of curses, horrors,
;;; and worst of all, commented curses at the curses horrors within.
;;;

;; (defgeneric describe-book ((book book) cook whither))

;;;
;;; Who's fault is it that default puns are considered funny by default?
;;;

(defgeneric describe-account (supplicant exchange stream)
  (:method ((supplicant t) (exchange t) (stream t))
    (cerror "YODO" "How many times have you died, this week?"))
  (:method ((supplicant t) (exchange exchange) (stream t))
    (error "I don't tame lions, how do you expect me to balance books?"))
  (:documentation "summarize how things are going, profit-wise"))

(defun respawn-syncer (&optional (supplicant *supplicant*) (wavenumber 17))
  (pexec (:name "syncer")
    (loop (with-slots (control response) supplicant
            (send control '(:sync)) (recv response) (sleep wavenumber)))))
