(defpackage #:scalpl.numerics
  (:nicknames #:scalpl.mathematics #:scalpl.abuse)
  (:use #:cl #:chanl #:anaphora #:local-time #:parse-float
        #:scalpl.util #:scalpl.actor    ; #:scalpl.net ?
        )
  (:export #:enable-pretty-printer-abuse
           #:exchange #:name #:assets #:markets #:parse-timestamp
           #:*exchanges* #:find-exchange #:fetch-exchange-data

           ;; FIXME the following symbol belongs in a processing layer
           ;; beyond the data model that is the domain of this file
           #:market-timestamp-sensitivity

           #:*unit-registry* #:asset #:find-asset #:asset-quantity
           #:quantity #:scaled-quantity #:cons-aq #:cons-aq* #:aq+ #:aq-
           #:market #:tick #:decimals #:primary #:counter #:find-market
           #:scaled-price #:cons-mp #:cons-mp* #:scalp #:aq/ #:aq*

           ;; TODO decide whether these belong in this file's data model
           #:offer #:bid #:ask #:offered #:taken #:given
           #:volume #:price #:placed #:oid #:consumed-asset #:cutoff
           #:gate #:gate-post #:gate-request #:output #:input #:cache
           #:trade #:cost #:direction #:txid #:tracked-market

           ;; #:agent-trunk #:delay #:fetcher

           ;; #:trades-tracker #:trades #:trades-since #:vwap #:book
           ;; #:book-tracker #:bids #:asks #:get-book #:get-book-keys
           ;; #:balance-tracker #:balances #:sync #:print-book #:asset-funds

           ;; #:placed-offers #:account-balances #:market-fee
           ;; #:execution #:fee #:net-cost #:net-volume #:fee-tracker
           ;; #:execution-tracker #:execution-since #:bases #:bases-without
           ;; #:post-offer #:cancel-offer #:supplicant #:lictor #:treasurer
           ;; #:order-slots #:response #:supplicate #:bases-for #:reserved
           ;; #:squash-reservations #:sufficiently-different?

           ))

(in-package #:scalpl.abuse)

;;; TODO
;;; This file MIGHT sketch an interface that some venues implement.
;;; Each client file could then instantiate objects consuming that,
;;; and specialize methods on generic functions that MUST follow...

;;; FIXME
;;; "Numerics" is not the best name, because things like venues and
;;; exchanges are not considered a scalar dimension or combination of
;;; simpler dimensions; and the fact that venues might change their
;;; set of listed symbols means that they could not be defined as some
;;; join or outer product on the set of initially listed symbols.

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

;;; FIXME
;;; the above comment block was composed before the venues/ directory...

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
;;; TODO read that entire article again, and then read the Talk page

(defvar *unit-registry* '(()))          ; Mathematicians HATE him!

(defclass registered-unit ()
  ((index :initform (length *unit-registry*) :reader index)))

(defmethod index ((null null)) 0)

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
  (warn "Incomplete;! c.f. `CL:*print-miser-width*'")
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

(macrolet ((define-filter (name &optional (keyword :test))
             `(defun ,name (&optional (registry *unit-registry*))
                (remove 'asset (mapcar 'cdr registry)
                        ,keyword 'subtypep :key 'class-of))))
  (define-filter registered-markets)
  (define-filter registered-assets :test-not))

;;;
;;; Asset Quantities
;;;

(deftype asset-quantity ()
  "A precise quantity of a given asset"
  '(complex unsigned-byte))
;;; the following is guaranteed by ANSI-compliance
;; (subtypep (upgraded-complex-part-type 'unsigned-byte)
;;           (upgraded-complex-part-type 'integer))

;;; TODO: overengineer the space of ANSI-compliance even further
;;; without depending upon the X3J13 bus factor; sadly, that one
;;; decreases monotonically across time, universal and internal!

;;; I composed the following code so long ago, it's probably banned
;;; from /r/copypasta by now:
;; (let #1=(types conditions)
;;   (do-external-symbols (symbol :COMMON-LISP (values . #1#))
;;     (multiple-value-bind (subtypep typep)
;;         (ignore-errors (subtypep symbol 'condition))
;;       (when typep
;;         (if subtypep
;;             (push symbol conditions)
;;             (push symbol types))))))

(defun asset (aq) (cdr (assoc (imagpart aq) *unit-registry*)))
(defun quantity (aq) (realpart aq))
(defun scaled-quantity (aq) (/ (realpart aq) (expt 10 (decimals (asset aq)))))

;;; what mnemonic, possibly allowing renames, for not conflating these?
(defun cons-aq (asset quantity) (complex (round quantity) (index asset)))
(defun cons-aq* (asset quantity)
  (cons-aq asset (* quantity (expt 10 (decimals asset)))))
;;; ... normalized? unprefixed? hehnerised? naturalised?

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
    ((null aq2) (cons-aq (asset aq1) (- (quantity aq1))))
    ((eq (asset aq1) (asset aq2))
     (cons-aq (asset aq1) (- (quantity aq1) (quantity aq2))))
    ((zerop aq1) (cons-aq (asset aq2) (- (quantity aq2))))
    ((zerop aq2) aq1)
    (t (error "assets ~A and ~A don't match" aq1 aq2))))

;;; exercise for CL learners:
;;; compose a toplevel comment block explaining why neither of the above
;;; functions are generic functions, nor toplevel defmethod orphans ...!

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

(defgeneric aq/ (given taken)
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

;;; exercise for CL learners:
;;; compose a toplevel comment block explaining why only one of the above
;;; functions is a generic function, without any toplevel defmethod forms
