;;;; qd.lisp

(defpackage #:scalpl.qd
  (:use #:cl #:chanl #:anaphora #:st-json #:local-time #:scalpl.util #:scalpl.exchange))

(in-package #:scalpl.qd)

;;;
;;;  ENGINE
;;;

(defclass ope-scalper ()
  ((input :initform (make-instance 'channel))
   (output :initform (make-instance 'channel))
   (supplicant :initarg :supplicant)
   (filter :initarg :filter)
   (epsilon :initform 0.001 :initarg :epsilon)
   (count :initform 30 :initarg :offer-count)
   (magic :initform 3 :initarg :magic-count)
   prioritizer thread))

(defclass ope-supplicant ()
  ((gate :initarg :gate)
   (placed :initform nil :initarg :placed)
   (control :initarg :control :initform (make-instance 'channel))
   (response :initarg :response :initform (make-instance 'channel))
   (balance-tracker :initarg :balance-tracker)
   (order-slots :initform 40 :initarg :order-slots)
   thread))

(defclass ope-prioritizer ()
  ((next-bids :initform (make-instance 'channel))
   (next-asks :initform (make-instance 'channel))
   (response :initform (make-instance 'channel))
   (supplicant :initarg :supplicant) thread
   (frequency :initarg :frequency :initform 1/7)))

(defun offers-spending (ope asset)
  (remove asset (slot-value ope 'placed)
          :key #'consumed-asset :test-not #'eq))

(defun balance-guarded-place (ope offer)
  (with-slots (gate placed order-slots balance-tracker) ope
    (let ((asset (consumed-asset offer)))
      (when (and (>= (asset-balance balance-tracker asset)
                     (reduce #'+ (mapcar #'volume (offers-spending ope asset))
                             :initial-value (volume offer)))
                 (> order-slots (length placed)))
        (awhen1 (post-offer gate offer) (push it placed))))))

;;; TODO: deal with partially completed orders
(defun ignore-offers (open mine &aux them)
  (dolist (offer open (nreverse them))
    (aif (find (price offer) mine :test #'= :key #'price)
         (let ((without-me (- (volume offer) (volume it))))
           (setf mine (remove it mine))
           (unless (< without-me 0.001)
             (push (make-instance 'offer :market (slot-value offer 'market)
                                  :price (price offer)
                                  :volume without-me)
                   them)))
         (push offer them))))

;;; receives messages in the control channel, outputs from the gate
(defun ope-supplicant-loop (ope)
  (with-slots (gate control response placed) ope
    (let ((command (recv control)))
      (destructuring-bind (car . cdr) command
        (send response (case car
                         (placed placed)
                         (filter (ignore-offers cdr placed))
                         (offer (balance-guarded-place ope cdr))
                         (cancel (awhen1 (cancel-offer gate cdr)
                                   (setf placed (remove cdr placed))))))
        (write-char (case car (offer #\o) (cancel #\c) (t #\Space)))
        (force-output)))))

(defmethod shared-initialize :after ((supplicant ope-supplicant) (slots t) &key)
  (with-slots (thread) supplicant
    (when (or (not (slot-boundp supplicant 'thread))
              (eq :terminated (task-status thread)))
      (setf thread (pexec (:name "qdm-preα ope supplicant")
                     (loop (ope-supplicant-loop supplicant)))))))

(defun ope-placed (ope)
  (with-slots (placed) (slot-value ope 'supplicant)
    (let ((all (sort (copy-list placed) #'< :key #'price)))
      (flet ((split (sign)
               (remove sign all :key (lambda (x) (signum (price x))))))
        ;;       bids       asks
        (values (split 1) (split -1))))))

;;; response: placed offer if successful, nil if not
(defgeneric ope-place (ope offer))
(defmethod ope-place ((ope ope-prioritizer) offer)
  (ope-place (slot-value ope 'supplicant) offer))
(defmethod ope-place ((ope ope-supplicant) offer)
  (with-slots (control response) ope
    (send control (cons 'offer offer)) (recv response)))

;;; response: trueish = offer no longer placed, nil = unknown badness
(defun ope-cancel (ope offer)
  (with-slots (control response) (slot-value ope 'supplicant)
    (send control (cons 'cancel offer)) (recv response)))

(defclass fee-tracker ()
  ((market :initarg :market :accessor market)
   (delay  :initarg :delay  :initform 67)
   (gate   :initarg :gate)
   (input  :initarg :input  :initform (make-instance 'channel))
   (output :initarg :output :initform (make-instance 'channel))
   fee updater server))

(defun fee-updater-loop (tracker)
  (with-slots (market gate delay input) tracker
    (awhen (market-fee gate market) (send input it))
    (sleep delay)))

(defun fee-server-loop (tracker)
  (with-slots (input output fee) tracker
    (ignore-errors (select ((recv input new) (setf fee new))
                           ((send output fee)) (t (sleep 1/7))))))

(defmethod shared-initialize :after ((tracker fee-tracker) (names t) &key)
  (with-slots (updater server market) tracker
    (when (or (not (slot-boundp tracker 'updater))
              (eq :terminated (task-status updater)))
      (setf updater
            (pexec
                (:name (concatenate 'string "qdm-preα fee updater for " (name market))
                 :initial-bindings `((*read-default-float-format* double-float)))
              ;; TODO: just pexec anew each time...
              ;; you'll understand what you meant someday, right?
              (loop (fee-updater-loop tracker)))))
    (when (or (not (slot-boundp tracker 'server))
              (eq :terminated (task-status server)))
      (setf server
            (pexec
                (:name (concatenate 'string "qdm-preα fee server for " (name market))
                 :initial-bindings `((*read-default-float-format* double-float)))
              ;; TODO: just pexec anew each time...
              ;; you'll understand what you meant someday, right?
              (loop (fee-server-loop tracker)))))))

(defclass ope-filter ()
  ((bids       :initarg :bids       :initform (make-instance 'channel))
   (asks       :initarg :asks       :initform (make-instance 'channel))
   (market     :initarg :market     :initform (error "must link market"))
   (supplicant :initarg :supplicant :initform (error "must link supplicant"))
   (frequency  :initarg :frequency  :initform 1/7) ; TODO: push depth deltas
   (lictor     :initarg :lictor     :initform (error "must link lictor"))
   (rudder     :initarg :rudder     :initform '(() . ()))
   fee foreigners thread))

;;; needs to do three different things
;;; 1) ignore-mine - already does (via filter-book)
;;; 2) profitable spread - already does (via ecase spaghetti)
;;; 3) profit vs recent cost basis - done, shittily - TODO parametrize depth

(defun ope-filter-loop (ope)
  (with-slots (market foreigners supplicant fee lictor rudder) ope
    (flet ((filter-book (side)
             (with-slots (control response) supplicant
               (send control `(filter . ,side)) (recv response)))
           (xyz (asset rudder type)
             (aif (getf (slot-reduce lictor bases) asset)
                  (scaled-price (nth-value 1 (bases-without
                                              it (cons-aq* asset rudder))))
                  (vwap lictor :type type :depth rudder :net t))))
      (destructuring-bind (bfee . afee) (recv (slot-value fee 'output))
        (destructuring-bind (bids . asks)
            (recv (slot-reduce market book-tracker output))
          (let ((other-bids (filter-book bids))
                (other-asks (filter-book asks)))
            (loop
               for best-bid = (1- (price (car other-bids)))
               for best-ask = (1- (price (car other-asks)))
               for spread = (profit-margin (abs best-bid) best-ask bfee afee)
               until (> spread 1) do
                 (ecase (round (signum (* (max 0 (- best-ask best-bid 5))
                                          (- (volume (car other-bids))
                                             (volume (car other-asks))))))
                   (-1 (decf (volume (car other-asks)) (volume (pop other-bids))))
                   (+1 (decf (volume (car other-bids)) (volume (pop other-asks))))
                   (0 (pop other-bids) (pop other-asks)))
               finally (setf foreigners (cons other-bids other-asks)))))
        (let ((quotient (expt 10 (decimals market)))
              (svwap (xyz (counter market) (car rudder) "sell"))
              (bvwap (xyz (primary market) (cdr rudder) "buy")))
          (macrolet ((do-side (vwap side &body args)
                       `(unless (zerop ,vwap)
                          (swhen (,side foreigners)
                            (loop for best = (first it)
                               for spread = (profit-margin ,@args)
                               until (> spread 1) do (pop it))))))
            (do-side svwap car (/ (abs (price best)) quotient) svwap bfee)
            (do-side bvwap cdr bvwap (/ (abs (price best)) quotient) 0 afee)))))
    (with-slots (bids asks frequency) ope
      (select ((send bids (car foreigners)))
              ((send asks (cdr foreigners)))
              (t (sleep frequency))))))

(defmethod shared-initialize :after ((ope ope-filter) (slots t) &key gate)
  (with-slots (market fee supplicant) ope
    (if (slot-boundp ope 'fee) (reinitialize-instance fee)
        (setf fee (make-instance 'fee-tracker :market market :gate gate)))
    (if (slot-boundp ope 'supplicant) (reinitialize-instance supplicant)
        (setf supplicant (make-instance 'ope-supplicant :gate gate)))))

(defmethod shared-initialize :around ((ope ope-filter) (slots t) &key)
  (call-next-method)                    ; this is an after-after method...
  (with-slots (fee thread market) ope
    (when (or (not (slot-boundp ope 'thread))
              (eq :terminated (task-status thread)))
      (setf thread
            (pexec (:name (concatenate
                           'string "qdm-preα ope filter for " (name market)))
              (loop (ope-filter-loop ope)))))))

(defmethod reinitialize-instance :after ((ope ope-filter) &key gate)
  (with-slots (fee market supplicant) ope
    (ensure-tracking market)
    (multiple-value-call 'reinitialize-instance fee
                         (if (null market) (values) (values :market market))
                         (if (null gate) (values) (values :gate gate)))
    (multiple-value-call 'reinitialize-instance supplicant
                         (if (null gate) (values) (values :gate gate)))))

;;; receives target bids and asks in the next-bids and next-asks channels
;;; sends commands in the control channel through #'ope-place
;;; sends completion acknowledgement to response channel
(defun ope-prioritizer-loop (ope)
  (with-slots (next-bids next-asks response frequency) ope
    (flet ((place (new) (ope-place ope new))
           (amount-change (old new &aux (old-vol (volume old)))
             (/ (abs (- (volume new) old-vol)) old-vol)))
      (flet ((update (char target placed &aux percents cutoff)
               ;; (dolist (o target (force-output))
               ;;   (format t "~&~5@$ @ ~D" (volume o) (price o)))
               (write-char char)
               (dolist (old placed (setf cutoff (third (sort percents #'>))))
                 (awhen (find (price old) target :key #'price :test #'=)
                   (push (amount-change old it) percents)))
               (dolist (old placed (mapcar #'place target))
                 (aif (aand1 (find (price old) target
                                   :key #'price :test #'=)
                             (< (amount-change old it) (or cutoff 0)))
                      (setf target (remove it target))
                      (dolist (new (remove (price old) target
                                           :key #'price :test #'<)
                               (ope-cancel ope old))
                        (if (place new) (setf target (remove new target))
                            (return (ope-cancel ope old))))))
               (send response t)))
        (multiple-value-bind (placed-bids placed-asks) (ope-placed ope)
          (select
            ((recv next-bids to-bid) (update #\b to-bid placed-bids))
            ((recv next-asks to-ask) (update #\a to-ask placed-asks))
            (t (sleep frequency))))))))

(defun profit-margin (bid ask &optional (bid-fee 0) (ask-fee 0))
  (if (= bid-fee ask-fee 0) (/ ask bid)
      (/ (* ask (- 1 (/ ask-fee 100)))
         (* bid (+ 1 (/ bid-fee 100))))))

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

(defun dumbot-offers (foreign-offers   ; w/ope-filter to avoid feedback
                      resilience       ; scalar•asset target offer depth to fill
                      funds            ; scalar•asset target total offer volume
                      epsilon          ; scalar•asset size of smallest order
                      max-orders       ; target amount of offers
                      magic            ; if you have to ask, you'll never know
                      &aux (acc 0) (share 0))
  (do* ((remaining-offers foreign-offers (rest remaining-offers))
        (processed-tally         0       (1+   processed-tally)))
       ((or (null remaining-offers)  ; EITHER: processed entire order book
            (and (> acc resilience)  ;     OR: (   BOTH: processed past resilience
                 (> processed-tally max-orders))) ; AND: processed enough orders )
        (flet ((pick (count offers)
                 (sort (subseq (sort (subseq offers 0 (1- processed-tally))
                                     #'> :key (lambda (x) (volume (cdr x))))
                               0 count) #'< :key (lambda (x) (price (cdr x)))))
               (offer-scaler (total bonus count)
                 (lambda (order)
                   (with-slots (market price) (cdr order)
                     (make-instance 'offer :market market :price (1- price)
                                    :volume (* funds (/ (+ bonus (car order))
                                                        (+ total (* bonus count)))))))))
          (let* ((target-count (min (floor (/ funds epsilon 4/3)) ; ygni! wut?
                                    max-orders processed-tally))
                 (chosen-stairs         ; the (shares . foreign-offer)s to fight
                  (if (>= magic target-count) (pick target-count foreign-offers)
                      (cons (first foreign-offers)
                            (pick (1- target-count) (rest foreign-offers)))))
                 (total-shares (reduce #'+ (mapcar #'car chosen-stairs)))
                 ;; we need the smallest order to be epsilon
                 (e/f (/ epsilon funds))
                 (bonus (if (>= 1 target-count) 0
                            (/ (- (* e/f total-shares) (caar chosen-stairs))
                               (- 1 (* e/f target-count))))))
            (break-errors (not division-by-zero) ; dbz = no funds left, no biggie
              (mapcar (offer-scaler total-shares bonus target-count)
                      chosen-stairs)))))
    ;; TODO - no side effects
    ;; TODO - use a callback for liquidity distribution control
    (with-slots (volume) (first remaining-offers)
      (push (incf share (* 4/3 (incf acc volume))) (first remaining-offers)))))

(defun ope-scalper-loop (ope)
  (with-slots (input output filter prioritizer epsilon count magic) ope
    (destructuring-bind (primary counter resilience ratio) (recv input)
      ;; Now run that algorithm thingy
      (with-slots (next-bids next-asks response) prioritizer
        ;; TODO: Refactor prioritizer API into send-side and recv-side
        ;; TODO: Rework flow so both sides are updated on each book query...
        ;; TODO: using two prioritizers (bid / ask), and an alternator?
        ;; TODO: properly deal with partial and completed orders
        (macrolet ((do-side (amount side chan epsilon)
                     `(let ((,side (recv (slot-value filter ',side))))
                        (unless (zerop ,amount)
                          (send ,chan
                                (dumbot-offers ,side resilience ,amount
                                               ,epsilon (/ count 2) magic))
                          (recv response)))))
          (do-side counter bids next-bids
                   (* epsilon (abs (price (first bids))) (max ratio 1)
                      (expt 10 (- (decimals (market (first bids)))))))
          (do-side primary asks next-asks (* epsilon (max (/ ratio) 1))))))
    (send output nil)))

(defmethod shared-initialize :after ((prioritizer ope-prioritizer) (slots t) &key)
  (with-slots (thread) prioritizer
    (when (or (not (slot-boundp prioritizer 'thread))
              (eq :terminated (task-status thread)))
      (setf thread (pexec (:name "qdm-preα ope prioritizer")
                     (loop (ope-prioritizer-loop prioritizer)))))))

(defmethod shared-initialize :after
    ((ope ope-scalper) (slots t) &key gate market balance-tracker lictor)
  (with-slots (filter prioritizer supplicant) ope
    (unless (slot-boundp ope 'supplicant)
      (setf supplicant (multiple-value-call 'make-instance
                         'ope-supplicant :gate gate :placed (placed-offers gate)
                         (if (null balance-tracker) (values)
                             (values :balance-tracker balance-tracker)))))
    (if (slot-boundp ope 'prioritizer)
        (reinitialize-instance prioritizer    :supplicant supplicant)
        (setf prioritizer
              (make-instance 'ope-prioritizer :supplicant supplicant)))
    (if (slot-boundp ope 'filter) (reinitialize-instance filter)
        (setf filter (make-instance 'ope-filter :market market
                                    :lictor lictor :gate gate
                                    :supplicant supplicant)))))

(defmethod shared-initialize :around ((ope ope-scalper) (slots t) &key)
  (call-next-method)                    ; another after-after method...
  (with-slots (thread) ope
    (when (or (not (slot-boundp ope 'thread))
              (eq :terminated (task-status thread)))
      (setf thread (pexec (:name "qdm-preα ope scalper")
                     (loop (ope-scalper-loop ope)))))))

;;;
;;; ACCOUNT TRACKING
;;;

(defclass account-tracker ()
  ((gate :initarg :gate)
   (treasurer :initarg :treasurer)
   (ope :initarg :ope)
   (lictor :initarg :lictor)))

(defclass balance-tracker ()
  ((balances :initarg :balances :initform nil)
   (input :initform (make-instance 'channel))
   (output :initform (make-instance 'channel))
   (control :initform (make-instance 'channel))
   (gate :initarg :gate)
   updater worker))

(defun balance-worker-loop (tracker)
  (with-slots (balances input output) tracker
    (let ((command (recv input)))
      (typecase command
        (string (send output (or (cdr (assoc command balances
                                             :test #'string=))
                                 0)))
        (cons (destructuring-bind (slot-name . value) command
                (setf (slot-value tracker slot-name) value)))))))

(defgeneric asset-balance (tracker asset)
  (:method ((tracker account-tracker) asset)
    (asset-balance (slot-value tracker 'treasurer) asset))
  (:method ((tracker balance-tracker) asset)
    (with-slots (input output) tracker
      (send input (name asset))
      (recv output))))

(defun balance-updater-loop (tracker)
  (with-slots (gate control input) tracker
    (recv control)
    (send control (ignore-errors (aprog1 (account-balances gate)
                                   (send input `(balances . ,it)))))))

(defmethod vwap ((tracker account-tracker) &key type depth net)
  (vwap (slot-value tracker 'lictor) :type type :depth depth :net net))

(defmethod shared-initialize :after ((tracker balance-tracker) (names t) &key)
  (with-slots (updater worker) tracker
    (when (or (not (slot-boundp tracker 'updater))
              (eq :terminated (task-status updater)))
      (setf updater
            (pexec (:name "qdm-preα balance updater" :initial-bindings
                          `((*read-default-float-format* double-float)))
              (loop (balance-updater-loop tracker)))))
    (when (or (not (slot-boundp tracker 'worker))
              (eq :terminated (task-status worker)))
      (setf worker (pexec (:name "qdm-preα balance worker")
                     ;; TODO: just pexec anew each time...
                     ;; you'll understand what you meant someday, right?
                     (loop (balance-worker-loop tracker)))))))

(defmethod shared-initialize :after
    ((tracker account-tracker) (names t) &key market)
  (with-slots (lictor treasurer gate ope) tracker
    (if (slot-boundp tracker 'lictor)
        (reinitialize-instance lictor :market market)
        (setf lictor (make-instance 'execution-tracker :market market :gate gate)))
    (if (slot-boundp tracker 'treasurer) (reinitialize-instance treasurer)
        (setf treasurer (make-instance 'balance-tracker :gate gate)))
    (unless (slot-boundp tracker 'ope)
      (setf ope (make-instance 'ope-scalper :lictor lictor :gate gate
                               :balance-tracker treasurer :market market)))))

(defun gapps-rate (from to)
  (getjso "rate" (read-json (drakma:http-request
                             "http://rate-exchange.appspot.com/currency"
                             :parameters `(("from" . ,from) ("to" . ,to))
                             :want-stream t))))

(defclass maker ()
  ((market :initarg :market :reader market)
   (fund-factor :initarg :fund-factor :initform 1)
   (resilience-factor :initarg :resilience :initform 1)
   (targeting-factor :initarg :targeting :initform 3/5)
   (control :initform (make-instance 'channel))
   (account-tracker :initarg :account-tracker)
   (name :initarg :name :accessor name)
   (report-depths :initform '(nil 4 1 1/4) :initarg :report-depths)
   thread))

(defun makereport (maker fund rate btc doge investment risked skew)
  (when (zerop (mod (sec-of (now)) 3))
    (print-book (slot-reduce maker account-tracker ope)))
  (with-slots (name market account-tracker report-depths) maker
    (labels ((sastr (side amount &optional model) ; TODO factor out aqstr
               (format nil "~V,,V$" (decimals (slot-value market side))
                       (if model (length (sastr side model)) 0) amount))
             (depth-profit (&optional depth)
               (flet ((vwap (side) (vwap account-tracker :type side :net t
                                         :market market :depth depth)))
                 (dbz-guard (* 100 (1- (profit-margin (vwap "buy")
                                                      (vwap "sell"))))))))
      ;; FIXME: modularize all this decimal point handling
      ;; we need a pprint-style ~/aq/ function, and pass it aq objects!
      ;; time, total, primary, counter, invested, risked, risk bias, pulse
      (format t "~&~A ~A~{ ~A~} ~2,2$% ~2,2$% ~2,2@$~{ ~6@$~}~%"
              name (subseq (princ-to-string (now)) 11 19)
              (mapcar #'sastr '(primary counter primary counter)
                      `(,@#1=`(,fund ,(* fund rate)) ,btc ,doge) `(() () ,@#1#))
              (* 100 investment) (* 100 risked) (* 100 skew)
              (maplist (lambda (depths)
                         (apply #'depth-profit
                                (sctypecase (first depths)
                                  (null) (number `(,(* fund it))))))
                       report-depths))))
  (force-output))

(defun %round (maker)
  (with-slots (fund-factor resilience-factor targeting-factor
               market name account-tracker) maker
    ;; Get our balances
    (let* ((trades (recv (slot-reduce market trades-tracker output)))
           ;; TODO: split into primary resilience and counter resilience
           (resilience (* resilience-factor (reduce #'max (mapcar #'volume trades))))
           ;; TODO: doge is cute but let's move on
           (doge/btc (vwap (slot-reduce market trades-tracker) :depth 50 :type :buy)))
      (with-slots (control) (slot-value account-tracker 'treasurer)
        (recv (send control nil)))      ; beautiful!
      (flet ((symbol-funds (symbol) (asset-balance account-tracker symbol))
             (total-of (btc doge) (+ btc (/ doge doge/btc)))
             (factor-fund (fund factor) (* fund fund-factor factor)))
        (let* ((total-btc (symbol-funds (slot-value market 'primary)))
               (total-doge (symbol-funds (slot-value market 'counter)))
               (total-fund (total-of total-btc total-doge))
               (investment (dbz-guard (/ total-btc total-fund)))
               (btc (factor-fund total-btc (* investment targeting-factor)))
               (doge (factor-fund total-doge (- 1 (* investment targeting-factor)))))
          ;; report funding
          (makereport maker total-fund doge/btc total-btc total-doge investment
                      (dbz-guard (/ (total-of    btc  doge) total-fund))
                      (dbz-guard (/ (total-of (- btc) doge) total-fund)))
          (with-slots (input output) (slot-value account-tracker 'ope)
            (send input (list btc doge resilience (/ 1/2 investment))) ; MAGIC
            (recv output)))))))

(defun dumbot-loop (maker)
  (with-slots (control) maker
    (select
      ((recv control command)
       ;; commands are (cons command args)
       (case (car command)
         ;; pause - wait for any other command to restart
         (pause (recv control))
         (stream (setf *standard-output* (cdr command)))))
      (t (%round maker)))))

(defmethod shared-initialize :after ((maker maker) (names t) &key gate)
  (with-slots (market account-tracker thread) maker
    (ensure-tracking market)
    (if (slot-boundp maker 'account-tracker)
        (reinitialize-instance account-tracker :gate gate :market market)
        (setf account-tracker
              (make-instance  'account-tracker :gate gate :market market)))
    (when (or (not (slot-boundp maker 'thread))
              (eq :terminated (task-status thread)))
      (setf thread
            (pexec
                (:name (concatenate 'string "qdm-preα " (name market))
                 :initial-bindings `((*read-default-float-format* double-float)))
              ;; TODO: just pexec anew each time...
              ;; you'll understand what you meant someday, right?
              (loop (dumbot-loop maker)))))))

(defun pause-maker (maker) (send (slot-value maker 'control) '(pause)))

(defun reset-the-net (maker &key (revive t) (delay 5))
  (mapc 'kill (mapcar 'task-thread (pooled-tasks)))
  #+ (or)
  (flet ((ensure-death (list)
           (let ((thread (reduce #'slot-value list :initial-value maker)))
             (tagbody
                (if (eq :terminated (task-status thread)) (go end)
                    (kill (task-thread thread)))
              loop
                (if (eq :terminated (task-status thread)) (go end) (go loop))
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
            (fee-tracker thread))))
  #+sbcl (sb-ext:gc :full t)
  (when revive
    (dolist (actor
              (list (slot-reduce maker market)
                    (slot-reduce maker account-tracker gate)
                    (slot-reduce maker account-tracker treasurer)
                    (slot-reduce maker account-tracker ope filter lictor)
                    (slot-reduce maker account-tracker ope)
                    maker))
      (sleep delay)
      (reinitialize-instance actor))))

(defmacro define-maker (name &rest keys
                        &key market gate
                          ;; just for interactive convenience
                          fund-factor targeting resilience account-tracker)
  (declare (ignore fund-factor targeting resilience account-tracker))
  (dolist (key '(:market :gate)) (remf keys key))
  `(defvar ,name (make-instance 'maker :market ,market :gate ,gate
                                :name ,(string-trim "*+<>" name)
                                ,@keys)))

#+nil
(define-maker *maker*
    :market (find-market "XXBTZEUR" kraken:*kraken*)
    :gate (make-instance 'kraken:kraken-gate
                         :pubkey #P "secrets/some.pubkey"
                         :secret #P "secrets/some.secret"))

(defun current-depth (maker)
  (with-slots (resilience-factor market) maker
    (with-slots (trades) (slot-value market 'trades-tracker)
      (* resilience-factor (reduce #'max (mapcar #'volume trades))))))

(defun performance-overview (maker)
  (with-slots (account-tracker market) maker
    (flet ((funds (symbol) (asset-balance account-tracker symbol))
           (total (btc doge)
             (+ btc (/ doge (vwap market :depth 50 :type :buy))))
           (vwap (side &optional d)
             (vwap account-tracker :type side :net t :market market :depth d)))
      (let* ((trades (slot-reduce account-tracker ope filter lictor trades))
             (uptime (timestamp-difference (now) (timestamp (first (last trades)))))
             (updays (/ uptime 60 60 24))
             (volume (reduce #'+ (mapcar #'volume trades)))
             (profit (* volume (1- (profit-margin (vwap "buy") (vwap "sell")))))
             (total (total (funds (primary market)) (funds (counter market)))))
        (format t "~&Been up              ~7@F days,~
                   ~%traded               ~7@F coins,~
                   ~%profit               ~7@F coins,~
                   ~%portfolio flip per   ~7@F days,~
                   ~%estd monthly profit: ~5@$ percent~%"
                updays volume profit (/ (* total updays 2) volume)
                (/ (* 100 profit) (/ updays 30) total))))))

(defgeneric print-book (book &key count)
  (:method ((maker maker) &rest keys)
    (macrolet ((path (&rest path)
                 `(apply #'print-book (slot-reduce maker ,@path) keys)))
      ;; TODO: interleaving
      (path account-tracker ope)
      (path market book-tracker)))
  (:method ((ope ope-scalper) &rest keys)
    (apply #'print-book (multiple-value-call 'cons (ope-placed ope)) keys))
  (:method ((tracker book-tracker) &rest keys)
    (apply #'print-book     (recv   (slot-value tracker 'output))    keys))
  (:method ((book cons) &key count)
    (destructuring-bind (bids . asks) book
      (flet ((width (side)
               (reduce 'max (mapcar 'length (mapcar 'princ-to-string side))
                       :initial-value 0)))
        (do ((bids bids (rest bids)) (bw (width bids))
             (asks asks (rest asks)) (aw (width asks)))
            ((or (and (null bids) (null asks))
                 (and (numberp count) (= -1 (decf count)))))
          (format t "~&~V@A || ~V@A~%" bw (first bids) aw (first asks)))))))
