;;;; qd.lisp

(defpackage #:scalpl.qd
  (:use #:cl #:chanl #:anaphora #:local-time #:scalpl.util #:scalpl.exchange))

(in-package #:scalpl.qd)

;;;
;;;  ENGINE
;;;

(defclass ope-supplicant ()
  ((gate :initarg :gate)
   (placed :initform nil :initarg :placed)
   (control :initarg :control :initform (make-instance 'channel))
   (response :initarg :response :initform (make-instance 'channel))
   (balance-tracker :initarg :balance-tracker)
   (order-slots :initform 40 :initarg :order-slots)
   thread))

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

;;; receives messages in the control channel, outputs from the gate
(defun ope-supplicant-loop (ope)
  (with-slots (gate control response placed) ope
    (let ((command (recv control)))
      (destructuring-bind (car . cdr) command
        (send response (case car
                         (offer (balance-guarded-place ope cdr))
                         (cancel (awhen1 (cancel-offer gate cdr)
                                   (setf placed (remove cdr placed))))))))))

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
(defun ope-place (ope offer)
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
  ((bids       :initarg :bids       :initform ())
   (asks       :initarg :asks       :initform ())
   (market     :initarg :market     :initform (error "must link market"))
   (supplicant :initarg :supplicant :initform (error "must link supplicant"))
   (frequency  :initarg :frequency  :initform 1/7) ; TODO: push depth deltas
   (lictor     :initarg :lictor     :initform (error "must link lictor"))
   (rudder     :initarg :rudder     :initform '(() . ()))
   (book-cache :initform nil)
   fee foreigners thread))

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

;;; needs to do three different things
;;; 1) ignore-offers - fishes offers from linked supplicant
;;; 2) profitable spread - already does (via ecase spaghetti)
;;; 3) profit vs recent cost basis - done, shittily - TODO parametrize depth

(defun ope-filter-loop (ope)
  (with-slots (market foreigners book-cache bids asks rudder frequency) ope
    (flet ((xyz (asset rside type &aux (lictor (slot-value ope 'lictor))
                       (rval (if (consp rudder) (funcall rside rudder) rudder)))
             (aif (getf (slot-reduce lictor bases) asset)
                  (scaled-price
                   (nth-value 1 (bases-without it (cons-aq* asset rval))))
                  (vwap lictor :type type :depth rval))))
      (destructuring-bind (bfee . afee) (recv (slot-reduce ope fee output))
        (let ((book (recv (slot-reduce market book-tracker output))))
          (unless (equal book book-cache)
            (with-slots (placed) (slot-value ope 'supplicant)
              (setf foreigners (cons (ignore-offers (car book) placed)
                                     (ignore-offers (cdr book) placed))
                    book-cache book))
            (let ((quotient (expt 10 (decimals market)))
                  (svwap (xyz (counter market) #'car "sell"))
                  (bvwap (xyz (primary market) #'cdr "buy")))
              (macrolet ((do-side (vwap side &body args)
                           `(unless (zerop ,vwap)
                              (swhen (,side foreigners)
                                (loop for best = (first it) while best
                                   for spread = (profit-margin ,@args)
                                   until (> spread 1) do (pop it))))))
                (do-side svwap car (/ (price best) quotient) svwap bfee)
                (do-side bvwap cdr bvwap (/ (price best) quotient) 0 afee)))
            (setf bids (car foreigners) asks (cdr foreigners))))
        (sleep frequency)))))

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

(defclass ope-prioritizer ()
  ((next-bids :initform (make-instance 'channel))
   (next-asks :initform (make-instance 'channel))
   (response :initform (make-instance 'channel))
   (supplicant :initarg :supplicant) thread
   (frequency :initarg :frequency :initform 1/7)))

(defun prioriteaze (ope target placed &aux to-add (excess placed))
  (flet ((place (new) (ope-place (slot-value ope 'supplicant) new)))
    (macrolet ((frob (add pop)
                 `(let* ((n (max (length ,add) (length ,pop)))
                         (m (- n (ceiling (log (1+ (random (1- (exp n)))))))))
                    (macrolet ((wrap (a . b) `(awhen (nth m ,a) (,@b it))))
                      (wrap ,pop ope-cancel ope) (wrap ,add place)))))
      (aif (dolist (new target (sort to-add #'< :key #'price))
             (aif (find (price new) excess :key #'price :test #'=)
                  (setf excess (remove it excess)) (push new to-add)))
           (frob it excess) (if excess (frob nil excess) ; yuck
                                (and target placed (frob target placed)))))))

;;; receives target bids and asks in the next-bids and next-asks channels
;;; sends commands in the control channel through #'ope-place
;;; sends completion acknowledgement to response channel
(defun ope-prioritizer-loop (ope)
  (with-slots (next-bids next-asks response frequency) ope
    (multiple-value-bind (next source)
        (recv (list next-bids next-asks) :blockp nil)
      (multiple-value-bind (placed-bids placed-asks) (ope-placed ope)
        (if (null source) (sleep frequency)
            ((lambda (side) (send response (prioriteaze ope next side)))
             (if (eq source next-bids) placed-bids placed-asks)))))))

(defun profit-margin (bid ask &optional (bid-fee 0) (ask-fee 0))
  (abs (if (= bid-fee ask-fee 0) (/ ask bid)
           (/ (* ask (- 1 (/ ask-fee 100)))
              (* bid (+ 1 (/ bid-fee 100)))))))

(defun dumbot-offers (foreigners       ; w/ope-filter to avoid feedback
                      resilience       ; scalar•asset target offer depth to fill
                      funds            ; scalar•asset target total offer volume
                      epsilon          ; scalar•asset size of smallest order
                      max-orders       ; target amount of offers
                      magic            ; if you have to ask, you'll never know
                      &aux (acc 0) (share 0) (others (copy-list foreigners))
                        (asset (consumed-asset (first others))))
  (do* ((remaining-offers others (rest remaining-offers))
        (processed-tally    0    (1+   processed-tally)))
       ((or (null remaining-offers)  ; EITHER: processed entire order book
            (and (> acc resilience)  ;     OR: (   BOTH: processed past resilience
                 (> processed-tally max-orders))) ; AND: processed enough orders )
        (flet ((pick (count offers)
                 (sort (subseq* (sort (or (subseq offers 0 (1- processed-tally))
                                          (warn "~&FIXME: GO DEEPER!~%") offers)
                                      #'> :key (lambda (x) (volume (cdr x))))
                               0 count) #'< :key (lambda (x) (price (cdr x)))))
               (offer-scaler (total bonus count)
                 (lambda (order &aux (vol (* funds (/ (+ bonus (car order))
                                                      (+ total (* bonus count))))))
                   (with-slots (market price) (cdr order)
                     (make-instance 'offer :market market :price (1- price)
                                    :volume vol :given (cons-aq* asset vol))))))
          (let* ((target-count (min (floor (/ funds epsilon 4/3)) ; ygni! wut?
                                    max-orders processed-tally))
                 (chosen-stairs         ; the (shares . foreign-offer)s to fight
                  (if (>= magic target-count) (pick target-count others)
                      (cons (first others) (pick (1- target-count) (rest others)))))
                 (total-shares (reduce #'+ (mapcar #'car chosen-stairs)))
                 ;; we need the smallest order to be epsilon
                 (e/f (/ epsilon funds))
                 (bonus (if (>= 1 target-count) 0
                            (/ (- (* e/f total-shares) (caar chosen-stairs))
                               (- 1 (* e/f target-count))))))
            (break-errors (not division-by-zero) ; dbz = no funds left, no biggie
              (mapcar (offer-scaler total-shares bonus target-count)
                      chosen-stairs)))))
    ;; TODO - use a callback for liquidity distribution control
    (with-slots (volume) (first remaining-offers)
      (push (incf share (* 4/3 (incf acc volume))) (first remaining-offers)))))

(defclass ope-scalper ()
  ((input :initform (make-instance 'channel))
   (output :initform (make-instance 'channel))
   (supplicant :initarg :supplicant)
   (filter :initarg :filter)
   (epsilon :initform 0.001 :initarg :epsilon)
   (count :initform 30 :initarg :offer-count)
   (magic :initform 3 :initarg :magic-count)
   (cut :initform 0 :initarg :cut)
   (spam :initform nil :initarg :spam)
   prioritizer thread))

(defun ope-sprinner (offers funds count magic bases punk dunk book)
  (if (or (null bases) (zerop count) (null offers)) offers
      (destructuring-bind (top . offers) offers
        (multiple-value-bind (bases vwab cost)
            ;; what appears to be the officer, problem?
            ;; (bases-without bases (given top)) fails, because bids are `viqc'
            (bases-without bases (cons-aq* (consumed-asset top) (volume top)))
          (flet ((profit (o) (funcall punk (1- (price o)) (price vwab))))
            (signal "~4,2@$ ~A ~D ~V$ ~V$" (profit top) top (length bases)
                    (decimals (market vwab)) (scaled-price vwab)
                    (decimals (asset cost)) (scaled-quantity cost))
            (let ((book (rest (member 0 book :test #'< :key #'profit))))
              (if (plusp (profit top))
                  `(,top ,@(ope-sprinner offers (- funds (volume top))
                                         (1- count) magic bases punk dunk book))
                  (ope-sprinner (funcall dunk book funds count magic) funds
                                count magic `((,vwab ,(aq* vwab cost) ,cost)
                                              ,@bases) punk dunk book))))))))

(defun ope-logger (ope)
  (lambda (log) (awhen (slot-value ope 'spam) (format t "~&~A ~A~%" it log))))

(defun ope-spreader (book resilience funds epsilon side ope)
  (flet ((dunk (book funds count magic)
           (and book (dumbot-offers book resilience funds epsilon count magic)))
         (punk (side cut fees)
           (macrolet ((punk (&rest args)
                        `(lambda (price vwab)
                           (- (* 100 (1- (profit-margin ,@args))) cut))))
             (if (eq side 'bids) (punk price vwab (car fees))
                 (punk vwab price 0 (cdr fees))))))
    (with-slots (count magic cut) ope
      (awhen (dunk book funds (/ count 2) magic)
        (ope-sprinner it funds (/ count 2) magic
                      (getf (slot-reduce ope filter lictor bases)
                            (asset (given (first it))))
                      (punk side cut (recv (slot-reduce ope filter fee output)))
                      #'dunk book)))))

(defun ope-scalper-loop (ope)
  (with-slots (input output filter prioritizer epsilon) ope
    (destructuring-bind (primary counter resilience ratio) (recv input)
      (with-slots (next-bids next-asks response) prioritizer
        (macrolet ((do-side (amount side chan epsilon)
                     `(let ((,side (copy-list (slot-value filter ',side))))
                        (unless (or (zerop ,amount) (null ,side))
                          (send ,chan (handler-bind
                                          ((simple-condition (ope-logger ope)))
                                        (ope-spreader ,side resilience ,amount
                                                      ,epsilon ',side ope)))
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
   (gate :initarg :gate) (fuzz :initarg :fuzz :initform (random 7))
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
  (with-slots (gate control input fuzz) tracker
    (send (nth-value 1 (recv control))
          (ignore-errors (when (zerop (random fuzz))
                           (awhen1 (account-balances gate)
                             (send input `(balances . ,it))))))))

(defmethod vwap ((tracker account-tracker) &rest keys)
  (apply #'vwap (slot-value tracker 'lictor) keys))

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

(defclass maker ()
  ((market :initarg :market :reader market)
   (fund-factor :initarg :fund-factor :initform 1)
   (resilience-factor :initarg :resilience :initform 1)
   (targeting-factor :initarg :targeting :initform (random 1.0))
   (skew-factor :initarg :skew-factor :initform 1)
   (control :initform (make-instance 'channel))
   (account-tracker :initarg :account-tracker)
   (name :initarg :name :accessor name)
   (report-depths :initform (list nil 4 1 1/4) :initarg :report-depths)
   (last-report :initform nil)
   thread))

(defmethod print-object ((maker maker) stream)
  (print-unreadable-object (maker stream :type t :identity nil)
    (write-string (name maker) stream)))

(defun makereport (maker fund rate btc doge investment risked skew)
  (with-slots (name market account-tracker report-depths last-report) maker
    (let ((new-report (list fund rate btc doge investment risked skew)))
      (if (equal last-report new-report) (return-from makereport)
          (setf last-report new-report)))
    (labels ((sastr (side amount &optional model) ; TODO factor out aqstr
               (format nil "~V,,V$" (decimals (slot-value market side))
                       (if model (length (sastr side model)) 0) amount))
             (depth-profit (&optional depth)
               (flet ((vwap (side) (vwap account-tracker :type side
                                         :market market :depth depth)))
                 (dbz-guard (* 100 (1- (profit-margin (vwap "buy")
                                                      (vwap "sell"))))))))
      ;; FIXME: modularize all this decimal point handling
      ;; we need a pprint-style ~/aq/ function, and pass it aq objects!
      ;; time, total, primary, counter, invested, risked, risk bias, pulse
      (format t "~&~A ~A~{ ~A~} ~2,2$% ~2,2$% ~2,2@$~{ ~4@$~}~%"
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
  (with-slots (fund-factor resilience-factor targeting-factor skew-factor
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
             (total-of (btc doge) (+ btc (/ doge doge/btc))))
        (let* ((total-btc (symbol-funds (slot-value market 'primary)))
               (total-doge (symbol-funds (slot-value market 'counter)))
               (total-fund (total-of total-btc total-doge)))
          ;; history, yo!
          ;; this test originated in a harried attempt at bugfixing an instance
          ;; of Maybe, where the treasurer reports zero balances when the http
          ;; request (checking for balance changes) fails; due to use of aprog1
          ;; when the Right Thing™ is awhen1. now that the bug's killed better,
          ;; Maybe thru recognition, the test remains; for when you lose the bug
          ;; don't lose the lesson, nor the joke.
          (unless (zerop total-fund)
            (let* ((investment (dbz-guard (/ total-btc total-fund)))
                   (btc  (* fund-factor total-btc investment targeting-factor))
                   (doge (* fund-factor total-doge
                            (- 1 (* investment targeting-factor)))))
              ;; report funding
              (makereport maker total-fund doge/btc total-btc total-doge investment
                          (dbz-guard (/ (total-of    btc  doge) total-fund))
                          (dbz-guard (/ (total-of (- btc) doge) total-fund)))
              (send (slot-reduce account-tracker ope input)
                    (list btc doge resilience
                          (expt (/ doge btc doge/btc) skew-factor)))
              (recv (slot-reduce account-tracker ope output)))))))))

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
        (reinitialize-instance account-tracker :market market)
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

(defun trades-profits (trades)
  (flet ((side-sum (side asset)
           (reduce #'aq+ (mapcar asset (remove side trades :key #'direction
                                               :test-not #'string-equal)))))
    (let ((aq1 (aq- (side-sum "buy"  #'taken) (side-sum "sell" #'given)))
          (aq2 (aq- (side-sum "sell" #'taken) (side-sum "buy"  #'given))))
      (ecase (- (signum (quantity aq1)) (signum (quantity aq2)))
        (0 (values nil aq1 aq2))
        (-2 (values (aq/ (- (conjugate aq1)) aq2) aq2 aq1))
        (+2 (values (aq/ (- (conjugate aq2)) aq1) aq1 aq2))))))

(defun performance-overview (maker &optional depth)
  (with-slots (account-tracker market) maker
    (flet ((funds (symbol) (asset-balance account-tracker symbol))
           (total (btc doge)
             (+ btc (/ doge (vwap market :depth 50 :type :buy))))
           (vwap (side)
             (vwap account-tracker :type side :market market :depth depth)))
      (let* ((trades (slot-reduce account-tracker ope filter lictor trades))
             (uptime (timestamp-difference (now) (timestamp (first (last trades)))))
             (updays (/ uptime 60 60 24))
             (volume (or depth (reduce #'+ (mapcar #'volume trades))))
             (profit (* volume (1- (profit-margin (vwap "buy") (vwap "sell"))) 1/2))
             (total (total (funds (primary market)) (funds (counter market)))))
        (format t "~&Been up              ~7@F days,~
                   ~%traded               ~7@F coins,~
                   ~%profit               ~7@F coins,~
                   ~%portfolio flip per   ~7@F days,~
                   ~%avg daily profit:    ~4@$%~
                   ~%estd monthly profit: ~4@$%~%"
                updays volume profit (/ (* total updays 2) volume)
                (/ (* 100 profit) updays total) ; ignores compounding, too high!
                (/ (* 100 profit) (/ updays 30) total))))))

(defgeneric print-book (book &key count prefix)
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
  (:method ((book cons) &key count prefix)
    (destructuring-bind (bids . asks) book
      (flet ((width (side)
               (reduce 'max (mapcar 'length (mapcar 'princ-to-string side))
                       :initial-value 0)))
        (do ((bids bids (rest bids)) (bw (width bids))
             (asks asks (rest asks)) (aw (width asks)))
            ((or (and (null bids) (null asks))
                 (and (numberp count) (= -1 (decf count)))))
          (format t "~&~@[~A ~]~V@A || ~V@A~%"
                  prefix bw (first bids) aw (first asks)))))))
