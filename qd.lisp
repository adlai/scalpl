;;;; qd.lisp

(defpackage #:scalpl.qd
  (:use #:cl #:anaphora #:st-json #:local-time #:scalpl.util #:scalpl.exchange))

(in-package #:scalpl.qd)

;;;
;;;  ENGINE
;;;

(defclass ope ()
  ((input :initform (make-instance 'channel))
   (output :initform (make-instance 'channel))
   (book-channel :initarg :book-channel)
   (supplicant :initarg :supplicant)
   prioritizer scalper))

(defclass ope-supplicant ()
  ((gate :initarg :gate)
   (placed :initform nil :initarg :placed)
   (control :initarg :control :initform (make-instance 'channel))
   (response :initarg :response :initform (make-instance 'channel))
   (balance-tracker :initarg :balance-tracker)
   thread))

(defclass ope-prioritizer ()
  ((next-bids :initform (make-instance 'channel))
   (next-asks :initform (make-instance 'channel))
   (prioritizer-response :initform (make-instance 'channel))
   (supplicant :initarg :supplicant) thread))

(defun offers-spending (ope asset)
  (remove asset (slot-value ope 'placed)
          :key #'consumed-asset :test-not #'eq))

(defun balance-guarded-place (ope offer)
  (with-slots (gate placed balance-tracker) ope
    (let ((asset (consumed-asset offer)))
      (when (>= (asset-balance balance-tracker asset)
                (reduce #'+ (mapcar #'volume (offers-spending ope asset))
                        :initial-value (volume offer)))
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
                                   (setf placed (remove cdr placed))))))))))

(defmethod shared-initialize :after ((supplicant ope-supplicant) slots &key)
  (with-slots (thread) supplicant
    (when (or (not (slot-boundp supplicant 'thread))
              (eq :terminated (task-status thread)))
      (setf thread (pexec (:name "qdm-preα ope supplicant")
                     (loop (ope-supplicant-loop supplicant)))))))

(defun ope-placed (ope)
  (with-slots (control response) (slot-value ope 'supplicant)
    (send control '(placed))
    (let ((all (sort (copy-list (recv response)) #'< :key #'price)))
      (flet ((split (sign)
               (remove sign all :key (lambda (x) (signum (price x))))))
        ;;       bids       asks
        (values (split 1) (split -1))))))

;;; response: placed offer if successful, nil if not
(defgeneric ope-place (ope offer))
(defmethod ope-place ((ope ope) offer)
  (ope-place (slot-value ope 'supplicant) offer))
(defmethod ope-place ((ope ope-prioritizer) offer)
  (ope-place (slot-value ope 'supplicant) offer))
(defmethod ope-place ((ope ope-supplicant) offer)
  (with-slots (control response) ope
    (send control (cons 'offer offer)) (recv response)))

;;; response: {count: "1"} if successful, nil if not
(defun ope-cancel (ope offer)
  (with-slots (control response) (slot-value ope 'supplicant)
    (send control (cons 'cancel offer)) (recv response)))

;;; receives target bids and asks in the next-bids and next-asks channels
;;; sends commands in the control channel through #'ope-place
;;; sends completion acknowledgement to prioritizer-response channel
(defun ope-prioritizer-loop (ope)
  (with-slots (next-bids next-asks prioritizer-response) ope
    (flet ((place (new) (ope-place ope new))
           (amount-change (old new &aux (old-vol (volume old)))
             (/ (abs (- (volume new) old-vol)) old-vol)))
      (flet ((update (target placed &aux percents cutoff)
               ;; (dolist (o target (force-output))
               ;;   (format t "~&~5@$ @ ~D" (volume o) (price o)))
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
               (send prioritizer-response t)))
        (multiple-value-bind (placed-bids placed-asks) (ope-placed ope)
          (select
            ((recv next-bids to-bid) (update to-bid placed-bids))
            ((recv next-asks to-ask) (update to-ask placed-asks))))))))

(defun profit-margin (bid ask &optional (fee '(0 . 0)))
  (/ (* ask (- 1 (/ (cdr fee) 100)))
     (* bid (+ 1 (/ (car fee) 100)))))

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
                      &aux (acc 0) (share 0))
  (do* ((remaining-offers foreign-offers (rest remaining-offers))
        (processed-tally         0       (1+   processed-tally)))
       ((or (null remaining-offers)  ; EITHER: processed entire order book
            (and (> acc resilience)  ;     OR: (   BOTH: processed past resilience
                 (> processed-tally max-orders))) ; AND: processed enough orders )
        (let* ((target-stair-count (min (floor (/ funds epsilon 4/3))  ; ygni!
                                        max-orders processed-tally))
               (chosen-stair-set        ; the (shares . foreign-offer)s to fight
                (cons (first foreign-offers)
                      (subseq (sort (subseq foreign-offers 1 processed-tally)
                                    #'> :key (lambda (x) (volume (cdr x))))
                              0 (1- target-stair-count))))
               (total-shares (reduce #'+ (mapcar #'car chosen-stair-set)))
               ;; we need the smallest order to be epsilon
               (e/f (/ epsilon funds))
               (bonus (/ (- (* e/f total-shares) (caar chosen-stair-set))
                         (- 1 (* e/f target-stair-count)))))
          (mapcar (lambda (order)
                    (with-slots (market price) (cdr order)
                      (make-instance 'offer :market market :price (1- price)
                                     :volume (* funds
                                                (/ (+ bonus (car order))
                                                   (+ total-shares
                                                      (* bonus
                                                         target-stair-count)))))))
                  (sort chosen-stair-set #'<
                        :key (lambda (x) (price (cdr x)))))))
    ;; TODO - no side effects
    ;; TODO - use a callback for liquidity distribution control
    (with-slots (volume) (first remaining-offers)
      (push (incf share (* 4/3 (incf acc volume))) (first remaining-offers)))))

(defun ope-scalper-loop (ope)
  (with-slots (input output book-channel prioritizer supplicant) ope
    (destructuring-bind (fee primary counter resilience &optional
                             ;; magic numbers, to be parametrized
                             (order-count 15) (epsilon 0.001))
        (recv input)
      ;; Now run that algorithm thingy
      (flet ((filter-book (book)
               (with-slots (control response) supplicant
                 (send control `(filter . ,book)) (recv response))))
        ;; The entire with-book operation needs to be turned into a separate
        ;; program entity ("actor"?) which receives updated order books, and
        ;; currently-placed offersets, and produces filtered books
        ;; Whether filtered books are pushed or pulled is TBD
        (macrolet ((do-side ((amount) &body body)
                     `(destructuring-bind (market-bids . market-asks)
                          (recv book-channel)
                        (let ((other-bids (filter-book market-bids))
                              (other-asks (filter-book market-asks)))
                          ;; NON STOP PARTY PROFIT MADNESS
                          (do* ((best-bid (- (price (car other-bids)))
                                          (- (price (car other-bids))))
                                (best-ask (price (car other-asks))
                                          (price (car other-asks)))
                                (spread (profit-margin (1+ best-bid) (1- best-ask) fee)
                                        (profit-margin (1+ best-bid) (1- best-ask) fee)))
                               ((> spread 1))
                            (ecase (round (signum (* (max 0 (- best-ask best-bid 10))
                                                     (- (volume (car other-bids))
                                                        (volume (car other-asks))))))
                              (-1 (decf (volume (car other-asks))
                                        (volume (pop other-bids))))
                              (+1 (decf (volume (car other-bids))
                                        (volume (pop other-asks))))
                              (0 (pop other-bids) (pop other-asks))))
                          (unless (zerop ,amount) ,@body)))))
          ;; TODO: Refactor prioritizer API into send-side and recv-side
          ;; TODO: Rework flow so both sides are updated on each book query...
          ;; TODO: using two prioritizers (bid / ask), and an alternator?
          ;; TODO: properly deal with partial and completed orders
          (with-slots (next-bids next-asks prioritizer-response) prioritizer
            (do-side (counter)
              (send next-bids
                    (dumbot-offers other-bids resilience counter
                                   (* epsilon (- (price (first other-bids)))
                                      (expt 10 (- (decimals (market (first other-bids))))))
                                   order-count))
              (recv prioritizer-response))
            (do-side (primary)
              (send next-asks (dumbot-offers other-asks resilience primary
                                             epsilon order-count))
              (recv prioritizer-response))))))
    (send output nil)))

(defmethod shared-initialize :after ((prioritizer ope-prioritizer) slots &key)
  (with-slots (thread) prioritizer
    (when (or (not (slot-boundp prioritizer 'thread))
              (eq :terminated (task-status thread)))
      (setf thread (pexec (:name "qdm-preα ope prioritizer")
                     (loop (ope-prioritizer-loop prioritizer)))))))

(defmethod shared-initialize :after ((ope ope) slots &key gate balance-tracker)
  (with-slots (supplicant prioritizer scalper) ope
    (if (slot-boundp ope 'supplicant)
        (multiple-value-call 'reinitialize-instance supplicant
                             (if (null gate) (values) (values :gate gate))
                             (if (null balance-tracker) (values)
                                 (values :balance-tracker balance-tracker)))
        (setf supplicant
              (multiple-value-call 'make-instance 'ope-supplicant
                                   :placed (placed-offers gate) :gate gate
                                   (if (null balance-tracker) (values)
                                       (values :balance-tracker balance-tracker)))))
    (if (slot-boundp ope 'prioritizer)
        (reinitialize-instance prioritizer :supplicant supplicant)
        (setf prioritizer (make-instance 'ope-prioritizer :supplicant supplicant)))
    (when (or (not (slot-boundp ope 'scalper))
              (eq :terminated (task-status scalper)))
      (setf scalper (pexec (:name "qdm-preα ope scalper")
                      (loop (ope-scalper-loop ope)))))))

;;;
;;; ACCOUNT TRACKING
;;;

(defclass account-tracker ()
  ((gate :initarg :gate)
   (treasurer :initarg :treasurer)
   (ope :initarg :ope)
   (lictors :initform nil)))

(defclass balance-tracker ()
  ((balances :initarg :balances :initform nil)
   (input :initform (make-instance 'channel))
   (output :initform (make-instance 'channel))
   (gate :initarg :gate)
   (delay :initform 15)
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
  (with-slots (gate input delay) tracker
    (ignore-errors (send input `(balances . ,(account-balances gate))))
    (sleep delay)))

(defmethod vwap ((tracker account-tracker) &key type market depth net)
  (vwap (getf (slot-value tracker 'lictors) market) :type type :depth depth :net net))

(defmethod shared-initialize :after ((tracker balance-tracker) names &key)
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
    ((tracker account-tracker) names &key markets)
  (with-slots (lictors treasurer gate ope) tracker
    (dolist (market markets)
      (setf (getf lictors market)
            (make-instance 'execution-tracker :market market :gate gate)))
    (unless (slot-boundp tracker 'ope)
      (setf ope (make-instance 'ope :gate gate :balance-tracker tracker)))
    (if (slot-boundp tracker 'treasurer) (reinitialize-instance treasurer)
        (setf treasurer (make-instance 'balance-tracker :gate gate)))))

(defun gapps-rate (from to)
  (getjso "rate" (read-json (drakma:http-request
                             "http://rate-exchange.appspot.com/currency"
                             :parameters `(("from" . ,from) ("to" . ,to))
                             :want-stream t))))

(defclass fee-tracker ()
  ((market :initarg :market)
   (gate :initarg :gate)
   (delay :initform 67)
   fee thread))

(defun fee-tracker-loop (tracker)
  (with-slots (market gate delay fee) tracker
    (awhen (market-fee gate market) (setf fee it))
    (sleep delay)))

(defmethod shared-initialize :after ((tracker fee-tracker) names &key)
  (with-slots (thread market gate fee) tracker
    (loop (awhen (market-fee gate market) (setf fee it) (return)))
    (when (or (not (slot-boundp tracker 'thread))
              (eq :terminated (task-status thread)))
      (setf thread
            (pexec
                (:name (concatenate 'string "qdm-preα fee tracker for " (name market))
                 :initial-bindings `((*read-default-float-format* double-float)))
              ;; TODO: just pexec anew each time...
              ;; you'll understand what you meant someday, right?
              (loop (fee-tracker-loop tracker)))))))

(defclass maker ()
  ((market :initarg :market :reader market)
   (fund-factor :initarg :fund-factor :initform 1)
   (resilience-factor :initarg :resilience :initform 1)
   (targeting-factor :initarg :targeting :initform 3/5)
   (control :initform (make-instance 'channel))
   (fee-tracker :initarg :fee-tracker)
   (trades-tracker :initarg :trades-tracker)
   (book-tracker :initarg :book-tracker)
   (account-tracker :initarg :account-tracker)
   (name :initarg :name :accessor name)
   thread))

(defun %round (maker)
  (with-slots (fund-factor resilience-factor targeting-factor market name
               fee-tracker trades-tracker book-tracker account-tracker)
      maker
    ;; whoo!
    (send (slot-value trades-tracker 'control) '(max))
    ;; Get our balances
    (let (;; TODO: split into primary resilience and counter resilience
          (resilience (* resilience-factor
                         (recv (slot-value trades-tracker 'output))))
          ;; TODO: doge is cute but let's move on
          (doge/btc (vwap trades-tracker :depth 50 :type :buy)))
      (flet ((symbol-funds (symbol) (asset-balance account-tracker symbol))
             (total-of (btc doge) (+ btc (/ doge doge/btc)))
             (factor-fund (fund factor) (* fund fund-factor factor)))
        (let* ((fee (slot-value fee-tracker 'fee))
               (total-btc (symbol-funds (slot-value market 'primary)))
               (total-doge (symbol-funds (slot-value market 'counter)))
               (total-fund (total-of total-btc total-doge))
               (investment (if (zerop total-fund) 0 (/ total-btc total-fund)))
               (btc (factor-fund total-btc (* investment targeting-factor)))
               (doge (factor-fund total-doge (- 1 (* investment targeting-factor)))))
          ;; report funding
          ;; FIXME: modularize all this decimal point handling
          (flet ((asset-decimals (kind)
                   (slot-value (slot-value market kind) 'decimals))
                 (depth-profit (&optional depth)
                   (flet ((vwap (side) (vwap account-tracker :type side :net t
                                             :market market :depth depth)))
                     (handler-case
                         (* 100 (1- (profit-margin (vwap "buy") (vwap "sell"))))
                       (division-by-zero () 0)))))
            ;; time, total, primary, counter, invested, risked, risk bias, pulse
            (format t "~&~A ~6@A ~V$ ~V$ ~V$ ~V$ ~$% ~$% ~@$ ~
                       ~6@$ ~6@$ ~6@$ ~6@$"
                    (format-timestring nil (now)
                                       :format '((:hour 2) #\:
                                                 (:min 2) #\:
                                                 (:sec 2)))
                    name
                    (asset-decimals 'primary)  total-fund
                    (asset-decimals 'counter) (* total-fund doge/btc)
                    (asset-decimals 'primary)  total-btc
                    (asset-decimals 'counter) total-doge
                    (* 100 investment)
                    (* 100 (if (zerop total-fund) 0
                               (/ (total-of btc doge) total-fund)))
                    (* 100 (if (zerop total-fund) 0
                               (/ (total-of (- btc) doge) total-fund)))
                    (depth-profit)
                    (depth-profit total-fund)
                    (depth-profit (/ total-fund 4))
                    (depth-profit (/ total-fund 16)))
            (force-output)
            (with-slots (ope) account-tracker
              (send (slot-value ope 'input) (list fee btc doge resilience))
              ;; distance from target equilibrium ( magic number 1/2 = target )
              (let ((lopsidedness (abs (- 1/2 investment))))
                ;; soft limit test: are we within (magic) 33% of the target?
                (when (> lopsidedness 1/4)
                  (flet ((urgent (class side fund)
                           (let ((price (1- (slot-value (fourth (slot-value book-tracker side)) 'price))))
                             (make-instance class :market market
                                            ;; jump back (magic) 1/7th of distance to target
                                            :volume (* fund lopsidedness 1/23)
                                            :price (abs price)))))
                    ;; ugh
                    (sleep 2)
                    ;; theoretically, this could exceed available volume, but
                    ;; that's highly unlikely with a fund-factor below ~3/2
                    (awhen (ope-place ope (if (> investment 1/2)
                                              (urgent 'ask 'asks total-btc)
                                              (urgent 'bid 'bids total-doge)))
                      (format t " ~A" it) (force-output)))))
              (recv (slot-value ope 'output)))))))))

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
  (with-slots (market fee-tracker trades-tracker book-tracker account-tracker thread) maker
    ;; FIXME: wtf is this i don't even
    (unless (slot-boundp maker 'trades-tracker)
      (setf trades-tracker (make-instance 'trades-tracker :market market))
      (sleep 12))
    (unless (slot-boundp maker 'book-tracker)
      (setf book-tracker (make-instance 'book-tracker :market market))
      (sleep 12))
    (unless (slot-boundp maker 'account-tracker)
      (setf account-tracker (make-instance 'account-tracker :gate gate :markets `(,market)))
      (sleep 12))
    ;; FIXME: ...
    (unless (slot-boundp maker 'fee-tracker)
      (setf fee-tracker (make-instance 'fee-tracker :market market :gate gate)))
    ;; stitchy!
    (setf (slot-value (slot-value account-tracker 'ope) 'book-channel)
          (slot-value book-tracker 'output))
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

(defun reset-the-net (maker &optional (revive t))
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
            (trades-tracker updater)
            (trades-tracker worker)
            (book-tracker updater)
            (book-tracker worker)
            (fee-tracker thread))))
  (when revive
    (mapc 'reinitialize-instance
          (list* (slot-value maker 'book-tracker)
                 (slot-value maker 'account-tracker)
                 (slot-value maker 'trades-tracker)
                 (slot-value (slot-value maker 'account-tracker) 'gate)
                 (slot-value (slot-value maker 'account-tracker) 'ope)
                 (slot-value (slot-value (slot-value maker 'account-tracker) 'ope) 'supplicant)
                 (slot-value maker 'fee-tracker)
                 maker
                 (loop
                    for (key value) on
                      (slot-reduce maker account-tracker lictors)
                    by #'cddr collect value)))))

(defmacro define-maker (name &rest keys
                        &key market gate
                          ;; just for interactive convenience
                          fund-factor targeting resilience
                          fee-tracker trades-tracker
                          book-tracker account-tracker)
  (declare (ignore fund-factor targeting resilience fee-tracker
                   trades-tracker book-tracker account-tracker))
  (dolist (key '(:market :gate)) (remf keys key))
  `(defvar ,name (make-instance 'maker :market ,market :gate ,gate
                                :name ,(string-trim "*+<>" name)
                                ,@keys)))

#+nil
(define-maker *maker* (find-market "market" *exchange*)
  (make-instance 'kraken-gate
                 :pubkey #P "secrets/some.pubkey"
                 :secret #P "secrets/some.secret"))
