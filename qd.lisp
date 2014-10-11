;;;; qd.lisp

(defpackage #:scalpl.qd
  (:use #:cl #:anaphora #:st-json #:local-time #:scalpl.util #:scalpl.kraken #:scalpl.exchange))

(in-package #:scalpl.qd)

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

;;; TODO: We have the fees paid for each order in the data from the exchange,
;;; so we should be able to calculate the _net_ price for each trade, and use
;;; that for profitability calculations, rather than fee at time of calculation.
(defun trades-history-chunk (tracker &key until since)
  (with-slots (delay gate) tracker
    (awhen (apply #'execution-history gate
               (append (when until `(:until ,until))
                       (when since `(:since ,since))))
      (with-json-slots (count trades) it
          (let* ((total (parse-integer count))
                 (chunk (make-array (list total) :fill-pointer 0)))
            (flet ((process (trades-jso)
                     (mapjso (lambda (tid data)
                               (map nil (lambda (key)
                                          (setf (getjso key data)
                                                (read-from-string (getjso key data))))
                                    '("price" "cost" "fee" "vol"))
                               (with-json-slots (txid time) data
                                 (setf txid tid time (parse-timestamp *kraken* time)))
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
                 (awhen (apply #'execution-history gate
                               :until until :ofs (princ-to-string (fill-pointer chunk))
                               (when since `(:since ,since)))
                   (with-json-slots (count trades) it
                     (let ((next-total (parse-integer count)))
                       (assert (= total next-total))
                       (process trades)))))))))))

(defun execution-worker-loop (tracker)
  (with-slots (trades control buffer) tracker
    ;; this is an example of where you could use STATE MACHINES
    (chanl:select
      ((chanl:recv control channel) (chanl:send channel trades))
      ((chanl:recv buffer trade) (push trade trades))
      (t (sleep 0.2)))))

(defun execution-updater-loop (tracker)
  (with-slots (delay since buffer) tracker
    (loop
       for trade across (or (trades-history-chunk tracker :since since) #())
       do (chanl:send buffer trade)
       finally (when trade (setf since trade)))
    (sleep delay)))

(defmethod shared-initialize :after ((tracker execution-tracker) slots &key)
  (with-slots (worker updater) tracker
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
              (loop (execution-updater-loop tracker)))))))

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
  (declare (optimize (debug 3)))
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
  (with-slots (gate control response placed balance-tracker) ope
    (let ((command (chanl:recv control)))
      (destructuring-bind (car . cdr) command
        (chanl:send response
                    (case car
                      (placed placed)
                      (filter (ignore-offers cdr placed))
                      (offer (balance-guarded-place ope cdr))
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
    (let ((all (sort (copy-list (chanl:recv response)) #'< :key #'price)))
      (flet ((split (sign)
               (remove sign all :key (lambda (x) (signum (price x))))))
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
    (flet ((place (new) (ope-place ope new))
           (amount-change (old new &aux (old-vol (volume old)))
             (/ (abs (- (volume new) old-vol)) old-vol)))
      (flet ((update (target placed &aux percents cutoff)
               ;; (dolist (o target)
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
               (chanl:send prioritizer-response t)))
        (chanl:select
          ((chanl:recv next-bids to-bid) (update to-bid (nth-value 0 (ope-placed ope))))
          ((chanl:recv next-asks to-ask) (update to-ask (nth-value 1 (ope-placed ope)))))))))

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

(defun dumbot-offers (book resilience funds epsilon max-orders &aux (acc 0) (share 0))
  (do* ((cur book (cdr cur))
        (n 0 (1+ n)))
       ((or (and (> acc resilience) (> n max-orders)) (null cur))
        (let* ((sorted (sort (subseq book 1 n) #'> :key (lambda (x) (volume (cdr x)))))
               (n-orders (min max-orders n))
               (relevant (cons (car book) (subseq sorted 0 (1- n-orders))))
               (total-shares (reduce #'+ (mapcar #'car relevant)))
               ;; we need the smallest order to be epsilon
               ;; FIXME: ¿ e/f × n > 1 ?
               (e/f (/ epsilon funds)))
          (flet ((liquidator (bonus total)
                   (lambda (order)
                     (with-slots (market price) (cdr order)
                       (make-instance 'offer :market market :price (- price 3)
                                      :volume (* funds (/ (+ bonus (car order))
                                                          total)))))))
            (let ((sorted (sort relevant #'< :key (lambda (x) (price (cdr x))))))
              (if (> epsilon (/ funds n-orders))
                  ;; temporary fix - disable scaling
                  ;; this means we get the largest m<n offers from the target,
                  ;; rather than m offers distributed throughout the n
                  (remove-if (lambda (offer) (< (volume offer) epsilon))
                             (mapcar (liquidator 0 total-shares) sorted))
                  (mapcar (let ((bonus (/ (- (* e/f total-shares) (caar relevant))
                                          (- 1 (* e/f n-orders)))))
                            (liquidator bonus (+ total-shares (* bonus n-orders))))
                          sorted))))))
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
                          ,@body))))
          ;; Need to rework this flow so the worker (actor calculating priorities) gets
          ;; the entire book at once...
          ;; TODO: properly deal with partial and completed orders
          (with-book ()
            (chanl:send next-bids (dumbot-offers other-bids resilience quote 0.01 15))
            (chanl:recv prioritizer-response))
          (with-book ()
            (chanl:send next-asks (dumbot-offers other-asks resilience base 0.0001 15))
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
    (awhen (gate-request gate "Balance")
      (chanl:send control
                  (cons 'balances
                        (mapcar-jso (lambda (asset balance)
                                      (cons asset (read-from-string balance)))
                                    it))))
    (sleep delay)))

(defmethod vwap ((tracker execution-tracker) &key type market depth)
  (let ((c (make-instance 'chanl:channel)))
    (chanl:send (slot-value tracker 'control) c)
    (let ((trades (remove type (chanl:recv c)
                          :key (lambda (c) (getjso "type" c)) :test #'string/=)))
      (when market
        (setf trades (remove (name market) trades
                             :key (lambda (c) (getjso "pair" c)) :test #'string/=)))
      (when depth
        (setf trades (loop for trade in trades collect trade
                           sum (getjso "vol" trade) into sum
                           until (>= sum depth))))
      (/ (reduce '+ (mapcar (lambda (x) (getjso "cost" x)) trades))
         (reduce '+ (mapcar (lambda (x) (getjso "vol" x)) trades))))))

(defmethod vwap ((tracker account-tracker) &key type market depth)
  (vwap (slot-value tracker 'lictor) :type type :depth depth :market market))

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
    (chanl:send control (cons (name asset) channel))
    (chanl:recv channel)))

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
              (eq :terminated (chanl:task-status thread)))
      (setf thread
            (chanl:pexec
                (:name (concatenate 'string "qdm-preα fee tracker for " (name market))
                 :initial-bindings `((*read-default-float-format* double-float)))
              ;; TODO: just pexec anew each time...
              ;; you'll understand what you meant someday, right?
              (loop (fee-tracker-loop tracker)))))))

(defclass maker ()
  ((market :initarg :market)
   (fund-factor :initarg :fund-factor :initform 1)
   (resilience-factor :initarg :resilience :initform 1)
   (targeting-factor :initarg :targeting :initform 3/5)
   (control :initform (make-instance 'chanl:channel))
   (fee-tracker :initarg :fee-tracker)
   (trades-tracker :initarg :trades-tracker)
   (book-tracker :initarg :book-tracker)
   (account-tracker :initarg :account-tracker)
   (name :initarg :name :accessor name)
   thread))

(defun %round (maker)
  (declare (optimize (debug 3)))
  (with-slots (fund-factor resilience-factor targeting-factor market name
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
        (let* ((fee (slot-value fee-tracker 'fee))
               (total-btc (symbol-funds (slot-value market 'base)))
               (total-doge (symbol-funds (slot-value market 'quote)))
               (total-fund (total-of total-btc total-doge))
               (investment (/ total-btc total-fund))
               (btc (factor-fund total-btc (* investment targeting-factor)))
               (doge (factor-fund total-doge (- 1 (* investment targeting-factor)))))
          ;; report funding
          ;; FIXME: modularize all this decimal point handling
          (flet ((asset-decimals (kind)
                   (slot-value (slot-value market kind) 'decimals))
                 (depth-profit (&optional depth)
                   (* 100 (1- (profit-margin (vwap account-tracker :type "buy"
                                                   :market market :depth depth)
                                             (vwap account-tracker :type "sell"
                                                   :market market :depth depth)
                                             fee)))))
            ;; time, total, base, quote, invested, risked, risk bias, pulse
            (format t "~&~A ~6@A ~V$ ~V$ ~V$ ~$% ~$% ~@$ ~
                       ~6@$ ~6@$ ~6@$ ~6@$ ~6@$ ~6@$"
                    (format-timestring nil (now)
                                       :format '((:hour 2) #\:
                                                 (:min 2) #\:
                                                 (:sec 2)))
                    name
                    (asset-decimals 'base)  total-fund
                    (asset-decimals 'base)  total-btc
                    (asset-decimals 'quote) total-doge
                    (* 100 investment)
                    (* 100 (/ (total-of btc doge) total-fund))
                    (* 100 (/ (total-of (- btc) doge) total-fund))
                    (depth-profit)
                    (depth-profit (* total-fund 16))
                    (depth-profit (* total-fund 4))
                    (depth-profit total-fund)
                    (depth-profit (/ total-fund 4))
                    (depth-profit (/ total-fund 16)))
            (force-output)
            (with-slots (ope) account-tracker
              (chanl:send (slot-value ope 'input) (list fee btc doge resilience))
              ;; distance from target equilibrium ( magic number 1/2 = target )
              (let ((lopsidedness (abs (- 1/2 investment))))
                ;; soft limit test: are we within (magic) 33% of the target?
                (when (> lopsidedness 1/8)
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
              (chanl:recv (slot-value ope 'output)))))))))

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
  (with-slots (market fee-tracker trades-tracker book-tracker account-tracker thread) maker
    ;; FIXME: wtf is this i don't even
    (unless (slot-boundp maker 'trades-tracker)
      (setf trades-tracker (make-instance 'trades-tracker :market market))
      (sleep 12))
    (unless (slot-boundp maker 'book-tracker)
      (setf book-tracker (make-instance 'book-tracker :market market))
      (sleep 12))
    (unless (slot-boundp maker 'account-tracker)
      (setf account-tracker (make-instance 'account-tracker :gate gate))
      (sleep 12))
    ;; FIXME: ...
    (unless (slot-boundp maker 'fee-tracker)
      (setf fee-tracker (make-instance 'fee-tracker :market market :gate gate)))
    ;; stitchy!
    (setf (slot-value (slot-value account-tracker 'ope) 'book-channel)
          (slot-value book-tracker 'book-output))
    (when (or (not (slot-boundp maker 'thread))
              (eq :terminated (chanl:task-status thread)))
      (setf thread
            (chanl:pexec
                (:name (concatenate 'string "qdm-preα " (name market))
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
