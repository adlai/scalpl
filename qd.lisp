;;;; qd.lisp - quick and dirty. kraken's api... wow

(defpackage #:glock.qd
  (:use #:cl #:st-json #:local-time #:glock.util #:glock.connection))

(in-package #:glock.qd)

(defvar *auth*)

(defun auth-request (path &optional options)
  (when *auth* (post-request path (car *auth*) (cdr *auth*) options)))

(defun get-assets ()
  (mapjso* (lambda (name data) (setf (getjso "name" data) name))
           (get-request "Assets")))

(defvar *assets* (get-assets))

(defun get-markets ()
  (mapjso* (lambda (name data) (setf (getjso "name" data) name))
           (get-request "AssetPairs")))

(defvar *markets* (get-markets))

;;; TODO: verify the number of decimals!
(defun parse-price (price-string decimals)
  (let ((dot (position #\. price-string)))
    (parse-integer (remove #\. price-string)
                   :end (+ dot decimals))))

(defun get-book (pair &optional count
                 &aux (decimals (getjso "pair_decimals"
                                        (getjso pair *markets*))))
  (with-json-slots (bids asks)
      (getjso pair
              (get-request "Depth"
                           `(("pair" . ,pair)
                             ,@(when count
                                     `(("count" . ,(princ-to-string count)))))))
    (flet ((parse (raw-order)
             (destructuring-bind (price amount timestamp) raw-order
               (declare (ignore timestamp))
               (cons (parse-price price decimals)
                     ;; the amount seems to always have three decimals
                     (read-from-string amount)))))
      (let ((asks (mapcar #'parse asks))
            (bids (mapcar #'parse bids)))
        (values asks bids)))))

;;; order-book and my-orders should both be in the same format:
;;; a list of (PRICE . AMOUNT), representing one side of the book
;;; TODO: deal with partially completed orders
(defun ignore-mine (order-book my-orders &aux new)
  (dolist (order order-book (nreverse new))
    (let ((mine (find (car order) my-orders :test #'= :key #'car)))
      (if mine
          (let ((without-me (- (cdr order) (cdr mine))))
            (setf my-orders (remove mine my-orders))
            (unless (< without-me 0.001)
              (push (cons (car order) without-me) new)))
          (push order new)))))

(defun open-orders ()
  (mapjso* (lambda (id order) (setf (getjso "id" order) id))
           (getjso "open" (auth-request "OpenOrders"))))

(defun cancel-order (order)
  (auth-request "CancelOrder" `(("txid" . ,(getjso "id" order)))))

(defun cancel-pair-orders (pair)
  (mapjso (lambda (id order)
            (declare (ignore id))
            (when (string= pair (getjso* "descr.pair" order))
              (cancel-order order)))
          (open-orders)))

(defparameter *validate* nil)

(define-condition volume-too-low () ())

(defun post-limit (type pair price volume decimals &optional options)
  (let ((price (/ price (expt 10d0 decimals))))
    (multiple-value-bind (info errors)
        (auth-request "AddOrder"
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
                      (post-limit type pair price (+ volume 0.01) 0 options))
                    ;; (signal 'volume-too-low)
                    (return
                      (post-limit type pair price (* volume price) 0
                                  (apply #'concatenate 'string "viqc"
                                         (when options '("," options))))))
                (format t "~&~A~%" message)))
          (progn
            ;; theoretically, we could get several order IDs here,
            ;; but we're not using any of kraken's fancy forex nonsense
            (setf (getjso* "descr.id" info) (car (getjso* "txid" info)))
            (getjso "descr" info))))))

;;; TODO: Incorporate weak references and finalizers into the whole CSPSM model
;;; so they get garbage collected when there are no more references to the
;;; output channels

;;;
;;; TRADES
;;;

(defclass trades-tracker ()
  ((pair :initarg :pair)
   (control :initform (make-instance 'chanl:channel))
   (buffer :initform (make-instance 'chanl:channel))
   (output :initform (make-instance 'chanl:channel))
   (delay :initarg :delay :initform 65)
   trades last updater worker))

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
                                volume price (* volume price)
                                (concatenate 'string side kind data)))))
                    trades)
            last)))

(defun vwap (tracker &key since type)
  (let ((trades (slot-value tracker 'trades)))
    (when since
      (setf trades (remove since trades :key #'car :test #'timestamp>=)))
    (when type
      (setf trades (remove (ccase type (buy #\b) (sell #\s)) trades
                           :key (lambda (trade) (char (fifth trade) 0))
                           :test-not #'char=)))
    (/ (reduce #'+ (mapcar #'fourth trades))
       (reduce #'+ (mapcar #'second trades)))))

(defun tracker-loop (tracker)
  (with-slots (control buffer output trades) tracker
    (chanl:select
      ((recv control command)
       ;; commands are (cons command args)
       (case (car command)
         ;; max - find max seen trade size
         (max (chanl:send output (reduce #'max (mapcar #'second trades))))
         ;; vwap - find vwap over recent trades
         (vwap (chanl:send output (apply #'vwap tracker (cdr command))))
         ;; pause - wait for any other command to restart
         (pause (chanl:recv control))))
      ((recv buffer raw-trades)
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

(defmethod initialize-instance :after ((tracker trades-tracker) &key)
  (with-slots (pair updater buffer delay worker last trades) tracker
    (unless (slot-boundp tracker 'updater)
      (setf updater
            (chanl:pexec
                (:name (concatenate 'string "qdm-preα trades updater for " pair)
                       :initial-bindings `((*read-default-float-format* double-float)))
              (loop
                 (multiple-value-bind (raw-trades until)
                     (handler-case (trades-since pair last)
                       (unbound-slot () (trades-since pair)))
                   (setf last until)
                   (chanl:send buffer raw-trades)
                   (sleep delay))))))
    (unless (slot-boundp tracker 'trades)
      (setf trades
            (let ((raw-trades (chanl:recv buffer)))
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
                      (cdr raw-trades) :initial-value (list (car raw-trades))))))
    (unless (slot-boundp tracker 'worker)
      (setf worker
            (chanl:pexec (:name (concatenate 'string "qdm-preα trades worker for " pair))
              ;; TODO: just pexec anew each time...
              ;; you'll understand what you meant someday, right?
              (loop (tracker-loop tracker)))))))

;;;
;;; ORDER BOOK
;;;

(defclass book-tracker ()
  ((pair :initarg :pair)
   (control :initform (make-instance 'chanl:channel))
   (bids-output :initform (make-instance 'chanl:channel))
   (asks-output :initform (make-instance 'chanl:channel))
   (delay :initarg :delay :initform 8)
   bids asks updater worker))

(defun book-loop (tracker)
  (with-slots (control bids asks bids-output asks-output) tracker
    (handler-case
        (chanl:select
          ((recv control command)
           ;; commands are (cons command args)
           (case (car command)
             ;; pause - wait for any other command to restart
             (pause (chanl:recv control))))
          ((send bids-output bids))
          ((send asks-output asks))
          (t (sleep 0.2)))
      (unbound-slot ()))))

(defmethod initialize-instance :after ((tracker book-tracker) &key)
  (with-slots (updater worker bids asks pair delay) tracker
    (when (or (not (slot-boundp tracker 'updater))
              (eq :terminated (chanl:task-status updater)))
      (setf updater
            (chanl:pexec
                (:name (concatenate 'string "qdm-preα book updater for " pair)
                 :initial-bindings `((*read-default-float-format* double-float)))
              (loop
                 (setf (values asks bids) (get-book pair))
                 (sleep delay))))
    (when (or (not (slot-boundp tracker 'worker))
              (eq :terminated (chanl:task-status worker)))
      (setf worker
            (chanl:pexec (:name (concatenate 'string "qdm-preα book worker for " pair))
              ;; TODO: just pexec anew each time...
              ;; you'll understand what you meant someday, right?
              (loop (book-loop tracker))))))))

;;;
;;; ACCOUNT TRACKING
;;;

(defclass account-tracker ()
  ((balances :initarg :balances)
   (control :initform (make-instance 'chanl:channel))
   (auth :initarg :auth)
   (delay :initform 27)
   updater worker))

(defun account-loop (tracker)
  (with-slots (auth balances control) tracker
    (let ((command (chanl:recv control)))
      (typecase command
        ;; ( asset . channel )
        (cons (chanl:send (cdr command)
                          (or (cdr (assoc (car command) balances
                                          :test #'string=))
                              0)))
        (t (let ((new (auth-request "Balance")))
             (when new
               (setf balances
                     (mapcar-jso (lambda (asset balance)
                                   (cons asset (read-from-string balance)))
                                 new)))))))))

(defmethod initialize-instance :after ((tracker account-tracker) &key)
  (with-slots (auth balances control worker updater delay) tracker
    (when (or (not (slot-boundp tracker 'worker))
              (eq :terminated (chanl:task-status worker)))
      (setf worker
            (chanl:pexec (:name "qdm-preα account worker"
                                :initial-bindings `((*auth* ,auth)
                                                    (*read-default-float-format* double-float)))
              ;; TODO: just pexec anew each time...
              ;; you'll understand what you meant someday, right?
              (loop (account-loop tracker)))))
    (when (or (not (slot-boundp tracker 'updater))
              (eq :terminated (chanl:task-status updater)))
      (setf updater
            (chanl:pexec (:name "qdm-preα account updater")
              (loop (sleep delay) (chanl:send control t)))))
    (chanl:send control t)))

(defun asset-balance (tracker asset &aux (channel (make-instance 'chanl:channel)))
  (with-slots (control) tracker
    (chanl:send control (cons asset channel))
    (chanl:recv channel)))

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

(defun dumbot-oneside (book resilience funds delta max-orders predicate
                       &aux (acc 0) (share 0))
  ;; calculate cumulative depths
  (do* ((cur book (cdr cur))
        (n 0 (1+ n)))
       ((or (> acc resilience) (null cur))
        (let* ((sorted (sort (subseq book 1 n) #'> :key #'cddr))
               (n-orders (min max-orders n))
               (relevant (cons (car book) (subseq sorted 0 (1- n-orders))))
               (total-shares (reduce #'+ (mapcar #'car relevant))))
          (mapcar (lambda (order)
                    (let ((vol (* funds (/ (car order) total-shares))))
                      (cons vol (+ delta (cadr order)))))
                  (sort relevant predicate :key #'cddr))))
    ;; modifies the book itself
    (push (incf share (* 11/6 (incf acc (cdar cur)))) (car cur))
    ;; (format t "~&Found ~$ at ~D total ~$ share ~$~%"
    ;;         (cddar cur) (cadar cur) acc share)
    ))

(defun gapps-rate (from to)
  (getjso "rate" (read-json (drakma:http-request
                             "http://rate-exchange.appspot.com/currency"
                             :parameters `(("from" . ,from) ("to" . ,to))
                             :want-stream t))))

(defun %round (maker)
  (with-slots (fee fund-factor resilience-factor targeting-factor
               pair (my-bids bids) (my-asks asks)
               trades-tracker book-tracker account-tracker)
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
        (let* ((market (getjso pair *markets*))
               (decimals (getjso "pair_decimals" market))
               (price-factor (expt 10 decimals))
               (total-btc (symbol-funds (getjso "base" market)))
               (total-doge (symbol-funds (getjso "quote" market)))
               (total-fund (total-of total-btc total-doge))
               (investment (/ total-btc total-fund))
               (scaled-factor (* investment targeting-factor))
               (btc (factor-fund total-btc scaled-factor))
               (doge (factor-fund total-doge (- 1 scaled-factor))))
          ;; report funding
          ;; FIXME: modularize all this decimal point handling
          (let ((base-decimals (getjso "decimals" (getjso (getjso "base" market) *assets*)))
                (quote-decimals (getjso "decimals" (getjso (getjso "quote" market) *assets*))))
            ;; time, total, base, quote, invested, risked, risk bias
            (format t "~&~A T ~V$ B ~V$ Q ~V$ I ~$% R ~$% B~@$%"
                    (format-timestring nil (now)
                                       :format '((:hour 2) #\:
                                                 (:min 2) #\:
                                                 (:sec 2)))
                    base-decimals  total-fund
                    base-decimals  total-btc
                    quote-decimals total-doge
                    (* 100 investment)
                    (* 100 (/ (total-of btc doge) total-fund))
                    (* 100 (/ (total-of (- btc) doge) total-fund))))
          ;; Now run that algorithm thingy
          (values
           ;; first process bids
           (multiple-value-bind (asks bids)
               (with-slots (asks-output bids-output) book-tracker
                 (values (chanl:recv asks-output)
                         (chanl:recv bids-output)))
             ;; TODO: properly deal with partial and completed orders
             (let ((other-bids (ignore-mine bids (mapcar 'cdr my-bids)))
                   (other-asks (ignore-mine asks (mapcar 'cdr my-asks))))
               ;; NON STOP PARTY PROFIT MADNESS
               (do* ((best-bid (caar other-bids) (caar other-bids))
                     (best-ask (caar other-asks) (caar other-asks))
                     (spread (profit-margin (1+ best-bid) (1- best-ask) fee)
                             (profit-margin (1+ best-bid) (1- best-ask) fee)))
                    ((> spread 1))
                 (ecase (round (signum (* (max 0 (- best-ask best-bid 10))
                                          (- (cdar other-bids) (cdar other-asks)))))
                   (-1 (decf (cdar other-asks) (cdr (pop other-bids))))
                   (+1 (decf (cdar other-bids) (cdr (pop other-asks))))
                   (0         (pop other-bids)      (pop other-asks))))
               (let ((to-bid (dumbot-oneside other-bids resilience doge 1 15 #'>))
                     new-bids)
                 (macrolet ((cancel (old place)
                              `(progn (cancel-order (car ,old))
                                      (setf ,place (remove ,old ,place)))))
                   (flet ((place (new)
                            (handler-case
                                (let ((o (post-limit "buy" pair (cdr new)
                                                     (car new) decimals "viqc")))
                                  ;; rudimentary protection against too-small orders
                                  (if o (push (cons o new) new-bids)
                                      (format t "~&Couldn't place ~S~%" new)))
                              (volume-too-low ()
                                (format t "~&Volume too low: ~S~%" new)
                                t))))
                     (dolist (old my-bids)
                       (let* ((new (find (cadr old) to-bid :key #'cdr :test #'=))
                              (same (and new (< (/ (abs (- (* price-factor
                                                              (/ (car new)
                                                                 (cdr new)))
                                                           (cddr old)))
                                                   (cddr old))
                                                0.15))))
                         (if same (setf to-bid (remove new to-bid))
                             (dolist (new (remove (cadr old) to-bid
                                                  :key #'cdr :test #'>)
                                      (cancel old my-bids))
                               (if (place new) (setf to-bid (remove new to-bid))
                                   (return (cancel old my-bids)))))))
                     (mapcar #'place to-bid)))
                 ;; convert new orders into a saner format (for ignore-mine)
                 (sort (append my-bids
                               (mapcar (lambda (order)
                                         (destructuring-bind (id quote-amount . price) order
                                           (list* id price
                                                  (* price-factor (/ quote-amount price)))))
                                       new-bids))
                       #'> :key #'cadr))))
           ;; next process asks
           (multiple-value-bind (asks bids)
               (with-slots (asks-output bids-output) book-tracker
                 (values (chanl:recv asks-output)
                         (chanl:recv bids-output)))
             (let ((other-bids (ignore-mine bids (mapcar 'cdr my-bids)))
                   (other-asks (ignore-mine asks (mapcar 'cdr my-asks))))
               ;; NON STOP PARTY PROFIT MADNESS
               (do* ((best-bid (caar other-bids) (caar other-bids))
                     (best-ask (caar other-asks) (caar other-asks))
                     (spread (profit-margin (1+ best-bid) (1- best-ask) fee)
                             (profit-margin (1+ best-bid) (1- best-ask) fee)))
                    ((> spread 1))
                 (ecase (round (signum (* (max 0 (- best-ask best-bid 10))
                                          (- (cdar other-bids) (cdar other-asks)))))
                   (-1 (decf (cdar other-asks) (cdr (pop other-bids))))
                   (+1 (decf (cdar other-bids) (cdr (pop other-asks))))
                   (0         (pop other-bids)      (pop other-asks))))
               (let ((to-ask (dumbot-oneside other-asks resilience btc -1 15 #'<))
                     new-asks)
                 (macrolet ((cancel (old place)
                              `(progn (cancel-order (car ,old))
                                      (setf ,place (remove ,old ,place)))))
                   (flet ((place (new)
                            (handler-case
                                (let ((o (post-limit "sell" pair (cdr new)
                                                     (car new) decimals)))
                                  (if o (push (cons o new) new-asks)
                                      (format t "~&Couldn't place ~S~%" new)))
                              (volume-too-low ()
                                (format t "~&Volume too low: ~S~%" new)
                                t))))
                     (dolist (old my-asks)
                       (let* ((new (find (cadr old) to-ask :key #'cdr :test #'=))
                              (same (and new (< (/ (abs (- (car new)
                                                           (cddr old)))
                                                   (cddr old))
                                                0.15))))
                         (if same (setf to-ask (remove new to-ask))
                             (dolist (new (remove (cadr old) to-ask
                                                  :key #'cdr :test #'<)
                                      (cancel old my-asks))
                               (if (place new) (setf to-ask (remove new to-ask))
                                   (return (cancel old my-asks)))))))
                     (mapcar #'place to-ask)))
                 ;; convert new orders into a saner format (for ignore-mine)
                 (sort (append my-asks
                               (mapcar (lambda (order)
                                         (destructuring-bind (id quote-amount . price) order
                                           (list* id price quote-amount)))
                                       new-asks))
                       #'< :key #'cadr))))))))))

(defclass maker ()
  ((pair :initarg :pair :initform "XXBTZEUR")
   (fund-factor :initarg :fund-factor :initform 1)
   (resilience-factor :initarg :resilience :initform 1)
   (targeting-factor :initarg :targeting :initform 1/2)
   (auth :initarg :auth)
   (control :initform (make-instance 'chanl:channel))
   (bids :initform nil :initarg :bids)
   (asks :initform nil :initarg :asks)
   (fee :initform 0.13 :initarg :fee)
   (delay :initform 3 :initarg :delay)
   trades-tracker book-tracker account-tracker thread))

(defun dumbot-loop (maker)
  (with-slots (pair control fund-factor resilience-factor bids asks delay trades-tracker book-tracker)
      maker
    (chanl:select
      ((recv control command)
       ;; commands are (cons command args)
       (case (car command)
         ;; pause - wait for any other command to restart
         (pause (chanl:recv control))
         (stream (setf *standard-output* (cdr command)))))
      (t (setf (values bids asks) (%round maker))
         (sleep delay)))))

(defmethod initialize-instance :after ((maker maker) &key)
  (with-slots (auth pair trades-tracker book-tracker account-tracker thread) maker
    ;; FIXME: wtf is this i don't even
    (unless (slot-boundp maker 'trades-tracker)
      (setf trades-tracker (make-instance 'trades-tracker :pair pair)))
    (unless (slot-boundp maker 'book-tracker)
      (setf book-tracker (make-instance 'book-tracker :pair pair)))
    (unless (slot-boundp maker 'account-tracker)
      (setf account-tracker (make-instance 'account-tracker :auth auth)))
    (when (or (not (slot-boundp maker 'thread))
              (eq :terminated (chanl:task-status thread)))
      (setf thread
            (chanl:pexec
                (:name (concatenate 'string "qdm-preα " pair)
                       :initial-bindings
                       `((*read-default-float-format* double-float)
                         (*auth* ,auth)))
              ;; TODO: just pexec anew each time...
              ;; you'll understand what you meant someday, right?
              (loop (dumbot-loop maker)))))))

(defun pause-maker (maker)
  (with-slots (control) maker
    (chanl:send control '(pause))))

(defvar *maker*
  (make-instance 'maker
                 ;; this may crash if we're not in the right directory...
                 :auth (cons (glock.connection::make-key #P "secrets/kraken.pubkey")
                             (glock.connection::make-signer #P "secrets/kraken.secret"))))

(defun trades-history (since &optional until)
  (getjso "trades"
          (auth-request "TradesHistory"
                        `(("start" . ,since) ("end" . ,until)))))
