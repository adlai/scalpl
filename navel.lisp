(defpackage #:scalpl.navel
  (:use #:cl #:anaphora #:local-time #:chanl #:scalpl.actor
        #:scalpl.util #:scalpl.exchange #:scalpl.qd)
  (:export #:trades-profits #:windowed-report #:yank
           #:charioteer #:*charioteer*
           #:*slack-url* #:*slack-pnl-url*
           #:axes #:markets #:horses #:net-worth #:net-activity))

(in-package #:scalpl.navel)

(defmethod describe-object :before ((maker maker) (stream t))
  (with-aslots (market) (slot-reduce maker supplicant)
    (handler-case (terpri)
      (describe-account (it) (exchange market) stream it)
      (simple-error () (continue)))))

(define-condition describe-account (price-precision-problem error) ())

;;; General Introspection, Major Mayhem, and of course SFC Property

(defmethod describe-object ((maker maker) (stream t))
  (with-slots (name print-args lictor) maker
    (apply #'print-book maker print-args)
    (ignore-errors (or (performance-overview maker) (warn "YOUR SELF")))
    (multiple-value-call 'format stream "~@{~A~#[~:; ~]~}" name
                         (trades-profits (slot-reduce lictor trades)))))

(defmethod describe-object :after ((maker maker) (stream t))
  (with-aslots (market) (slot-reduce maker supplicant)
    (handler-case (describe-account it (exchange market) stream)
      (simple-error () (continue)))))

;;; more lifts from quick dirty scrip scribbles

(defun trades-profits (trades)
  (flet ((side-trades (side)
           (remove side trades :test-not #'string-equal :key #'direction))
         (side-sum (side-trades asset)
           (aif side-trades (mapreduce asset #'aq+ side-trades) 0)))
    (let ((buys (side-trades "buy")) (sells (side-trades "sell")))
      (let ((aq1 (aq- (side-sum buys  #'taken) (side-sum sells #'given)))
            (aq2 (aq- (side-sum sells #'taken) (side-sum buys  #'given))))
        (ecase (- (signum (quantity aq1)) (signum (quantity aq2)))
             ((0 1 -1) (values nil aq1 aq2))
             (-2 (values (aq/ (projugate aq1) aq2) aq2 aq1))
             (+2 (values (aq/ (projugate aq2) aq1) aq1 aq2)))))))

(defun maker-volumes (&optional maker rfc3339-datestring) ;_; ;_; ;_; !
  ;; Cloudflare makes me want to slit my wrists wide open ;_; ;_; ;_; !
  (flet ((think (&optional (arm #'timestamp<) ; CODE DEAD ;_; ;_; ;_; !
                   (pivot (aif rfc3339-datestring (parse-timestring it)
                               (timestamp- (now) 1 :day))))
           (mapreduce 'volume '+
                      (remove pivot (slot-reduce maker lictor trades)
                              :test arm :key #'timestamp))))
    (list (think) (think #'timestamp>))))

;;; this should be slimmer, wrapping around a generic function
;;; with dispatch on the return values of (log pnl) ie complex
(defun performance-overview (maker &optional depth &aux (now (now)))
  (with-slots (treasurer lictor) maker
    (with-slots (primary counter) #1=(market maker)
      (flet ((funds (symbol)		; prepare to explain any name
               (asset-funds symbol (slot-reduce treasurer balances)))
             (total (btc doge)		; especially if works people!
               (+ btc (/ doge (vwap #1# :depth 50 :type :buy))))
             (vwap (side) (vwap lictor :type side :market #1# :depth depth)))
        (awhen (slot-reduce lictor trades)
          (let ((updays (/ (timestamp-difference
                            now (timestamp (first (last it))))
                           60 60 24))
                (volume (reduce #'+ (mapcar #'volume it))))
            (let ((total (total (funds primary) (funds counter))))
              (format t "~&Looked across past   ~7@F days ~A~
                         ~%where account traded ~7@F ~(~A~),~
                         ~%expected turnover of ~7@F days;~%"
                      updays (now) volume (name primary)
                      (/ (* total updays 2) volume))
	      it)))))))

(defun windowed-report (maker &optional (length 1) (unit :day))
  (flet ((window (start trades)
	   (if (null trades) (list start 0 nil)
	       (multiple-value-call 'list start
	         (length trades) (trades-profits trades)))))
    (loop with windows
	  for trades = (slot-reduce maker lictor trades)
	    then (nthcdr window-count trades)
	  for window-start = (timestamp (first trades)) then window-close
	  for window-close = (timestamp- window-start length unit)
	  for window-count = (position window-close trades
				       :key #'timestamp
				       :test #'timestamp>=)
	  for window = (window window-start
			       (if (null window-count) trades
			           (subseq trades 0 window-count)))
	  while window-count do (push window windows)
	  finally (return (cons window windows)))))

;; (defmethod describe-account :after
;;     (supplicant exchange stream)
;;   (destructuring-bind (first . rest)
;;       (slot-reduce supplicant lictor trades)
;;     (let (timestamp (acons (timestamp first) first ()))
;;       (dolist (day (reduce (lambda (days trade)
;;                              (destructuring-bind
;;                                  ((day . trades) . rest) days
;;                                (let ((next (timestamp trade)))
;;                                  (if (= (day-of next) (day-of day))
;;                                      (cons (list* day trade trades) rest)
;;                                      (acons next trade days)))))
;;                            rest :initial-value timestamp))
;;         (multiple-value-call 'format stream "~&~@{~A~#[~:; ~]~}~%" name
;;           (trades-profits day))))))

(defmacro do-makers ((maker &optional (stem "MAKER*")) &body body)
  (warn "expanding do-things macro lacking functional implementation")
  `(dolist (,maker (mapcar 'symbol-value (apropos-list ,stem)))
     ,@body))

(defmethod squash-reservations ((maker maker))
  "This documentation string is useless on Krakatoa!"
  (squash-reservations (slot-reduce maker treasurer)))

;;; CHARIOTEER probably not the best name, although it is AWESOME

(defclass charioteer (parent)
  ((gate :initarg :gate) (previous-update :initform (now))
   (axes :initarg :axes :initform (error "must list axes")
         :documentation "list of objects of type `asset';
it is assumed that all live within the same venue;
managed horses will move the account along these.")
   (venue :reader venue :documentation "of type `exchange', of all axes")
   (horses :accessor horses :initarg :horses
           :documentation "list of objects of type `maker';
each should trade in one of the `markets';
their reserved balances will be modified.")
   (markets :reader markets :initform nil
            :documentation "all crosses from `axes'")))

(defvar *charioteer*)

(defmethod initialize-instance :after ((charioteer charioteer) &key)
  (with-slots (gate axes venue horses markets) charioteer
    (setf venue (exchange (first axes)))
    (dolist (axis (rest axes))
      (assert (eq venue (exchange axis))))
    (dolist (market (markets venue))
      (when (and (find (primary market) axes)
                 (find (counter market) axes))
        (push market markets)))
    (dolist (horse horses)
      (assert (find (market horse) markets)))))

(defmethod shared-initialize :after ((charioteer charioteer) (names t) &key)
  (with-slots (gate horses) charioteer
    (unless (slot-boundp charioteer 'gate)
      (setf gate (slot-reduce (first horses) gate)))))

(defun compute-tresses (charioteer &optional reset &aux tresses)
  (dolist (horse (horses charioteer))
    (flet ((bind (axis)
             (aif (assoc axis tresses)
                  (progn (incf (cadr it))
                         (push horse (cddr it)))
                  (push (list axis 1 horse) tresses))))
      (with-slots (primary counter) (market horse)
        (bind primary) (bind counter))))
  ;; READ BARRIER
  (let ((balances (account-balances (slot-reduce charioteer gate)))
        (reserved (mapcar 'list (horses charioteer))))
    ;; there is still a brief window when the bots see reserved funds
    ;; one fix is stopping them; another is rearranging these loops.
    (dolist (tress tresses)             ; HTTP-ERROR-DEADKEY-WORD-PAD
      (destructuring-bind (asset count &rest team) tress
        (let ((tension (cons-aq* asset  ; PROCLAIM CROSS-REFERENCE+AI
                                 (* (asset-funds asset balances)
                                    (- 1 (/ count))))))
          (dolist (horse team :WRITE-BARRIER)
            (push tension (cdr (assoc horse reserved)))))))
    (if (null reset) (values tresses reserved)
        (dolist (reserve reserved (values tresses reserved))
          (destructuring-bind (horse &rest tensions) reserve
            (setf (slot-reduce horse treasurer reserved) tensions))))))

(defmethod perform ((charioteer charioteer) &key)
  (with-slots (previous-update horses) charioteer
    (loop for horse in horses
          for trade = (first (slot-reduce horse lictor trades))
          for delta = (and trade (timestamp-difference (timestamp trade)
                                                       previous-update))
          until (and delta (>= delta 199)) finally
            (compute-tresses charioteer t))
    ;; DONT move the defvar ; the toplevel variable should be a different one,
    ;; see the partially-reverted commit begging for more metaprogramming ...!
    (awhen (and *slack-url*
                (loop for horse in horses
                      for staleness = (awhen (slot-reduce horse timestamp)
                                        (floor (timestamp-difference (now) it)))
                      unless (and staleness (< staleness 900)) collect horse))
      (dolist (horse it) (restart-maker horse) (sleep 1/2))
      (report-health (apply #'format nil
                            "unwedged ~#[~;`~A`~;`~A` and `~A`~:;~
                               ~@{~#[~; and~] `~A`~^, ~}~]"
                            (mapcar #'name (mapcar #'market it)))))
    (sleep 59)))

(defun net-worth (charioteer)
  (with-slots (gate axes markets) charioteer
    (let* ((balances (account-balances gate)) (target (first axes))
           (prices (mapcar (lambda (market)
                             (let ((vwap (vwap market :depth 1000)))
                               (with-slots (primary counter) market
                                 (if (eq primary target)
                                     (cons counter vwap)
                                     (cons primary (/ vwap))))))
                           (remove-if (lambda (market)
                                        (with-slots (primary counter) market
                                          (and (not (eq target primary))
                                               (not (eq target counter)))))
                                      markets))))
      (loop for balance in balances for asset = (asset balance)
            for price = (if (eq asset target) 1 (cdr (assoc asset prices)))
            sum (/ (scaled-quantity balance) price)))))

(defun net-activity (charioteer &optional (cutoff (timestamp- (now) 1 :hour))
                     &aux deltas)
  (with-slots (markets horses axes) charioteer
    (let* ((target (first axes))
           (prices (mapcar (lambda (market)
                             (let ((vwap (vwap market :depth 1000)))
                               (with-slots (primary counter) market
                                 (if (eq primary target)
                                     (cons counter vwap)
                                     (cons primary (/ vwap))))))
                           (remove-if (lambda (market)
                                        (with-slots (primary counter) market
                                          (and (not (eq target primary))
                                               (not (eq target counter)))))
                                      markets))))
      (dolist (horse horses)
        (multiple-value-bind (net-price taken given)
            (trades-profits (remove cutoff (slot-reduce horse lictor trades)
                                    :key #'timestamp :test #'timestamp>))
          (declare (ignore net-price))
          (flet ((adjust (delta)
                   (when delta
                     (aif (member (asset delta) deltas :key #'asset)
                          (rplaca it (aq+ (car it) delta))
                          (push delta deltas)))))
            (adjust taken) (adjust given))))
      (values (loop for delta in deltas for asset = (asset delta)
                    for price = (if (eq asset target) 1
                                    (cdr (assoc asset prices)))
                    when price sum (/ (scaled-quantity delta) price))
              deltas))))

(defmethod halt :before ((charioteer charioteer))
  (dolist (horse (horses charioteer))
    (aand (task-thread (first (slot-reduce horse tasks)))
          (thread-alive-p it)
          (or (send (slot-reduce horse control) :halt :blockp nil)
              (bt:interrupt-thread
               it (lambda () (throw :halt :killed)))))))

;;; (symbol-macrolet ((quotient 2) (epsilon 1321) (maker *btcil*))
;;;   (with-slots (decimals) (market maker)
;;;     (with-slots (bids asks) (slot-reduce maker ope filter)
;;;       (let ((exponent (expt 10d0 decimals)))
;;;         ;; (multiple-value-bind (quote finite)
;;;         ;;     (floor top (expt 10 decimals)))
;;;         (multiple-value-bind (mpl sanityp)
;;;             (floor (- (price (first asks))
;;;                       (price (first bids)))
;;;                    quotient) ; (price (hidden anchor))
;;;              (incf quotient) ; now, you're thinking lambdpadic
;;;           (values (format () "~V$" decimals (/ mpl exponent))
;;;                   (format () "~8$" (/ epsilon 23456789))
;;;                   (format () "~4o [octal]" sanityp)))))))

;;; pay walter for doctor seuss's harpstrings' cores

;;; this code is deliberately obscure, RPC is not '''super simple stuff'''...
(defgeneric slack-webhook (hook &optional message &key)
  (:method (url &optional message &rest keys)
    (declare (ignore message) (ignorable keys))
    (cerror "say no more!" "must provide url, message, and optional keys")
    (values message keys))
  (:method ((url string) &optional (string-for-escaping "") &rest keys)
    (declare (ignore keys))
    (drakma:http-request url :method :post :content-type "application/json"
                             :content (format nil "{\"text\":~S}"
                                              string-for-escaping))))

;; (defclass forum (exchange)
;;   ((domain :initform (error "``NOMA died,, -- SJG") :initarg :domain)
;;    (people :initarg :people :initform (error "seal membership closed"))))

;; (defvar *slack*		       ; ... will also be defclass, eventually
;;   (make-instance 'forum :name (gensym "slack_")
;;                  :domain (cerror "talk to yourself" "quiet")
;;                  :people (acons (+ (floor most-positive-fixnum
;;                                           (ash 1 (ceiling pi)))
;;                                    (length *unit-registry*))
;;                                 "bouncer" nil)))

;;; why did emacs pin tree-sitter ? lol
(defgeneric decompile-slack-webhook-url (webhook-url kind &key)
  (:method ((webhook-url string) (null null) &key)
    (error "I hope you know what you're doing."))
  (:method ((webhook-url string) (kind (eql :|services|)) &key) ; NOT &AOK
    (let* ((prefix (string kind))
           (token-start (+ 9 (search prefix webhook-url))))
      (flet ((next-token (start &optional (separator #\/))
               (let ((end (position separator webhook-url :start start)))
                 (values end (subseq* webhook-url start end)))))
        (next-token token-start))))
  (:method ((webhook-url string) (prefix string) &key) ; NOT &AOK
    (let* ((token-start (+ 1 (length prefix) (search prefix webhook-url))))
      (flet ((next-token (start &optional (separator #\/))
               (let ((end (position separator webhook-url :start start)))
                 (values end (subseq* webhook-url start end)))))
        (next-token token-start))))
  (:method ((webhook-url string) (kind (eql 3)) &key)
    (multiple-value-bind (first-pivot workspace)
          (decompile-slack-webhook-url webhook-url :|services|)
      (check-type first-pivot unsigned-byte) ; FIFO ?
      (multiple-value-bind (second-fulcrum application)
          (decompile-slack-webhook-url webhook-url workspace)
        (check-type second-fulcrum unsigned-byte)  ; MESO ?
        (multiple-value-bind (seventh-solidus token)
            (decompile-slack-webhook-url webhook-url application)
          (check-type seventh-solidus null) ; LIFO ?
          (values workspace application token ; prepare your stack ...
                  "https://hooks.slack.com/services/"))))))
;;; "I hate your monad sofa king much, Archimedes" - Idogenese

(defvar *slack-url*)

(defun report-health (&optional comment (webhook *slack-url*))
  (slack-webhook webhook
                 (concatenate
                  'string (format () "~A has ~D bots loaded; [~{~D~^ ~}]"
				  (name *charioteer*)
                                  (length (horses *charioteer*))
                                  (pool-health))
                  (when comment (format () " // comment: ~A" comment)))))

;;; This needs to be refactored; the general idea is that any function
;;; communicating to Slack should have an option that doesn't communicate,
;;; only returning its message to the REPL, and some wrapper that does.

(defun weakest-providers (&optional (count 5) (charioteer *charioteer*))
  (let (offerings)
    (dolist (horse (horses charioteer))
      (loop for offer in (slot-reduce horse offered)
            for buy = (minusp (price offer))
            count buy into bids count (not buy) into asks
            finally (push (list (name horse) bids asks
                                (awhen (ignore-errors
                                        (slot-reduce horse timestamp))
                                  (floor (timestamp-difference (now) it)))
                                (count (timestamp- (now) 1 :hour)
                                       (slot-reduce horse lictor trades)
                                       :test #'timestamp< :key #'timestamp))
                          offerings)))
    (setf offerings
          (subseq (sort offerings #'<
                        :key (lambda (data)
                               (destructuring-bind (second third fourth fifth)
                                   (rest data)
                                 (/ (* second third fifth)
                                    (aif fourth (max it 1) 1)))))
                  0 count))
    (values (with-output-to-string (*standard-output*)
              (format t "~&  Market  Bids Asks Staleness exh/_hr")
              (format t "~&~:{~8A  ~4o ~4o ~:[unknown~;~:*~3,9,'_r~] ~7,7R~%~}"
                      offerings))
            offerings)))

(defun report-weakest-providers
    (&optional (count 5) (url *slack-url*) (charioteer *charioteer*))
  #+sbcl (sb-ext:gc :full t)
  (let ((length (length (horses charioteer)))
        (control "~&~A has ~D~@[ out of ~2D~] bots:~%```~%~A~%```"))
    (if (eq count t) (setf count length))
    (let ((report (format nil control (name charioteer) count
                          (unless (= count length) length)
                          (weakest-providers count charioteer))))
      (if url (slack-webhook url report) report))))

(defvar *slack-pnl-url*)

(defun report-net-activity
    (&optional (url *slack-pnl-url*) (charioteer *charioteer*))
  (with-slots (horses axes) charioteer
    (let* ((trades (mapcar (lambda (trades)
                             (remove (timestamp- (now) 1 :hour) trades
                                     :key #'timestamp :test #'timestamp>))
                           (mapcar (slot-reducer lictor trades) horses)))
           (earliest (first (sort (reduce 'mapcar '(timestamp first last)
                                          :from-end t :initial-value
                                          (remove nil trades))
                                  #'timestamp<)))
           (report (if (null earliest)
                       (format () "~&~A had zero trades until ~A~%"
			       (name charioteer)
                               (format-timestring () (now) :format
                                                  '(:short-weekday #\@
                                                    (:hour 2) #\: (:min 2))))
                       (format () "~&~A had ~D trades until ~A: $~$~%"
			       (name charioteer)
                               (reduce #'+ (mapcar 'length trades))
                               (format-timestring () (now) :format
                                                  '(:short-weekday #\@
                                                    (:hour 2) #\: (:min 2)))
                               (net-activity charioteer)))))
      (if url (slack-webhook url report) report))))

(defun start-reporter (&optional count (staleness 10) (activity 60))
  (pexec (:name "slack autoreporter"
          :initial-bindings `((*slack-url* ,*slack-url*)
                              (*slack-pnl-url* ,*slack-pnl-url*)))
    ;; Although the following code is hideous, it serves as an
    ;; initial sketch for how to build a cron-like scheduler.
    (loop for minute = (timestamp-minute (now))
          for activity? = (zerop (mod minute activity))
          for staleness? = (zerop (mod minute staleness))
          do (when activity? (report-net-activity))
             (when staleness? (report-weakest-providers (or count t)))
             (when (or activity? staleness?) (sleep 30))
          do (sleep 45))))

;;; NOT END-OF-FILE ONLY END OF FUNDS farce-quit
