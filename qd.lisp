(defpackage #:scalpl.qd
  (:use #:cl #:chanl #:anaphora #:local-time
        #:scalpl.util #:scalpl.exchange #:scalpl.actor))

(in-package #:scalpl.qd)

;;;
;;;  ENGINE
;;;

(defun ope-placed (ope)
  (with-slots (placed) (slot-value ope 'supplicant)
    (let ((all (sort (copy-list placed) #'< :key #'price)))
      (flet ((split (test) (remove-if test all :key #'price)))
        ;;               bids             asks
        (values (split #'plusp) (split #'minusp))))))

;;; response: placed offer if successful, nil if not
(defun ope-place (ope offer)
  (with-slots (control response) ope
    (send control (cons :offer offer)) (recv response)))

;;; response: trueish = offer no longer placed, nil = unknown badness
(defun ope-cancel (ope offer)
  (with-slots (control response) (slot-value ope 'supplicant)
    (send control (cons :cancel offer)) (recv response)))

(defclass filter (actor)
  ((abbrev :allocation :class :initform "filter")
   (bids :initform ()) (asks :initform ()) (book-cache :initform nil)
   (supplicant :initarg :supplicant :initform (error "must link supplicant"))
   (frequency  :initarg :frequency  :initform 17))) ; FIXME: s/ll/sh/!?

(defmethod christen ((filter filter) (type (eql 'actor)))
  (slot-reduce filter supplicant name))

;;; TODO: deal with partially completed orders
(defun ignore-offers (open mine &aux them)
  (dolist (offer open (nreverse them))
    (aif (find (price offer) mine :test #'= :key #'price)
         (let ((without-me (- (volume offer) (volume it))))
           (setf mine (remove it mine))
           (unless (< without-me 0.001)
             (push (make-instance 'offer :market (slot-value offer 'market)
                                  :price (price offer) :volume without-me)
                   them)))
         (push offer them))))

;;; needs to do three different things
;;; 1) ignore-offers - fishes offers from linked supplicant
;;; 2) profitable spread - already does (via ecase spaghetti)
;;; 3) profit vs recent cost basis - done, shittily - TODO parametrize depth

(defmethod perform ((filter filter) &key)
  (with-slots (market book-cache bids asks frequency supplicant) filter
    (let ((book (recv (slot-reduce market book-tracker output))))
      (unless (eq book book-cache)
        (with-slots (placed fee) supplicant
          (destructuring-bind (bid . ask) (recv (slot-reduce fee output))
            (macrolet ((ignore (side)
                         `(ignore-offers (nthcdr ,side (,side book)) placed))
                       (cache (n side) `(price (nth ,n (,side book-cache)))))
              (flet ((ignore-both (car &aux (cdr car))
                       (values (ignore car) (ignore cdr))))
                (setf book-cache book (values bids asks)
                      (ignore-both      ; TODO: targeting-aware lopsidedness
                       (1- (loop for i from 0 count t until
                                (plusp (1- (profit-margin
                                            (cache i car) (cache i cdr)
                                            bid ask)))))))))))))
    (sleep frequency)))

(defclass prioritizer (actor)
  ((next-bids :initform (make-instance 'channel))
   (next-asks :initform (make-instance 'channel))
   (response :initform (make-instance 'channel))
   (supplicant :initarg :supplicant)
   (abbrev :allocation :class :initform "prioritizer")
   (frequency :initarg :frequency :initform 1/7))) ; FIXME: s/ll/sh/

(defmethod christen ((prioritizer prioritizer) (type (eql 'actor)))
  (slot-reduce prioritizer supplicant name)) ; this is starting to rhyme

(defun sufficiently-different? (new old) ; someday dispatch on market
  (< 0.03 (abs (log (/ (quantity (given new)) (quantity (given old)))))))

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
           (frob it (reverse excess))   ; which of these is worst?
           (if excess (frob () excess)  ; choose the lesser weevil
               (and target placed (= (length target) (length placed))
                    (loop for new in target and old in placed
                       when (sufficiently-different? new old)
                       collect new into news and collect old into olds
                       finally (when news (frob news olds)))))))))

;;; receives target bids and asks in the next-bids and next-asks channels
;;; sends commands in the control channel through #'ope-place
;;; sends completion acknowledgement to response channel
(defmethod perform ((prioritizer prioritizer) &key)
  (with-slots (next-bids next-asks response frequency) prioritizer
    (multiple-value-bind (next source)
        (recv (list next-bids next-asks) :blockp nil)
      (multiple-value-bind (placed-bids placed-asks) (ope-placed prioritizer)
        (if (null source) (sleep frequency)
            ((lambda (side) (send response (prioriteaze prioritizer next side)))
             (if (eq source next-bids) placed-bids placed-asks)))))))

(defun profit-margin (bid ask &optional (bid-fee 0) (ask-fee 0))
  (abs (if (= bid-fee ask-fee 0) (/ ask bid)
           (/ (* ask (- 1 (/ ask-fee 100)))
              (* bid (+ 1 (/ bid-fee 100)))))))

(defun dumbot-offers (foreigners       ; w/filter to avoid feedback
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
            (and (> acc resilience)  ;     OR:   BOTH: processed past resilience
                 (> processed-tally max-orders))) ; AND: processed enough orders
        (flet ((pick (count offers)
                 (sort (subseq* (sort (or (subseq offers 0 (1- processed-tally))
                                          (warn "~&FIXME: GO DEEPER!~%") offers)
                                      #'> :key (lambda (x) (volume (cdr x))))
                               0 count) #'< :key (lambda (x) (price (cdr x)))))
               (offer-scaler (total bonus count)
                 (let ((scale (/ funds (+ total (* bonus count)))))
                   (lambda (order &aux (vol (* scale (+ bonus (car order)))))
                     (with-slots (market price) (cdr order)
                       (make-instance 'offer ; FIXME: :given (ring a bell?)
                                      :given (cons-aq* asset vol) :volume vol
                                      :market market :price (1- price)))))))
          (let* ((target-count (min (floor (/ funds epsilon 4/3)) ; ygni! wut?
                                    max-orders processed-tally))
                 (chosen-stairs         ; the (shares . foreign-offer)s to fight
                  (if (>= magic target-count) (pick target-count others)
                      (cons (first others)
                            (pick (1- target-count) (rest others)))))
                 (total-shares (reduce #'+ (mapcar #'car chosen-stairs)))
                 ;; we need the smallest order to be epsilon
                 (e/f (/ epsilon funds))
                 (bonus (if (>= 1 target-count) 0
                            (/ (- (* e/f total-shares) (caar chosen-stairs))
                               (- 1 (* e/f target-count))))))
            (break-errors (not division-by-zero) ; dbz = no funds left, too bad
              (mapcar (offer-scaler total-shares bonus target-count)
                      chosen-stairs)))))
    ;; TODO - use a callback for liquidity distribution control
    (with-slots (volume) (first remaining-offers)
      (push (incf share (* 4/3 (incf acc volume))) (first remaining-offers)))))

(defclass ope-scalper (parent)
  ((input :initform (make-instance 'channel))
   (output :initform (make-instance 'channel))
   (abbrev :allocation :class :initform "ope")
   (frequency :initform (random 1e1) :initarg :frequency) ; lel = l0l0
   (supplicant :initarg :supplicant) filter prioritizer
   (epsilon :initform 0.001 :initarg :epsilon)
   (magic :initform 3 :initarg :magic-count)
   (spam :initform nil :initarg :spam)))

(defmethod christen ((ope ope-scalper) (type (eql 'actor)))
  (name (slot-value ope 'supplicant)))

(defun ope-sprinner (offers funds count magic bases punk dunk book)
  (if (or (null bases) (zerop count) (null offers)) offers ; c'est pas un bean
      (destructuring-bind (car . cdr) offers
        (multiple-value-bind (bases vwab cost)
            ;; what appears to be the officer, problem?
            ;; (bases-without bases (given top)) fails, because bids are `viqc'
            (bases-without bases (cons-aq* (consumed-asset car) (volume car)))
          (flet ((profit (o)
                   (funcall punk (1- (price o)) (price vwab) (cdar funds))))
            (when vwab
              (signal "~4,2@$ ~A ~D ~V$ ~V$" (profit car) car (length bases)
                      (decimals (market vwab)) (scaled-price vwab)
                      (decimals (asset cost)) (scaled-quantity cost))
              (setf book (rest (member 0 book :test #'< :key #'profit))))
            (if (and vwab (plusp (profit car)))
                `(,car .,(ope-sprinner
                          cdr (destructuring-bind ((caar . cdar) . cdr) funds
                                (aprog1 `((,(- caar (volume car)) .,cdar) .,cdr)
                                  (signal "~S" it)))
                          (1- count) magic bases punk dunk book))
                (ope-sprinner (funcall dunk book funds count magic) funds
                              count magic (and vwab `((,vwab ,(aq* vwab cost)
                                                             ,cost) ,@bases))
                              punk dunk book)))))))

(defun ope-logger (ope)
  (lambda (log)
    (awhen (slot-value ope 'spam)
      (format t "~:[~*~;~&~A~] ~A~@*~:[~%~;~]"
              (cdr (simple-condition-format-arguments log)) it log))))

(defun ope-spreader (book resilience funds epsilon side ope)
  (flet ((dunk (book funds count magic &optional (start epsilon))
           (and book (dumbot-offers book resilience (caar funds)
                                    start count magic))))
    (with-slots (supplicant magic cut) ope
      (with-slots (order-slots) supplicant
        (awhen (dunk book funds (/ order-slots 2) magic)
          (ope-sprinner it funds (/ order-slots 2) magic
                        (getf (slot-reduce ope supplicant lictor bases)
                              (asset (given (first it))))
                        (destructuring-bind (bid . ask)
                            (recv (slot-reduce ope supplicant fee output))
                          (macrolet ((punk (&rest args)
                                       `(lambda (price vwab inner-cut)
                                          (- (* 100 (1- (profit-margin ,@args)))
                                             inner-cut))))
                            (ccase side ; give ☮ a chance!
                              (bids (punk price vwab bid))
                              (asks (punk vwab  price 0 ask)))))
                        #'dunk book))))))

(defmethod perform ((ope ope-scalper) &key)
  (with-slots (input output filter prioritizer epsilon frequency) ope
    (destructuring-bind (primary counter resilience ratio) (recv input)
      (with-slots (next-bids next-asks response) prioritizer
        (macrolet ((do-side (amount side chan epsilon)
                     `(let ((,side (copy-list (slot-value filter ',side))))
                        (unless (or (actypecase ,amount (number (zerop it))
                                                (cons (zerop (caar it))))
                                    (null ,side))
                          (send ,chan (handler-bind
                                          ((simple-condition (ope-logger ope)))
                                        (ope-spreader ,side resilience ,amount
                                                      ,epsilon ',side ope)))
                          (recv response)))))
          (do-side counter bids next-bids
                   (* epsilon (abs (price (first bids))) (max ratio 1)
                      (expt 10 (- (decimals (market (first bids)))))))
          (do-side primary asks next-asks (* epsilon (max (/ ratio) 1))))))
    (send output (sleep frequency))))

(defmethod initialize-instance :after ((ope ope-scalper) &key)
  (with-slots (filter prioritizer supplicant) ope
    (macrolet ((init (slot)
                 `(setf ,slot (make-instance ',slot :supplicant supplicant
                                             :delegates (list supplicant))))
               (children (&rest slots)
                 `(progn ,@(mapcar (lambda (slot) `(adopt ope ,slot)) slots))))
      (children (init prioritizer) (init filter)))))

;;;
;;; ACCOUNT TRACKING
;;;

(defclass maker (parent)
  ((fund-factor :initarg :fund-factor :initform 1)
   (resilience-factor :initarg :resilience :initform 1)
   (targeting-factor :initarg :targeting :initform (random 1.0))
   (skew-factor :initarg :skew-factor :initform 1)
   (cut :initform 0 :initarg :cut) ope (supplicant :initarg :supplicant)
   (snake :initform (list 30))          ; FIXME: snake-args
   (abbrev :initform "maker" :allocation :class) (last-report :initform nil)
   (print-args :initform '(:market t :ours t :wait () :count 28)))) ; perfect

(defmethod christen ((maker maker) (type (eql 'actor)))
  (name (slot-reduce maker gate)))

(defmethod print-object ((maker maker) stream)
  (print-unreadable-object (maker stream :type t :identity nil)
    (write-string (name maker) stream)))

(defun profit-snake (lictor length &aux (trades (slot-value lictor 'trades)))
  (flet ((depth-profit (depth)
           (flet ((vwap (side) (vwap lictor :type side :depth depth)))
             (* 100 (1- (profit-margin (vwap "buy") (vwap "sell"))))))
         (side-last (side)
           (volume (find side trades :key #'direction :test #'string-equal)))
         (chr (fraction)
           (funcall (if (plusp fraction) #'identity #'char-downcase)
                    (digit-char (- 36 (ceiling (* (abs fraction) 26))) 36))))
    (with-output-to-string (out)
      (let* ((min-sum (loop for trade in trades for volume = (net-volume trade)
                         if (string-equal (direction trade) "buy")
                         sum volume into buy-sum else sum volume into sell-sum
                         finally (return (min buy-sum sell-sum))))
             (min-last (apply 'min (mapcar #'side-last '("buy" "sell"))))
             (scale (expt (/ min-sum min-last) (/ (1+ length))))
             (dps (loop for i to length collect
                       (depth-profit (/ min-sum (expt scale i)))))
             (highest (apply #'max 0 (remove-if #'minusp dps)))
             (lowest  (apply #'min 0 (remove-if  #'plusp dps))))
        (format out "~4@$" (depth-profit min-sum))
        (dolist (dp dps (format out "~4@$" (first (last dps))))
          (format out "~C" (case (round (signum dp)) (0 #\Space)
                             (+1 (chr (/ dp highest)))
                             (-1 (chr (- (/ dp lowest)))))))))))

(defun makereport (maker fund rate btc doge investment risked skew &optional ha)
  (with-slots (name market ope snake last-report) maker
    (unless ha
      (let ((new-report (list btc doge investment risked skew
                              (first (slot-reduce maker lictor trades)))))
        (if (equal new-report (if (channelp (first last-report))
                                  (cdr last-report) last-report))
            (return-from makereport)
            (if (channelp (first last-report))
                (setf (cdr last-report) new-report)
                (setf last-report new-report)))))
    (labels ((sastr (side amount) ; TODO factor out aqstr
               (format nil "~V,,V$" (decimals (slot-value market side))
                       0 (float amount 0d0))))
      ;; FIXME: modularize all this decimal point handling
      ;; we need a pprint-style ~/aq/ function, and pass it aq objects!
      ;; time, total, primary, counter, invested, risked, risk bias, pulse
      (format t "~&~A ~A ~{~A~^~1,2@T~} ~4,6T~2,2$% ~2,2$~2,2@$ ~A~%"
              name (subseq (princ-to-string (now)) 11 19)
              (mapcar #'sastr '(primary counter primary counter)
                      `(,@#1=`(,fund ,(* fund rate)) ,btc ,doge))
              (* 100 investment) (* 100 risked) (* 100 skew)
              (apply 'profit-snake (slot-reduce ope supplicant lictor) snake))))
  (force-output))

(defmethod perform ((maker maker) &key)
  (call-next-method maker :blockp ())   ; memento, du musste mori!
  (with-slots (fund-factor resilience-factor targeting-factor skew-factor
               market name ope cut) maker
    (let* ((trades (recv (slot-reduce market trades))) ; nananananana
           ;; TODO: split into primary resilience and counter resilience
           (resilience (* resilience-factor ; FIXME online histomabob
                          (reduce #'max (mapcar #'volume trades))))
           (balances (with-slots (sync) (slot-reduce maker treasurer)
                       (recv (send sync sync)))) ; excellent!
           (doge/btc (vwap market :depth 50 :type :buy)))
      (flet ((total-of (btc doge) (+ btc (/ doge doge/btc))))
        (let* ((total-btc (asset-funds (primary market) balances))
               (total-doge (asset-funds (counter market) balances))
               (total-fund (total-of total-btc total-doge)))
          ;; history, yo!
          ;; this test originated in a harried attempt at bugfixing an instance
          ;; of Maybe, where the treasurer reports zero balances when the http
          ;; request (checking for balance changes) fails; due to use of aprog1
          ;; when the Right Thing™ is awhen1. now that the bug's killed better,
          ;; Maybe thru recognition, the test remains; for when you lose the bug
          ;; don't lose the lesson, nor the joke.
          (unless (zerop total-fund)
            (let* ((buyin (dbz-guard (/ total-btc total-fund)))
                   (btc  (* fund-factor total-btc buyin targeting-factor))
                   (doge (* fund-factor total-doge
                            (/ (- 1 buyin) targeting-factor)))
                   (skew (log (max 1/100 (/ doge btc doge/btc)))))
              ;; report funding
              (makereport maker total-fund doge/btc total-btc total-doge buyin
                          (dbz-guard (/ (total-of    btc  doge) total-fund))
                          (dbz-guard (/ (total-of (- btc) doge) total-fund)))
              (flet ((f (g h) `((,g . ,(* cut (max 0 (* skew-factor h)))))))
                (send (slot-reduce ope input)
                      (list (f btc skew) (f doge (- skew)) resilience
                            (expt (exp skew) skew-factor)))
                (recv (slot-reduce ope output))))))))))

(defmethod initialize-instance :after ((maker maker) &key)
  (with-slots (supplicant ope delegates) maker
    (adopt maker supplicant) (push supplicant delegates)
    (adopt maker (setf ope (make-instance 'ope-scalper
                                          :supplicant supplicant)))))

(defun reset-the-net (maker &key (revive t) (delay 5))
  (mapc 'kill (mapcar 'task-thread (pooled-tasks)))
  #+sbcl (sb-ext:gc :full t)
  (when revive
    (dolist (actor (list (slot-reduce maker market) (slot-reduce maker gate)
                         (slot-reduce maker ope) maker))
      (sleep delay) (reinitialize-instance actor))))

(defmacro define-maker (name &rest keys)
  `(defvar ,name
     (make-instance 'maker :name ,(string-trim "*+<>" name) ,@keys)))

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
        ((0 1 -1) (values nil aq1 aq2))
        (-2 (values (aq/ (- (conjugate aq1)) aq2) aq2 aq1))
        (+2 (values (aq/ (- (conjugate aq2)) aq1) aq1 aq2))))))

(defun performance-overview (maker &optional depth)
  (with-slots (treasurer lictor) maker
    (with-slots (primary counter) #1=(market maker)
      (flet ((funds (symbol)
               (asset-funds symbol (slot-reduce treasurer balances)))
             (total (btc doge)          ; patt'ring on my chamber door?
               (+ btc (/ doge (vwap #1# :depth 50 :type :buy))))
             (vwap (side) (vwap lictor :type side :market #1# :depth depth)))
        (let* ((trades (slot-reduce maker lictor trades))
               (uptime (timestamp-difference
                        (now) (timestamp (first (last trades)))))
               (updays (/ uptime 60 60 24))
               (volume (or depth (reduce #'+ (mapcar #'volume trades))))
               (profit (* volume (1- (profit-margin (vwap "buy")
                                                    (vwap "sell"))) 1/2))
               (total (total (funds primary) (funds counter))))
          (format t "~&Been up              ~7@F days ~A~
                     ~%traded               ~7@F ~(~A~),~
                     ~%profit               ~7@F ~(~2:*~A~*~),~
                     ~%portfolio flip per   ~7@F days,~
                     ~%avg daily profit:    ~4@$%~
                     ~%estd monthly profit: ~4@$%~%"
                  updays (now) volume (name primary) profit
                  (/ (* total updays 2) volume)
                  (/ (* 100 profit) updays total) ; ignores compounding, du'e!
                  (/ (* 100 profit) (/ updays 30) total)))))))

(defmethod print-book ((maker maker) &rest keys &key market ours wait)
  (macrolet ((path (&rest path)
               `(apply #'print-book (slot-reduce ,@path) keys)))
    (with-slots (response) (slot-reduce maker ope prioritizer)
      (multiple-value-bind (next source) (when wait (recv response))
        (let ((placed (multiple-value-call 'cons
                        (ope-placed (slot-reduce maker ope)))))
          (path placed) (when ours (setf (getf keys :ours) placed))
          (when source (send source next)))))
    (when market (path maker market book-tracker))))

(defmethod describe-object ((maker maker) (stream t))
  (with-slots (name print-args lictor) maker
    (apply #'print-book maker print-args) (performance-overview maker)
    (multiple-value-call 'format stream "~@{~A~#[~:; ~]~}" name
                         (trades-profits (slot-reduce lictor trades)))))

#+clozure
(progn
  (defclass memory-supervisor (actor)
    ((to-halt :initarg :to-halt) (trigger :initarg :trigger)
     (frequency :initarg :frequency :initform 15)
     (leave-alive :initarg :leave-alive :initform 2)))

  (defmethod christen ((supervisor memory-supervisor) (type (eql 'actor)))
    (format () "memory supervisor ~A"
            (first (last (slot-value supervisor 'to-halt)))))

  (defmethod perform ((supervisor memory-supervisor) &key)
    (with-slots (to-halt frequency trigger leave-alive tasks) supervisor
      (sleep frequency)
      (flet ((threads (all)
               (remove () (mapcar #'task-thread (set-difference all tasks)))))
        (when (or (> (memory-usage) trigger)
                  (< (+ frequency leave-alive)
                     (count-if #'thread-alive-p (threads (pooled-tasks)))))
          (format t "~&(memory-usage) = ~D, cycling~%" (memory-usage))
          (etypecase leave-alive
            (integer
             (mapc #'halt (reverse to-halt)) (sleep frequency)
             (loop for them = (threads (butlast (pooled-tasks) leave-alive))
                while them do (mapc 'kill them))
             (sleep frequency)            ; "this is just cr*p" - Linus
             (loop until (< (memory-usage) (/ trigger 2)) do
                  (ccl:gc) (sleep frequency))
             (dolist (actor to-halt)
               (reinitialize-instance actor) (sleep frequency)))  ; races :(
            (function (error "TODO"))))))))
