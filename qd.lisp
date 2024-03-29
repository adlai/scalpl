(defpackage #:scalpl.qd
  (:use #:cl #:chanl #:anaphora #:local-time
        #:scalpl.util #:scalpl.exchange #:scalpl.actor)
  (:export #:ope-placed #:ope-place #:ope-cancel
           #:prioritizer #:prioriteaze
           #:next-bids #:next-asks #:sufficiently-different?))

(in-package #:scalpl.qd)

;;;
;;;  ENGINE
;;;

(defun ope-placed (ope)
  (with-slots (offered) (slot-value ope 'supplicant)
    (let ((all (sort (copy-list offered) #'< :key #'price)))
      (flet ((split (test) (remove-if test all :key #'price)))
        ;;               bids             asks
        (values (split #'plusp) (split #'minusp))))))

;;; response: placed offer if successful, nil if not
(defun ope-place (ope offer)
  (with-slots (control response) (slot-value ope 'supplicant)
    (send control (list :offer offer))))

;;; response: trueish = offer no longer placed, nil = unknown badness
(defun ope-cancel (ope offer)
  (with-slots (control response) (slot-value ope 'supplicant)
    (send control (list :cancel offer))))

(defclass filter (actor)
  ((abbrev :allocation :class :initform "filter") (cut :initarg :cut)
   (bids :initform ()) (asks :initform ()) (book-cache :initform nil)
   (supplicant :initarg :supplicant :initform (error "must link supplicant"))
   (frequency  :initarg :frequency  :initform 1/7))) ; FIXME: s/ll/sh/!?

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
  (with-slots (market book-cache bids asks frequency supplicant cut) filter
    (let ((book (recv (slot-reduce market book))))
      (unless (eq book book-cache)
	(setf book-cache book)
        (with-slots (offered fee) supplicant
          (destructuring-bind (bid . ask) (recv (slot-reduce fee output))
	    (loop with rudder = (sin (phase cut)) with scale = (abs rudder)
		  for i from 0 for j = (1+ (floor (* i (- 1 scale))))
		  for a = (if (plusp rudder) j i)
		  for b = (if (plusp rudder) i j)
		  ;; do (break)
		  until (< (/ (realpart cut) 100)
			   (1- (profit-margin (price (nth a (car book)))
					      (price (nth b (cdr book)))
					      bid ask)))
		  finally (setf bids (ignore-offers (nthcdr a (car book))
						    offered)
				asks (ignore-offers (nthcdr b (cdr book))
						    offered)))))))
    (sleep frequency)))

(defclass prioritizer (actor)
  ((next-bids :initform (make-instance 'channel))
   (next-asks :initform (make-instance 'channel))
   (response :initform (make-instance 'channel))
   (supplicant :initarg :supplicant) (expt :initform (exp 1))
   (abbrev :allocation :class :initform "prioritizer")
   (frequency :initarg :frequency :initform 1/7))) ; FIXME: s/ll/sh/

(defmethod christen ((prioritizer prioritizer) (type (eql 'actor)))
  (slot-reduce prioritizer supplicant name)) ; this is starting to rhyme

(defun sufficiently-different? (new old) ; someday dispatch on market
  (< 0.04 (abs (log (/ (quantity (given new)) (quantity (given old)))))))

(defgeneric prioriteaze (ope target placed)
  (:method ((ope prioritizer) target placed &aux to-add (excess placed))
    (flet ((frob (add pop &aux (max (max (length add) (length pop))))
             (with-slots (expt) ope
               (let ((n (expt (random (expt max (/ expt))) expt)))
                 (awhen (nth (floor n) add) (ope-place ope it))
                 (awhen (nth (- max (ceiling n)) pop) (ope-cancel ope it))))))
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
    (multiple-value-bind (placed-bids placed-asks) (ope-placed prioritizer)
      (select ((recv next-bids next)
               (send response (prioriteaze prioritizer next placed-bids)))
              ((recv next-asks next)
               (send response (prioriteaze prioritizer next placed-asks)))
              (t (sleep frequency))))))

(defun profit-margin (bid ask &optional (bid-fee 0) (ask-fee 0))
  (abs (if (= bid-fee ask-fee 0) (/ ask bid)
           (/ (* ask (- 1 (/ ask-fee 100)))
              (* bid (+ 1 (/ bid-fee 100)))))))

;;; "plan to throw one away, for you already have"
;;; "plan two: yeet it like a neet"
;;; before undichotomising simultaneity and sequentialism,
;;; reframe Kolmogrov complexity as nonlinear metric
(defun dumbot-offers (foreigners        ; filtered to prevent overdamping
                      resilience        ; pq target offer depth to fill
                      funds             ; pq target total offer volume
                      epsilon           ; pq size of smallest order
                      max-orders        ; maximal offer count
                      magic             ; if you have to ask, you'll never know
                      &aux (acc 0.0) (share 0) (others (copy-list foreigners))
                        (asset (consumed-asset (first others))))
  (do* ((remaining-offers others (rest remaining-offers))
        (processed-tally    0    (1+   processed-tally)))
       ((or (null remaining-offers)  ; EITHER: processed entire order book
            ()                       ;  TODO : passed over enough liquidity
            (and (> acc resilience)  ;     OR:   BOTH: processed past resilience
                 (> processed-tally max-orders))) ; AND: at maximal order count
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
    ;; DONT use callbacks for liquidity distribution control
    (with-slots (volume) (first remaining-offers)
      (push (incf share (* 7/4 (incf acc volume))) (first remaining-offers)))))

(defclass ope-scalper (parent)
  ((input :initform (make-instance 'channel))
   (output :initform (make-instance 'channel))
   (abbrev :allocation :class :initform "ope")
   (frequency :initform 1/7 :initarg :frequency)
   (supplicant :initarg :supplicant) filter prioritizer
   (epsilon :initform (expt 0.14 5) :initarg :epsilon)
   (magic :initform 3 :initarg :magic-count)))

(defmethod christen ((ope ope-scalper) (type (eql 'actor)))
  (name (slot-value ope 'supplicant)))

(defun ope-sprinner (offers funds count magic bases punk dunk book)
  (if (or (null bases) (zerop count) (null offers)) offers ; c'est pas un bean
      (destructuring-bind (car . cdr) offers
        (multiple-value-bind (bases vwab cost)
            ;; what appears to be the officer, problem?
            ;; (bases-without bases (given top)) fails, because bids are `viqc'
            (bases-without bases (cons-aq* (consumed-asset car) (volume car)))
          (if (or (null bases) (zerop count) (null offers)) offers  ; again!
              (flet ((profit (o)
                       (funcall punk (1- (price o)) (price vwab) (cdar funds))))
                (when vwab
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
                                  punk dunk book))))))))

(defun ope-spreader (book resilience funds epsilon side ope)
  (flet ((dunk (book funds count magic &optional (start epsilon))
           (and book (dumbot-offers book resilience (caar funds)
                                    start (floor count) magic))))
    (with-slots (supplicant magic) ope
      (with-slots (order-slots) supplicant
        (awhen (dunk book funds (/ order-slots 2) magic)
          (ope-sprinner it funds (/ order-slots 2) magic
                        (bases-for supplicant (asset (given (first it))))
                        (destructuring-bind (bid . ask)
                            (recv (slot-reduce ope supplicant fee output))
                          (macrolet ((punk (&rest args)
                                       `(lambda (price vwab inner-cut)
                                          (- (* 100 (1- (profit-margin ,@args)))
                                             inner-cut))))
                            (ccase side ; give (sem #\N #\D) a chance!
                              (bids (punk price vwab bid))
                              (asks (punk vwab  price 0 ask)))))
                        #'dunk book))))))

;;; C-h k C-x ]
(defmethod perform ((ope ope-scalper) &key)
  ;; before you read any further, remember:
  ;; abstracting patterns with strictly fewer than THREE [ie (1+ 2!)]
  ;; instances is not only all integral roots of premature noptimals,
  ;; it also just makes you sound like a lad, a cad, and never a chad
  (with-slots (input output filter prioritizer epsilon frequency) ope
    (destructuring-bind (primary counter resilience ratio) (recv input)
      (with-slots (cut) filter
	(setf cut (complex (realpart cut)
			   (* (realpart cut) (atan (log ratio))))))
      (with-slots (next-bids next-asks response) prioritizer
        (macrolet ((do-side (amount side chan epsilon)
                     #+ () "can't I write documentation for local macros?"
                     `(let ((,side (copy-list (slot-value filter ',side))))
                        (unless (or (zerop (caar ,amount)) (null ,side))
                          (send ,chan (ope-spreader ,side resilience ,amount
                                                    ,epsilon ',side ope))
                          ;; M-x viper SPACE          ; why'dit haffter
                          (recv response)))))
          (let ((e (/ epsilon (+ 1/13 (abs (log (1+ (abs (log ratio)))))))))
            (do-side counter bids next-bids
              (* (max e epsilon) (abs (price (first bids))) (max ratio 1)
                 (expt 10 (- (decimals (market (first bids)))))))
            (do-side primary asks next-asks (* (max e epsilon) (max (/ ratio) 1)))
            ;; no, you may not write CL:DOCUMENTATION for macrolets,
            ;; you fine-source-reusing literati scum-bucket investor;
            ;; and keep those widths in check unless you want an "and mate"!
            )))) ; this line is fine though, plenty of sideband, and green, too!
    (send output (sleep frequency))))
;;; C-h k C-x [

(defmethod initialize-instance :after ((ope ope-scalper) &key cut)
  (with-slots (filter prioritizer supplicant) ope
    (macrolet ((init (slot &rest args)
                 `(setf ,slot (make-instance ',slot :supplicant supplicant
                                             :delegates `(,supplicant) ,@args)))
               (children (&rest slots)
                 `(progn ,@(mapcar (lambda (slot) `(adopt ope ,slot)) slots))))
      (children (init prioritizer) (init filter :cut cut)))))

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
         (chr (real)                ;_; was I a good function? // No.
           (funcall (if (plusp real) #'identity #'char-downcase)
                    (char "HWFUMONERYLSICAZJX"
                          (floor (* (abs real) #xFF) #xF)))))
    (with-output-to-string (out)
      (when (and (find "buy" trades :key #'direction :test #'string-equal)
                 (find "sell" trades :key #'direction :test #'string-equal))
        (let* ((min-sum (loop for trade in trades
                           for volume = (net-volume trade)
                           if (string-equal (direction trade) "buy")
                           sum volume into buy-sum
                           else sum volume into sell-sum
                           finally (return (min buy-sum sell-sum))))
               (min-last (apply 'min (mapcar #'side-last '("buy" "sell"))))
               (scale (expt (/ min-sum min-last) (/ (1+ length))))
               ;; FIXME: neither a sticky hocker nor a logical shocker be
               (dps (loop for i to length collect
                         (depth-profit (/ min-sum (expt scale i)))))
               (highest (apply #'max 0 (remove-if #'minusp dps)))
               (lowest  (apply #'min 0 (remove-if  #'plusp dps))))
          (format out "~4@$" (depth-profit min-sum))
          (dolist (dp dps (format out "~4@$" (first (last dps))))
            (format out "~C" (case (round (signum dp)) (0 #\Space)
                                   (+1 (chr (/ dp highest)))
                                   (-1 (chr (- (/ dp lowest))))))))))))

(defun makereport (maker fund rate btc doge investment risked skew &optional ha)
  (with-slots (name market ope snake last-report) maker
    (unless ha
      (let ((new-report (list (float btc) (float doge) ; approximate,
			      investment risked skew   ; intentionally.
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
      (aprog1 (format () "~&~A~A ~{~A~^ ~} ~5,4,,VF~4,4F~4,4@F~A~%"
                      name (format-timestring ; a naggy mess and lispy, too!
                            () (now) :format '((:hour 2) (:min 2) (:sec 2)))
                      (mapcar #'sastr '(primary counter primary counter)
                              `(,@#1=`(,fund ,(* fund rate)) ,btc ,doge))
                      ;; THE FOLLOWING LINES ARE SEVERE MATHEMATICAL AGONY!
                      #.(code-char (1+ (char-code #\9))) ; this loud n'ugh?
                      investment risked skew ; >= SU[1x2]? PL[3]? the usual
                      (apply 'profit-snake   ; approach is useless here...!
                             (slot-reduce ope supplicant lictor) snake))
        ;; Incidentally, the agony is due to numerical bases;
        ;; CL:FORMAT is perfectly fine, and mostly painless.
        (format t it) (if (channelp (first last-report))
                          (send (first last-report) it))
        ;; If I ever have to see three consecutive tildes, remind me that
        ;; I am not supposed to live beyond one third of a dozen decades.
        )))
  (force-output))

(defmethod perform ((maker maker) &key)
  (call-next-method maker :blockp ())   ; memento, du musste mori!
  (with-slots (fund-factor resilience-factor targeting-factor skew-factor
               market name ope cut supplicant) maker
    (let* ((trades (recv (slot-reduce market trades))) ; nananananana
           (balances (with-slots (sync) (slot-reduce maker treasurer)
                       (recv (send sync sync)))) ; excellent!
           (doge/btc (vwap market :depth 50 :type :sell)))
      (flet ((total-of (btc doge) (float (+ btc (/ doge doge/btc)))))
        (let* ((total-btc (asset-funds (primary market) balances))
               (total-doge (asset-funds (counter market) balances))
               (total-fund (total-of total-btc total-doge)))
          ;; history, yo!
          ;; this test originated in a harried attempt at bugfixing an instance
          ;; of Maybe, where the treasurer reports zero balances when the http
          ;; request (checking for balance changes) fails; due to use of aprog1
          ;; when the Right Thing's anaphoric. now that the bug's killed better,
          ;; Maybe thru recognition, the test remains; for when you lose the bug
          ;; don't lose the lesson, nor the joke.
          (unless (zerop total-fund)
            (let* ((buyin (dbz-guard (/ total-btc total-fund)))
                   (btc  (* fund-factor total-btc
                            buyin targeting-factor))
                   (doge (* fund-factor total-doge
                            (/ (- 1 buyin) targeting-factor)))
                   ;; status should always be \in (0,1)
                   (status (dbz-guard (/ (total-of    btc  doge) total-fund)))
                   ;; torque's range varies, depending on markets and climates
                   (torque (dbz-guard (/ (total-of (- btc) doge) total-fund)))
                   ;; this old formula has lost its spice; needs sigmoid clamp
                   (skew (log (if (zerop (* btc doge))
                                  (max 1/100
                                       (min 100
                                            (or (ignore-errors
                                                  (/ doge btc doge/btc)) 0)))
                                  (/ doge btc doge/btc)))))
              ;; ;; "Yeah, science!" - King of the Universe, Right Here
              ;; (when (= (signum skew) (signum (log targeting-factor)))
              ;;   (setf targeting-factor (/ targeting-factor)))
              ;; report funding
              (makereport maker total-fund doge/btc total-btc total-doge
                          buyin status torque)
              (flet ((f (g h) `((,g . ,(* cut (max 0 (* skew-factor h)))))))
                (send (slot-reduce ope input)
                      (list (f (min btc (* 2/3 total-btc)) skew)
                            (f (min doge (* 2/3 total-doge)) (- skew))
                            (* resilience-factor
                               (reduce #'max (mapcar #'volume trades)
                                       :initial-value 0))
                            (expt (exp skew) skew-factor)))
                (recv (slot-reduce ope output))))))))))

(defmethod initialize-instance :after ((maker maker) &key)
  (with-slots (supplicant ope delegates cut) maker
    (adopt maker supplicant) (push supplicant delegates)
    (adopt maker (setf ope (make-instance 'ope-scalper :cut cut
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
           (aif (remove side trades :key #'direction :test-not #'string-equal)
                (reduce #'aq+ (mapcar asset it)) 0)))
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
               (volume (reduce #'+ (mapcar #'volume trades)))
               (profit (* volume (1- (profit-margin (vwap "buy")
                                                    (vwap "sell"))) 1/2))
               (total (total (funds primary) (funds counter))))
	  (format t "~&I failed calculus, so why take my ~
                       word for any of these reckonings?~%")
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

;;; General Introspection, Major Mayhem, and of course SFC Property

(defmethod describe-object ((maker maker) (stream t))
  (with-slots (name print-args lictor) maker
    (apply #'print-book maker print-args) (performance-overview maker)
    (multiple-value-call 'format stream "~@{~A~#[~:; ~]~}" name
                         (trades-profits (slot-reduce lictor trades)))))

(defmethod describe-object :after ((maker maker) (stream t))
  (with-aslots (market) (slot-reduce maker supplicant)
    (describe-account it (exchange market) stream)))

;; (flet ((window (start trades)
;; 	 (if (null trades) (list start 0 nil)
;; 	   (multiple-value-call 'list start
;; 	     (length trades) (trades-profits trades)))))
;;   (loop with windows
;; 	for trades = (slot-reduce *maker* lictor trades)
;; 	then (nthcdr window-count trades) ; FUCK A DUCK A RANDOM DUCK
;; 	for window-start = (timestamp (first trades)) then window-close
;; 	for window-close = (timestamp- window-start 1 :day)
;; 	for window-count = (position window-close trades
;; 				     :key #'timestamp
;; 				     :test #'timestamp>=)
;; 	for window = (window window-start
;; 			     (if (null window-count) trades
;; 			       (subseq trades 0 window-count)))
;; 	while window-count do (push window windows)
;; 	finally (return (cons window windows))))

;; (defmethod describe-account :after (supplicant exchange stream)
;;   (destructuring-bind (first . rest) (slot-reduce supplicant lictor trades)
;;     (dolist (day (reduce (lambda (days trade)
;;                            (destructuring-bind ((day . trades) . rest) days
;;                              (let ((next (timestamp trade)))
;;                                (if (= (day-of next) (day-of day))
;;                                    (cons (list* day trade trades) rest)
;;                                    (acons next trade days)))))
;;                          rest :initial-value (acons (timestamp first) first ())))
;;       (multiple-value-call 'format stream "~&~@{~A~#[~:; ~]~}~%" name
;;                            (trades-profits day)))))
