(defpackage #:scalpl.qd
  (:use #:cl #:chanl #:anaphora #:local-time #:split-sequence
        #:scalpl.util #:scalpl.actor #:scalpl.net
        #:scalpl.exchange #:scalpl.fx-feed)
  (:export #:ope-placed #:ope-place #:ope-cancel
           #:prioritizer #:prioriteaze
           #:next-bids #:next-asks
           #:estimated-profit
           #:maker #:restart-maker #:profit-margin #:print-args))

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
  (with-slots (control) (slot-value ope 'supplicant)
    (send control (list :offer offer))))

;;; response: trueish = offer no longer placed, nil = unknown badness
(defun ope-cancel (ope offer)
  (with-slots (control) (slot-value ope 'supplicant)
    (send control (list :cancel offer))))

(defclass filter (actor)
  ((abbrev :allocation :class :initform "filter") (cut :initarg :cut)
   (stink-tolerances :initarg :stink-tolerances :initform nil)
   (bids :initform ()) (asks :initform ()) (book-cache :initform nil)
   (supplicant :initarg :supplicant	; DISAMBIGUATION GONEHYET!
               :initform (error "must link supplicant"))
   (frequency  :initarg :frequency  :initform 1))) ; FIXME: s/ll/sh/!?

(defmethod christen ((filter filter) (type (eql 'actor)))
  (slot-reduce filter supplicant name))

;;; TODO: deal with partially completed orders ! [ cf Powell's warning ]
(defun ignore-offers (open mine &aux them)
  (dolist (offer open (nreverse them))
    (aif (find (price offer) mine :test #'= :key #'price)
         (let ((without-me (- (volume offer) (volume it))))
           (setf mine (remove it mine))
           (unless (< without-me 0.001)
             (push (make-instance 'offer
                                  :price (price offer) :volume without-me
                                  :market (slot-value offer 'market))
                   them)))
         (push offer them))))

;;; needs to do three different things
;;; 1) ignore-offers - fishes offers from linked supplicant
;;; 2) profitable spread - already does (via ecase spaghetti)
;;; 3) profit vs recent cost basis - done, shittily - TODO parametrize depth

(defgeneric filter (filter)
  (:method ((filter filter))
    ;; (declare (optimize (space 0) debug safety (speed 0)))
    (with-slots (supplicant book-cache cut) filter
      (let ((unstunk (cons (car book-cache) (cdr book-cache))))
        (with-slots (stink-tolerances) filter
          (when stink-tolerances
            (flet ((ignore-stink (book ratio)
                     (subseq book 0
                             (position (* ratio (price (first book)))
                                       book :key #'price :test #'<))))
              (macrolet ((trim-side (side stink)
                           `(when ,stink
                              (setf (,side unstunk)
                                    (ignore-stink (,side unstunk) ,stink)))))
                (destructuring-bind (fear . greed) stink-tolerances
                  (trim-side car fear) (trim-side cdr greed)))))
          (with-slots (offered fee) supplicant
            (destructuring-bind (bid . ask) (recv (slot-reduce fee output))
              (loop with rudder = (phase cut) with scale = (abs rudder)
                    for i from 0 for j = (1+ (floor (* i (- 1 (/ scale pi)))))
                    for a = (if (plusp rudder) j i)
                    for b = (if (plusp rudder) i j)
                    ;; do (break)
                    until (let ((offer-a (nth a (car unstunk)))
                                (offer-b (nth b (cdr unstunk))))
                            (or (null offer-a) (null offer-b)
                                (< (/ (realpart cut) 100)
                                   (1- (profit-margin (price offer-a)
                                                      (price offer-b)
                                                      bid ask)))))
                    finally (return (values
                                     (ignore-offers (nthcdr a (car unstunk))
                                                    offered)
                                     (ignore-offers (nthcdr b (cdr unstunk))
                                                    offered)))))))))))

(defmethod perform ((filter filter) &key)
  (with-slots (market book-cache bids asks frequency) filter
    (let ((book (recv (slot-reduce market book))))
      (unless (eq book book-cache)
        (setf book-cache book)
        (setf (values bids asks) (filter filter))))
    (sleep (/ frequency))))

(defclass feed-filter (filter)
  ((feed :initarg :feed)
   (ticker :reader ticker)
   (reciprocalp :reader reciprocalp)))

(defmethod shared-initialize :after ((filter feed-filter) names &key)
  (with-slots (feed market ticker reciprocalp) filter
    (setf (values ticker reciprocalp) (market-ticker market feed))))

(defmethod filter ((filter feed-filter))
  ;; (declare (optimize debug))
  (with-slots (market feed ticker reciprocalp) filter
    (let ((multiplier (expt 10 (decimals market))))
      ;; this test fails when connecting to dormant feeds,
      ;; e.g. during the weekend between FX sessions. it's
      ;; still a reasonable situation, you should be ready
      ;; for when the feed resumes when the session begins
      (if (zerop (hash-table-count (feed-table feed))) (call-next-method)
          (destructuring-bind
              (timestamp bid-volume bid-price mid-price ask-volume ask-price)
              (gethash ticker (feed-table feed))
            ;; the `timestamp' should be used for determining relevance;
            ;; some feed table could include stale data from a feed that
            ;; is no longer subscribed... or you compare multiple feeds?
            (declare (ignore timestamp bid-volume mid-price ask-volume))
            (let ((scaled-bid (if (not reciprocalp)
                                  (* multiplier bid-price)
                                  (/ multiplier ask-price)))
                  (scaled-ask (if (not reciprocalp)
                                  (* multiplier ask-price)
                                  (/ multiplier bid-price))))
              (multiple-value-bind (bids asks) (call-next-method)
                (values (member (- scaled-ask) bids :key 'price :test '<)
                        (member scaled-bid asks :key 'price :test '<)))))))))

(defclass static-filter (filter)
  ((bid-cutoff :initarg :bid-cutoff)
   (ask-cutoff :initarg :ask-cutoff)))

(defmethod filter ((filter static-filter))
  (let ((multiplier (expt 10 (decimals (market filter)))))
    (flet ((cutoff-beyond (offers config)
             (member (* multiplier config) offers :key 'price :test '<)))
      (macrolet ((maybe-cutoff (side slot)
                   `(if (not (slot-boundp filter ',slot)) ,side
                        (cutoff-beyond ,side (slot-value filter ',slot)))))
        (multiple-value-bind (bids asks) (call-next-method)
          (values (maybe-cutoff bids bid-cutoff)
                  (maybe-cutoff asks ask-cutoff)))))))

;;; TODO ... "PERSPECTIVES"
;;; distantly related to &environment except within trading context
;;; so rather than compile, load, and run being the possible times,
;;; perspectives deal with computation time, execution time, and...
;;; locations also relevant - probably superset of exchanges

(defclass prioritizer (actor)
  ((next-bids :initform (make-instance 'channel))
   (next-asks :initform (make-instance 'channel))
   (response :initform (make-instance 'channel))
   (supplicant :initarg :supplicant) (expt :initform (exp 1))
   (abbrev :allocation :class :initform "prioritizer")
   (frequency :initarg :frequency :initform 1))) ; FIXME: s/ll/sh/

(defmethod christen ((prioritizer prioritizer) (type (eql 'actor)))
  (slot-reduce prioritizer supplicant name)) ; this is starting to rhyme

(defgeneric prioriteaze (ope target placed)
  (:method ((ope prioritizer) target placed &aux to-add (excess placed))
    ;; (declare (optimize debug)) (break)
    (flet ((frob (add pop &aux (max (max (length add) (length pop))))
             (with-slots (expt) ope   ; Siri, what is Graham's number?
               (let ((n (expt (random (expt max (/ expt))) expt)))
                 (awhen (nth (- max (ceiling n)) pop) (ope-cancel ope it))
                 (awhen (nth (floor n) add) (ope-place ope it))))))
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
              (t (sleep (/ frequency)))))))

(defun profit-margin (bid ask &optional (bid-fee 0) (ask-fee 0))
  (abs (if (= bid-fee ask-fee 0) (/ ask bid)	; VALUES
           (/ (* ask (- 1 (/ ask-fee 100)))	; PREMATURE
              (* bid (+ 1 (/ bid-fee 100)))))))	; OPTIMISATION

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
                                          (warn "DEPTH; charge?") offers)
                                      #'> :key (lambda (x) (volume (cdr x))))
                                0 count)
                       #'< :key (lambda (x) (price (cdr x)))))
               (offer-scaler (total bonus count)
                 (let ((scale (/ funds (+ total (* bonus count)))))
                   (lambda (order &aux (vol (* scale (+ bonus (car order)))))
                     (with-slots (market price) (cdr order)
                       (make-instance
                        'offer
                        :given (cons-aq* asset vol)
                        :volume vol :market market
                        :price (with-slots (tick decimals) market
                                 (alet (if (slot-boundp market 'tick)
                                           (* tick (expt 10 decimals))
                                           1)
                                   (- price
                                      (* (if (> max-orders (expt magic magic))
                                             (1+ (random magic)) 1)
                                         it))))))))))
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
    ;; DONT forget why Let Over Lambda was named how it was!
    (with-slots (volume) (first remaining-offers)
      ;; BLESSED RNGESUS SHALLOWLY THY BLAME !!!
      (push (incf share (/ (incf acc volume) 5/7))
            ;; THY'ZDROVYE,MINE;GZUNDHYT
            (first remaining-offers)))))

(defclass ope-scalper (parent)
  ((input :initform (make-instance 'channel))
   (output :initform (make-instance 'channel))
   (abbrev :allocation :class :initform "ope")
   (frequency :initform 1/5 :initarg :frequency)
   (supplicant :initarg :supplicant) filter prioritizer
   (epsilon :initform 1e-3 :initarg :epsilon)
   (magic :initform 3 :initarg :magic-count)))

(defmethod christen ((ope ope-scalper) (type (eql 'actor)))
  (name (slot-value ope 'supplicant)))

(defun ope-sprinner (offers funds count bases punk dunk book)
  "accumulates liquidity provisioning from `funds' into `offers' along `book'"
  (if (or (null bases) (zerop count) (null offers)) offers ; tail recursion !!!
      (destructuring-bind (top . stink) offers ; each repetition gets stinkier!
        (multiple-value-bind (bases vwab cost) ; these triples need DEFMACRO..?
            ;; (bases-without bases (given top)) fails, because bids are `viqc'
            (bases-without bases (cons-aq* (consumed-asset top) (volume top)))
          ;; what appears to be the officer, problem? DEFUN CONSUMED-AQ* !!!
          (if (or (null bases) (zerop count) (null offers)) offers  ; again?
              (flet ((profit (o)
                       (funcall punk (1- (price o)) (price vwab) (cdar funds))))
                (when (and vwab (plusp (quantity vwab)))
                  (setf book (rest (member 0 book :test #'< :key #'profit))))
                (if (and vwab (plusp (profit top)))
                    `(,top .,(ope-sprinner
                              stink (destructuring-bind
                                        ((caar . cdar) . cdr) funds
                                      `((,(- caar (volume top)) .,cdar) .,cdr))
                              (1- count) bases punk dunk book))
                    (ope-sprinner (funcall dunk book funds count) funds
                                  count (and vwab `((,vwab ,(aq* vwab cost)
                                                           ,cost) ,@bases))
                                  punk dunk book))))))))

(defgeneric ope-spreader (ope book resilience funds epsilon side)
  (:documentation "allocates from `funds' along `book' by remaining arguments")
  (:method ((ope ope-scalper) book resilience funds epsilon side)
    (let ((slots (/ (slot-reduce ope supplicant order-slots) 2)))
      (flet ((dunk (book funds count &optional (start epsilon))
	       ;; (declare (optimize debug)) (break)
               (and book (dumbot-offers book resilience (caar funds)
                                        start (floor count)
                                        (slot-reduce ope magic)))))
        (awhen (dunk book funds slots)
          (ope-sprinner it funds slots
                        (bases-for (slot-reduce ope supplicant)
                                   (asset (given (first it))))
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
        (setf cut (complex (realpart cut) ; now your stanx wafted
                           (/ (realpart cut)  ; denumerodud propagate
                              (- (/ pi 2) (atan (log ratio)))))))
      (with-slots (next-bids next-asks response) prioritizer
        (macrolet ((do-side (amount side chan epsilon)
                     #+ () "can't I write documentation for local macros?"
                     `(let ((,side (copy-list (slot-value filter ',side))))
			;; (break)
                        (unless (or (zerop (caar ,amount)) (null ,side))
                          ;; bookmark dumb_bot hierarchies colimit responses
                          (send ,chan (ope-spreader ope ,side resilience
                                                    ,amount ,epsilon ',side))
                          ;; M-x viper SPACE          ; why'dit haffter
                          (recv response)))))
          (let ((e (/ epsilon (+ 1/13 (abs (log (1+ (abs (log ratio)))))))))
            (do-side counter bids next-bids
              (min (* (max e epsilon)
                      (abs (price (first bids)))
                      (max ratio 1)
                      (expt 10 (- (decimals (market (first bids))))))
                   (/ (caar counter) 7)))
            (do-side primary asks next-asks
              (min (* (max e epsilon) (max (/ ratio) 1))
                   (/ (caar primary) 7)))
            ;; no, you may not write CL:DOCUMENTATION for macrolets,
            ;; you fine-source-reusing literati scum-bucket investor;
            ;; and keep those widths in check unless you want an "and mate"!
            )))) ; this line is fine though, plenty of sideband, and green, too!
    (send output (sleep (/ frequency)))))
;;; C-h k C-x [

(defmethod initialize-instance :after
    ((ope ope-scalper) &key cut &allow-other-keys)
  (with-slots (filter prioritizer supplicant) ope
    (macrolet ((init (slot &rest args)
                 `(setf ,slot (make-instance ',slot :supplicant supplicant
                                             :delegates `(,supplicant) ,@args)))
               (children (&rest slots)
                 `(progn ,@(mapcar (lambda (slot) `(adopt ope ,slot)) slots))))
      (children (init prioritizer)
                (init filter :cut (abs cut))))))

;;;
;;; ACCOUNT TRACKING
;;;

(defclass maker (parent)
  ((fund-factor :initarg :fund-factor :initform 1)
   (resilience-factor :initarg :resilience :initform 1)
   (targeting-factor :initarg :targeting :initform (random 1.0))
   (skew-exponent :initarg :skew-factor :initform 1)
   (cut :initform 0 :initarg :cut) ope (supplicant :initarg :supplicant)
   (snake :initform (list 43))
   (print-args :initform '(:market t :ours t :wait () :count 7))
   (abbrev :initform "maker" :allocation :class)
   (last-report :initform nil))
  )

(defmethod christen ((maker maker) (type (eql 'actor)))
  (with-slots (gate market) (slot-reduce maker supplicant)
    (format () "~A.~A" (name market) (name gate))))

(defmethod print-object ((maker maker) stream)
  (print-unreadable-object (maker stream :type t :identity nil)
    (write-string (name maker) stream)))

(defmethod agent-trunk append ((maker maker))  ; HATCHIBOMBOTAR
  (with-slots (supplicant) maker
    (list* maker supplicant (agent-trunk supplicant)))) ; uses?

(defun profit-snake (lictor length)
  (let ((trades (slot-reduce lictor trades)))
    (flet ((depth-profit (depth)	; guah huagj WAG YOUR DOGG LION!
             (flet ((vwap (side) (vwap lictor :type side :depth depth)))
               (* 100 (1- (profit-margin (vwap "buy") (vwap "sell"))))))
           (side-last (side)
             (volume (find side trades :key #'direction
                                       :test #'string-equal)))
           (chr (real)               ;_; was I a good function? // No.
             (funcall (if (plusp real) #'identity #'char-upcase)
                      (char "hwfumonerylsicazjx" ; VTQPKGDB ???
                            (floor (* (abs real) #xFF) #xF)))))
      (with-output-to-string (out)
        ;; (fresh-line)                    ; READ JIHAN
        (when (and (find "buy" trades :key #'direction
                                      :test #'string-equal)
                   (find "sell" trades :key #'direction
                                       :test #'string-equal))
          (let* ((min-sum (loop for trade in trades ; progvit
                                for volume = (net-volume trade)
                                if (string-equal (direction trade) "buy")
                                  sum volume into buy-sum
                                else sum volume into sell-sum
                                finally (return (min buy-sum sell-sum))))
                 (mean-last (/ (+ (side-last "buy") (side-last "sell")) 2))
                 (scale (expt (/ min-sum mean-last) (/ (1+ length))))
                 ;; neither a sticky hocker nor a logical shocker be
                 (dps (loop for i to length
                            collect (depth-profit
                                     (/ min-sum (expt scale i)))))
                 (highest (apply #'max 0 (remove-if #'minusp dps)))
                 (lowest  (apply #'min 0 (remove-if  #'plusp dps))))
            (format out "~4@$ " (depth-profit min-sum))
            (dolist (dp dps (format out " ~4@$" (first (last dps))))
              (format out "~C" (case (round (signum dp)) (0 #\Space)
                                     (+1 (chr (/ dp highest)))
                                     (-1 (chr (- (/ dp lowest)))))))))))))

(defun estimated-profit (maker)
  (multiple-value-bind (vwap taken given)
      (trades-profits (slot-reduce maker lictor trades))
    (declare (ignore vwap))
    (with-slots (market) maker
      (let ((current-price (cons-mp* market (vwap market :since (timestamp- (now) 1 :day)))))
        (if (eq (primary market) (asset taken))
            (aq+ taken (aq* current-price given))
            (aq+ given (aq* current-price taken)))))))

(defun split-profit-estimates (maker)
  ;; quite possibly the worst code written in my life, SO FAR ...
  (multiple-value-bind (rtaken rgiven utaken ugiven)
      (loop for skip = nil then (cons (car tail) skip)
	    and tail on (sort (copy-list (slot-reduce maker lictor trades))
                              #'< :key #'volume) ; 4 14 32 461 512 537 OMG ...
	    for net-range-effect = (multiple-value-list (trades-profits tail))
	    when (null (first net-range-effect))
	      return (multiple-value-call 'values
		       (apply 'values (cdr net-range-effect))
		       (apply 'values (cdr (multiple-value-list
					    (trades-profits skip)))))
            finally (return-from split-profit-estimates
                      (with-slots (primary counter) (market maker)
                        (values (cons-aq primary 0) (cons-aq counter 0)
                                (estimated-profit maker)))))
    (with-slots (market) maker
      (let ((current-price (cons-mp* market (vwap market)))) ; default
        (values rtaken rgiven
		(if (eq (primary market) (asset utaken))
		    (aq+ utaken (aq* current-price ugiven))
		    (aq+ ugiven (aq* current-price utaken))))))))

(defun makereport (maker fund rate btc doge investment risked skew &optional ha)
  (with-slots (name market ope snake last-report cut) maker
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
      (aprog1 (format () "~&~@<~A ~A ~{~A~^ ~} ~5,4,,VF~4,4F~4,4@F~:_~A~:>~%"
                      name (format-timestring ; a naggy mess and lispy, too!
                            () (now) :format '((:hour 2) (:min 2) (:sec 2)))
                      (mapcar #'sastr '(counter primary counter primary) ;!
                              `(,doge ,btc ,@#1=`(,(* fund rate) ,fund)));
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
  ;; Some deployments might need the following form uncommented...
  (multiple-value-bind (primary counter unrealised)
      (split-profit-estimates maker)
    ;; TODO handle case where there is only unrealised, before any
    ;; "roundtrips" have been closed to generate realised profits.
    (format *debug-io* "~&Realised: [ ~A ~A ]   Unrealised: ~A~%"
            primary counter unrealised))
  (force-output))

(defmethod perform ((maker maker) &key)
  (declare (optimize debug))
  (call-next-method maker :blockp ())   ; memento, du musste mori!
  (with-slots (fund-factor resilience-factor targeting-factor skew-exponent
               market name ope cut supplicant) maker
    (with-slots (control response) supplicant
      (send control '(:sync)) (recv response))
    (let ((trades (recv (slot-reduce market trades)))
          (balances (with-slots (sync balances)
                        (slot-reduce maker treasurer)
                      (recv (send sync sync))
                      balances))
          ;; TODO: perhaps it's time for Exponentially Decaying Average?
          (doge/btc (vwap market)))     ; NEVER LINK CHAT ARCHIVE die !!
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
                            (if (zerop targeting-factor) 1
                                (/ (- 1 buyin) targeting-factor))))
                   ;; status should always be \in (0,1)
                   (status (dbz-guard (/ (total-of    btc  doge) total-fund)))
                   ;; torque's range varies, depending on markets and climates
                   (torque (dbz-guard (/ (total-of (- btc) doge) total-fund)))
                   ;; this old formula has lost its spice; needs sigmoid clamp
                   (skew (log (expt (if (zerop (* btc doge))
                                        (max 1/100
                                             (min 100
                                                  (or (ignore-errors
                                                       (/ doge btc doge/btc))
                                                      0)))
                                        (/ doge btc doge/btc))
                                    (/ fund-factor)))))
              ;; ;; "Yeah, science!" - King of the Universe, Right Here
              ;; (when (= (signum skew) (signum (log targeting-factor)))
              ;;   (setf targeting-factor (/ targeting-factor)))
              (flet ((f (g h) `((,g . ,(* cut (max 0 (* skew-exponent h)))))))
                (send (slot-reduce ope input)
                      (list (f (* 2/3 total-btc) skew)
                            (f (* 2/3 total-doge) (- skew))
                            (current-depth maker)
                            (expt (exp skew) skew-exponent)))
                (recv (slot-reduce ope output)))
              ;; print more obnoxious untabulated numeroscopy
              (makereport maker total-fund doge/btc total-btc total-doge
                          buyin status torque)
              ;; (throw :up (gensym "BARF"))
              )))))))

(defmethod initialize-instance :after
    ((maker maker) &rest keys &key &allow-other-keys)
  (with-slots (supplicant ope delegates cut) maker
    (adopt maker supplicant) (push supplicant delegates)
    (adopt maker (setf ope (apply 'make-instance 'ope-scalper
                                  :cut cut :supplicant supplicant keys)))))

(defmacro define-maker (lexeme &rest keys)
  (let* ((stem (etypecase lexeme	; COMMON-LISP:CHECK-TYPE pls
                 (symbol (string lexeme)) (string lexeme)))
         (name (let ((attack (elt stem 0))
                     (ending (elt stem (1- (length stem)))))
                 (or (cond	   ; SO MUCH THRUMB // NO WORK HELPING
                       ((and (char= attack ending)
                             (member attack '(#\* #\+ #|...|#)))
                        (string-trim "+*" stem))    ; problem ?
                       ((and (member attack '(#\< #\>))	; tell it to
                             (member ending '(#\< #\>))) ; your cafe
                        (cerror "Supply stem [sequence of base-char]"
                                "WHO ARE YOU & WHY THIS CODE")))
                     (error "GO TO FAIL, FO DIRECTIONS THAT FAILED")))))
    `(defvar ,lexeme (make-instance 'maker :name ,name ,@keys))))

(defmethod restart-maker ((maker maker))
  (with-slots (supplicant prioritizer) (slot-reduce maker ope)
    (with-slots (treasurer lictor) supplicant
      (flet ((maybe-kill (actor)
               (awhen (first (slot-reduce actor tasks))
                 (aand (task-thread it) (thread-alive-p it)
                       (kill it)))))
        (mapc #'maybe-kill
              (list maker prioritizer supplicant treasurer lictor
                    (slot-reduce lictor fetcher)
                    (slot-reduce maker ope)))
        (sleep 1/2) (reinitialize-instance maker)))))

(defgeneric current-depth (maker &key random-state)	       ; CLUNK
  (:method  ((maker maker) &key random-state)		       ; goes
    (with-slots (resilience-factor market) maker	       ;  the
      (with-slots (trades) (slot-reduce market trades-tracker) ; KNIFE
        (unless (or (null random-state) (eq random-state *random-state*))
          (warn "~&Don't run out of entropy, fool!~%"))
        (* (reduce #'max (mapcar #'volume trades) :initial-value 0)
           (+ (random (exp resilience-factor)) resilience-factor))))))

;;; the most important stage in AMNIOTE [chordate?] life is gastrulation

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

;;;; introspection commonly trivialised 'navel-gazing' ... -> navel.lisp
