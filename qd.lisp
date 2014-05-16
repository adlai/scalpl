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

(defvar *last-track-time*)
(defvar *last-trade-id*)

(defvar *max-seen-trade* 30)

(defun track-trades (pair interval-seconds
                     &aux (now (timestamp-to-unix (now))))
  (when (or (not (boundp '*last-track-time*))
            (< interval-seconds (- now *last-track-time*)))
    (with-json-slots (last (trades pair))
        (get-request "Trades"
                     `(("pair" . ,pair)
                       ,@(when (boundp '*last-trade-id*)
                           `(("since" .,(princ-to-string *last-trade-id*))))))
      (when trades
        (let ((l (mapcar (lambda (trade)
                           (destructuring-bind (price volume time side kind data) trade
                             (let ((stamp (local-time:unix-to-timestamp (round time)))
                                   (vol (read-from-string volume)))
                               (list stamp vol (concatenate 'string side kind)))))
                         trades)))
          (setf *last-trade-id*   last
                *last-track-time* now
                *max-seen-trade*
                (reduce #'max
                        (sort (mapcar #'second
                                      (reduce (lambda (acc next)
                                                (let ((prev (first acc)))
                                                  (if (and (local-time:timestamp= (first prev) (first next))
                                                           (string= (third prev) (third prev)))
                                                      (cons (list (first prev)
                                                                  (+ (second prev) (second next))
                                                                  (third prev))
                                                            (cdr acc))
                                                      (cons next acc))))
                                              (cdr l) :initial-value (list (car l))))
                              #'>)
                        :initial-value *max-seen-trade*))))))
  (format t "~&~A largest seen trade: ~F (updated ~A)~%"
          (now) *max-seen-trade* (unix-to-timestamp *last-track-time*)))

(defun profit-margin (bid ask fee-percent)
  (* (/ ask bid) (- 1 (/ fee-percent 100))))

(defun dumbot-oneside (book resilience funds delta max-orders predicate
                       &aux (acc 0) (share 0))
  ;; calculate cumulative depths
  (do* ((cur book (cdr cur))
        (n 0 (1+ n)))
       ((> acc resilience)
        (let* ((sorted (sort (subseq book 1 n) #'> :key #'cddr))
               (n-orders (min max-orders n))
               (relevant (cons (car book) (subseq sorted 0 (1- n-orders))))
               (total-shares (reduce #'+ (mapcar #'car relevant))))
          (mapcar (lambda (order)
                    (let ((vol (* funds (/ (car order) total-shares))))
                      (cons vol (+ delta (cadr order)))))
                  (sort relevant predicate :key #'cddr))))
    ;; modifies the book itself
    (push (incf share (incf acc (cdar cur))) (car cur))
    ;; (format t "~&Found ~$ at ~D total ~$ share ~$~%"
    ;;         (cddar cur) (cadar cur) acc share)
    ))

(defun gapps-rate (from to)
  (getjso "rate" (read-json (drakma:http-request
                             "http://rate-exchange.appspot.com/currency"
                             :parameters `(("from" . ,from) ("to" . ,to))
                             :want-stream t))))

(defun %round (fund-factor resilience-factor
               &optional
                 (pair "XXBTXXDG") my-bids my-asks
               &aux
                 (market (getjso pair *markets*))
                 (decimals (getjso "pair_decimals" market))
                 (price-factor (expt 10 decimals)))
  ;; Track our resilience target
  (track-trades pair 25)
  ;; Get our balances
  (let ((balances (auth-request "Balance"))
        (resilience (* resilience-factor *max-seen-trade*))
        (doge/btc (read-from-string (second (getjso "p" (getjso pair (get-request "Ticker" `(("pair" . ,pair)))))))))
    (flet ((symbol-funds (symbol) (read-from-string (getjso symbol balances)))
           (total-of (btc doge) (+ btc (/ doge doge/btc)))
           (factor-fund (fund factor) (* fund fund-factor factor)))
      (let* ((total-btc (symbol-funds (getjso "base" market)))
             (total-doge (symbol-funds (getjso "quote" market)))
             (total-fund (total-of total-btc total-doge))
             (btc-fraction (/ total-btc total-fund))
             (btc (factor-fund total-btc btc-fraction))
             (doge (factor-fund total-doge (- 1 btc-fraction))))
        ;; report funding
        (format t "~&\"My cryptocurrency portfolio is ~$% invested in dogecoins\""
                (* 100 (- 1 btc-fraction)))
        (format t "~&very fund ~8$฿ + ~1$Ð ≈» ~8$฿~%"
                total-btc total-doge total-fund)
        (format t "~&such risk ~8$฿ + ~1$Ð ≈» ~8$฿ (~$% risked)~%"
                btc doge (total-of btc doge)
                (* 100 (/ (total-of btc doge) total-fund)))
        ;; report orders
        (format t "~A resilience: ~F" (now) resilience)
        (format t "~&bids @ ~{~D~#[~:; ~]~}~%" (mapcar #'cadr my-bids))
        (format t "~&asks @ ~{~D~#[~:; ~]~}~%" (mapcar #'cadr my-asks))
        ;; Now run that algorithm thingy
        ;; TODO: MINIMIZE OFF-BOOK TIME!
        ;; Get the current order book status
        (multiple-value-bind (asks bids) (get-book pair)
          ;; TODO: properly deal with partial and completed orders
          (let ((other-bids (ignore-mine bids (mapcar 'cdr my-bids)))
                (other-asks (ignore-mine asks (mapcar 'cdr my-asks))))
            ;; NON STOP PARTY PROFIT MADNESS
            (do* ((best-bid (caar other-bids) (caar other-bids))
                  (best-ask (caar other-asks) (caar other-asks))
                  (spread (profit-margin (1+ best-bid) (1- best-ask) 0.14)
                          (profit-margin (1+ best-bid) (1- best-ask) 0.14)))
                 ((> spread 1)
                  (format t "~&My estimated spread profit: ~D" spread))
              (pop other-bids)
              (pop other-asks)
              (format t "~&Dropping unprofitable spread: ~F from ~D to ~D~%"
                      spread best-bid best-ask))
            (let ((to-bid (dumbot-oneside other-bids resilience doge 1 15 #'>))
                  (to-ask (dumbot-oneside other-asks resilience btc -1 15 #'<))
                  new-bids new-asks)
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
                  (mapcar #'place to-ask))
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
              (values (sort (append my-bids
                                    (mapcar (lambda (order)
                                              (destructuring-bind (id quote-amount . price) order
                                                (list* id price
                                                       (* price-factor (/ quote-amount price)))))
                                            new-bids))
                            #'> :key #'cadr)
                      (sort (append my-asks
                                    (mapcar (lambda (order)
                                              (destructuring-bind (id quote-amount . price) order
                                                (list* id price quote-amount)))
                                            new-asks))
                            #'< :key #'cadr)))))))))

(defvar *fund-factor* 1)
(defvar *resilience-factor* 1)

(defvar *maker*
  ;; FIXME: this function is on the wishlist
  (chanl:pexec (:name "qdm-preα"
                :initial-bindings
                `((*read-default-float-format* double-float)
                  (*auth*
                   ,(cons (glock.connection::make-key #P "secrets/kraken.pubkey")
                          (glock.connection::make-signer #P "secrets/kraken.secret")))))
    (let (bids asks)
      (loop
         (setf (values bids asks)
               (%round *fund-factor*
                       *resilience-factor*
                       "XXBTZEUR" bids asks))))))

(defun trades-history (since &optional until)
  (getjso "trades"
          (auth-request "TradesHistory"
                        `(("start" . ,since) ("end" . ,until)))))
