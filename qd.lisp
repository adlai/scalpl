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
(defun parse-price (price-string &optional decimals)
  (parse-integer (remove #\. price-string)))

(defun get-book (pair &optional count)
  (with-json-slots (bids asks)
      (getjso pair
              (get-request "Depth" `(("pair" . ,pair)
                                     ,@(when count
                                             `(("count" . ,(princ-to-string count)))))))
    (flet ((parse (raw-order)
             (destructuring-bind (price amount timestamp) raw-order
               (declare (ignore timestamp))
               (cons (parse-price price)
                     (read-from-string amount)))))
      (let ((asks (mapcar #'parse asks))
            (bids (mapcar #'parse bids)))
        (values asks bids)))))

;;; order-book and my-orders should both be in the same format:
;;; a list of (PRICE . AMOUNT), representing one side of the book
(defun ignore-mine (order-book my-orders &aux new)
  (print my-orders)
  (dolist (order order-book (nreverse new))
    (if (and my-orders (= (car order) (caar my-orders)))
        (let* ((mine (pop my-orders))
               (without-me (- (cdr order) (cdr mine))))
          (format t "~&At ~A, I have ~A, remain ~A"
                  (car order) (car mine) without-me)
          (unless (zerop without-me)
            (push (cons (car order) without-me) new)))
        (push order new))))

(defun open-orders ()
  (mapjso* (lambda (id order) (setf (getjso "id" order) id))
           (getjso "open" (auth-request "OpenOrders"))))

(defun cancel-order (id)
  (auth-request "CancelOrder" `(("txid" . ,id))))

(defun cancel-pair-orders (pair)
  (mapjso (lambda (id order)
            (when (string= pair (getjso* "descr.pair" order))
              (cancel-order id)
              (format t "~&wont ~A~%" (getjso* "descr.order" order))))
          (open-orders)))

(defun post-limit (type pair price volume &optional options)
  (multiple-value-bind (info errors)
      (auth-request "AddOrder"
                    (print `(("ordertype" . "limit")
                       ("type" . ,type)
                       ("pair" . ,pair)
                       ("volume" . ,(princ-to-string volume))
                       ("price" . ,(format nil "~$" price))
                       ,@(when options `(("oflags" . ,options)))
                       ;; ("validate" . "true")
                       )))
    (if errors
        (dolist (message errors)
          (if (and (search "volume" message)
                   (not (search "viqc" options)))
              (return
                (post-limit type pair price (* volume price)
                            (apply #'concatenate 'string "viqc"
                                   (when options '("," options)))))
              (format t "~&~A~%" message)))
        (progn
          (format t "~&want ~A~%" (getjso* "descr.order" info))
          (getjso "descr.txid" info)))))

(defvar *last-track-time*)
(defvar *last-candle-id*)

(defparameter *max-seen-vol* 0)

(defun track-vol (pair interval &aux (now (timestamp-to-unix (now))))
  (when (or (not (boundp '*last-candle-id*)) ; initial request
            (< (* interval 60)          ; convert minutes to seconds
               (- now *last-track-time*)))
    (with-json-slots (last (candles pair))
        (get-request "OHLC"
                     `(("pair" . ,pair)
                       ("interval" . ,(princ-to-string interval))
                       ,@(when (boundp '*last-candle-id*)
                               `(("since" . ,(princ-to-string *last-candle-id*))))))
      (setf *last-candle-id* last
            *last-track-time* now
            *max-seen-vol* (reduce 'max
                                   (mapcar 'read-from-string
                                           (map 'list 'seventh candles))
                                   :initial-value *max-seen-vol*))
      (format t "~&~A highest ~D minute volume - ~F"
              (unix-to-timestamp last)
              interval *max-seen-vol*))))

(defun dumbot-oneside (book resilience funds &optional (delta 0) &aux (acc 0))
  ;; calculate cumulative depths
  (do ((cur book (cdr cur))
       (n 0 (1+ n)))
      ((> acc resilience)
       (setf book (subseq book 0 n)))
    ;; modifies the book itself
    (push (incf acc (cdar cur))
          (car cur)))
  (mapcar (lambda (order)
            (let ((vol (* funds (/ (cddr order) acc))))
              (cons vol (+ delta (cadr order)))))
          book))

(defun gapps-rate (from to)
  (getjso "rate" (read-json (drakma:http-request
                             "http://rate-exchange.appspot.com/currency"
                             :parameters `(("from" . ,from) ("to" . ,to))
                             :want-stream t))))

(defun %round (fund-factor resilience-factor
               &optional (pair "XXBTXXDG") my-bids my-asks
                 &aux (info (getjso pair *markets*)))
  ;; Track our resilience target
  (track-vol pair 5)
  ;; Get our balances
  (let ((balances (auth-request "Balance"))
        (resilience (* resilience-factor *max-seen-vol*))
        (doge/btc (read-from-string (second (getjso "p" (getjso pair (get-request "Ticker" `(("pair" . ,pair)))))))))
    (flet ((symbol-funds (symbol) (read-from-string (getjso symbol balances)))
           (total-of (btc doge) (+ btc (/ doge doge/btc)))
           (factor-fund (fund factor) (* fund fund-factor factor)))
      (let* ((total-btc (symbol-funds (getjso "base" info)))
             (total-doge (symbol-funds (getjso "quote" info)))
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
        ;; Now run that algorithm thingy
        (time
         ;; TODO: MINIMIZE OFF-BOOK TIME!
         (progn
           ;; First, remove our liquidity, so we don't watch ourselves
           (cancel-pair-orders (getjso "altname" info))
           ;; Next, get the current order book status
           (multiple-value-bind (asks bids) (get-book pair)
             ;; Finally, inject our liquidity
             (values (mapc (lambda (order-info)
                             (post-limit "buy" pair
                                         (float (/ (cdr order-info) (expt 10 (getjso "pair_decimals" info))) 0d0)
                                         (car order-info) "viqc"))
                           (dumbot-oneside bids resilience doge 1))
                     (mapc (lambda (order-info)
                             (post-limit "sell" pair
                                         (float (/ (cdr order-info) (expt 10 (getjso "pair_decimals" info))) 0d0)
                                         (car order-info)))
                           (dumbot-oneside asks resilience btc -1))))))))))

(defvar *maker*
  (chanl:pexec (:name "qdm-preα"
                :initial-bindings
                `((*read-default-float-format* double-float)
                  (*auth*
                   ,(cons (glock.connection::make-key #P "secrets/kraken.pubkey")
                          (glock.connection::make-signer #P "secrets/kraken.secret")))))
    (let (bids asks)
      (loop
         (setf (values bids asks) (%round 4/5 1 "XXBTXXDG" bids asks))
         (pprint bids)
         (pprint asks)
         (sleep 40)))))
