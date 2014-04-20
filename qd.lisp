;;;; qd.lisp - quick and dirty. kraken's api... wow

(defpackage #:glock.qd
  (:use #:cl #:st-json #:glock.util #:glock.connection))

(in-package #:glock.qd)

(defvar *auth*)

(defun auth-request (path &optional options)
  (when *auth* (post-request path (car *auth*) (cdr *auth*) options)))

(defun get-markets (&aux (pairs (get-request "AssetPairs")))
  (mapjso (lambda (name data)
            (setf (getjso "name" data) name))
          pairs)
  pairs)

(defun get-book (pair &optional count)
  (with-json-slots (bids asks)
      (getjso pair
              (get-request "Depth" `(("pair" . ,pair)
                                     ,@(when count
                                             `(("count" . ,(princ-to-string count)))))))
    (flet ((parse (raw-order)
             (destructuring-bind (price amount timestamp) raw-order
               (declare (ignore timestamp))
               (cons (read-from-string price)
                     (read-from-string amount)))))
      (let ((asks (mapcar #'parse asks))
            (bids (mapcar #'parse bids)))
        (values asks bids)))))

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
                    `(("ordertype" . "limit")
                      ("type" . ,type)
                      ("pair" . ,pair)
                      ("volume" . ,(princ-to-string volume))
                      ("price" . ,(princ-to-string price))
                      ,@(when options `(("oflags" . ,options)))
                      ;; ("validate" . "true")
                      ))
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

(defparameter *last-candle-id* 0)

(defparameter *max-seen-vol* 0)

(defun track-vol (pair interval)
  (with-json-slots (last (candles pair))
      (get-request "OHLC"
                   `(("pair" . ,pair)
                     ("interval" . ,(princ-to-string interval))))
    (setf *last-candle-id* last
          *max-seen-vol* (reduce 'max
                                 (mapcar 'read-from-string
                                         (map 'list 'seventh candles)))))
  (format t "~&~A highest ~D minute volume - ~F"
          (local-time:unix-to-timestamp *last-candle-id*)
          interval *max-seen-vol*))

(defparameter *max-seen-vol*
  (or (and (boundp '*max-seen-vol*) *max-seen-vol*)
      (track-vol "XXBTXXDG" 5)))

(defun dumbot-oneside (book resilience funds &aux (acc 0))
  ;; calculate cumulative depths
  (do ((cur book (cdr cur))
       (n 0 (1+ n)))
      ((> acc resilience)
       (setf book (subseq book 0 n)))
    ;; modifies the book itself
    (push (incf acc (cdar cur))
          (car cur)))
  (mapcar (lambda (order)
            (cons (* funds (/ (cddr order) acc))
                  (cadr order)))
          book))

(defun gapps-rate (from to)
  (getjso "rate" (read-json (drakma:http-request
                             "http://rate-exchange.appspot.com/currency"
                             :parameters `(("from" . ,from) ("to" . ,to))
                             :want-stream t))))

(defun %round (fund-factor resilience-factor)
  ;; Track our resilience target
  (track-vol "XXBTXXDG" 5)
  ;; Get our balances
  (let ((balances (auth-request "Balance"))
        (resilience (* resilience-factor *max-seen-vol*))
        (doge/btc (read-from-string (second (getjso* "XXBTXXDG.p" (get-request "Ticker" '(("pair" . "XXBTXXDG"))))))))
    (flet ((symbol-funds (symbol) (read-from-string (getjso symbol balances)))
           (total-of (btc doge) (+ btc (/ doge doge/btc)))
           (factor-fund (fund factor) (* fund fund-factor factor)))
      (let* ((total-btc (symbol-funds "XXBT"))
             (total-doge (symbol-funds "XXDG"))
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
           (cancel-pair-orders "XBTXDG")
           ;; Next, get the current order book status
           (multiple-value-bind (asks bids) (get-book "XXBTXXDG")
             ;; Finally, inject our liquidity
             (mapc (lambda (info)
                     (post-limit "buy" "XXBTXXDG" (+ (cdr info) 0.1) (car info) "viqc"))
                   (dumbot-oneside bids resilience doge))
             (mapc (lambda (info)
                     (post-limit "sell" "XXBTXXDG" (- (cdr info) 0.1) (car info)))
                   (dumbot-oneside asks resilience btc)))))))))

(defvar *maker*
  (chanl:pexec (:name "qdm-preα"
                :initial-bindings
                `((*auth*
                   ,(cons (glock.connection::make-key #P "secrets/kraken.pubkey")
                          (glock.connection::make-signer #P "secrets/kraken.secret")))))
    (loop (%round 1 1/2) (sleep 45))))
