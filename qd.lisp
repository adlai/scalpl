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
              (format t "~&REM ~A~%" (getjso* "descr.order" order))
              (cancel-order id)))
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
              (post-limit type pair price (* volume price)
                          (apply #'concatenate 'string "viqc"
                                 (when options '("," options))))
              (format t "~&~A~%" message)))
        (progn
          (format t "~&ADD ~A~%" (getjso* "descr.order" info))
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
        (resilience (* resilience-factor *max-seen-vol*)))
    (flet ((factor-funds (symbol)
             (* fund-factor (read-from-string (getjso symbol balances)))))
      (let ((btc (factor-funds "XXBT"))
            (doge (factor-funds "XXDG")))
        ;; report funding
        (format t "~&Risking ~9,1F doge and ~6,3F btc~%" doge btc)
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

;;; (loop (%round 1/2 1/2) (sleep 30))
