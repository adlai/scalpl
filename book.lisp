;;;; utils.lisp

(defpackage #:glock.book
  (:use #:cl #:glock.util #:glock.connection #:st-json)
  (:export #:book))

(in-package #:glock.book)

(defun make-depth-channel (&optional (output (make-instance 'chanl:channel)))
  (chanl:pexec ()
    (let ((glue (external-program:process-output-stream (external-program:start "node" '("-e" "require('goxstream').createStream({ticker: false, depth: true}).pipe(process.stdout)") :output :stream))))
      (read-line glue)                  ; connected to: wss://websocket.mtgox.com
      (read-line glue)                  ; subscribing to channel: depth.BTCUSD
      (loop (chanl:send output (parse-depth-message (st-json:read-json glue))))))
  output)

(defun parse-depth-message (raw-jso)
  (with-json-slots (type_str price_int volume_int total_volume_int now)
      (getjso "depth" raw-jso)
    (jso "now" (goxstamp (parse-integer now))
         "type" (concatenate 'string type_str "s")
         "price" (/ (parse-integer price_int) (expt 10 5))
         "delta" (/ (parse-integer volume_int) (expt 10 8)) ; TODO: Verify order book state
         "total" (/ (parse-integer total_volume_int) (expt 10 8)))))

;;; Talking to the API

(defclass book (post-request)
  (:default-initargs :full t))

(defmethod initialize-instance :around ((book book) &key full)
  (call-next-method book :path (format nil "money/depth/~:[fetch~;full~]" full)))

(defstruct (order (:constructor %make-order)) price amount time)

(defun make-order (raw-order)
  (with-json-slots (stamp price_int amount_int) raw-order
    (multiple-value-bind (sec nsec) (floor (parse-integer stamp) #.(expt 10 6))
      (%make-order :time (local-time:unix-to-timestamp sec :nsec nsec)
                   :price (/ (parse-integer price_int) #.(expt 10 5))
                   :amount (/ (parse-integer amount_int) #.(expt 10 8))))))

(defmethod request :around ((connection mtgox-connection) (method book))
  (with-json-slots (now bids asks) (call-next-method)
    (let ((asks (mapcar #'make-order asks))
          (bids (reduce (lambda (bids order) (cons (make-order order) bids))
                        bids :initial-value nil)))
      (jso "now" now
           "asks" (remove 1/100000000 asks :key #'order-amount)
           "bids" (remove 1/100000000 bids :key #'order-amount)))))

;;; This function is not thread safe!
(defun apply-depth-message (depth book)
  (with-json-slots (now type price total) depth
    (assert (member type '("asks" "bids") :test #'string=))
    (let ((order (print (find price (getjso type book) :key #'order-price))))
      ;; If there's a more recent order on the books, we discard the update
      (unless (and order (print (local-time:timestamp< now (order-time order))))
        (if (print (zerop total))
            ;; Delete order
            ;; FIXME: What if there's a more recent order on the books?
            (setf (getjso type book)
                  (delete price (getjso type book) :key #'order-price :count 1))
            ;; Add new / adjust existing
            (if order
                (setf (order-time order) now
                      (order-amount order) total)
                ;; Add new order
                (let ((test (if (string= type "bids") #'> #'<))
                      (new (%make-order :price price :amount total :time now)))
                  (if (funcall test price (order-price (first (getjso type book))))
                      ;; Add new "best" order
                      (push new (getjso type book))
                      ;; Add order at appropriate place in list
                      (do* ((tail (getjso type book) (cdr tail)))
                           ((or (funcall test price (order-price (car tail)))
                                (null (cdr tail)))
                            (push new (cdr tail))))))))))
    ;; Return the new order on the books
    (find (getjso "price" depth) (getjso type book) :key #'order-price)))

(defun spread (book)
  (- (order-price (first (getjso "asks" book)))
     (order-price (first (getjso "bids" book)))))

(defun truncate-to-size (book number-of-orders)
  (jso "bids" (subseq (getjso "bids" book) 0 number-of-orders)
       "asks" (subseq (getjso "asks" book) 0 number-of-orders)))

(defun print-book (book &optional (rows 5))
  (format t "~&WARNING: FLOAT COERCION~%")
  (format t "     Sum        Size        Bid        Ask        Size         Sum~%")
  (loop
     for bid in (subseq (getjso "bids" book) 0 rows)
     for ask in (subseq (getjso "asks" book) 0 rows)
     sum (order-amount bid) into bid-total
     sum (order-amount ask) into ask-total
     do (format t "~&~11,8F ~11,8F ~10,5F ~10,5F ~11,8F ~11,8F~%"
                bid-total (order-amount bid) (order-price bid)
                (order-price ask) (order-amount ask) ask-total)))

(defun bid-within-depth (depth book)
  (dolist (order (getjso "bids" book))
    (when (>= 0 (decf depth (order-amount order)))
      (return (+ (order-price order) (expt 10 -5))))))

(defun ask-within-depth (depth book)
  (dolist (order (getjso "asks" book))
    (when (>= 0 (decf depth (order-amount order)))
      (return (- (order-price order) (expt 10 -5))))))
