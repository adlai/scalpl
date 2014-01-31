;;;; utils.lisp

(defpackage #:glock.order-book
  (:use #:cl #:glock.util #:glock.connection #:st-json)
  (:export #:order-book))

(in-package #:glock.order-book)

(defun make-depth-channel (&optional (output (make-instance 'chanl:channel)))
  (chanl:pexec ()
    (let ((glue (external-program:process-output-stream (external-program:start "node" '("-e" "require('goxstream').createStream({ticker: false, depth: true}).pipe(process.stdout)") :output :stream))))
      (read-line glue)
      (read-line glue)
      (loop (chanl:send output (st-json:read-json glue)))))
  output)

(defun depth-message-price (jso)
  (/ (parse-integer (getjso* "depth.price_int" jso)) #.(expt 10 5)))

(defun depth-message-volume (jso)
  (/ (parse-integer (getjso* "depth.volume_int" jso)) #.(expt 10 8)))

;;; Talking to the API

(defclass order-book (post-request)
  (:default-initargs :full t))

(defmethod initialize-instance :around ((book order-book) &key full)
  (call-next-method book :path (format nil "money/depth/~:[fetch~;full~]" full)))

(defstruct (order (:constructor %make-order)) price amount time)

(defun make-order (raw-order)
  (with-json-slots (stamp price_int amount_int) raw-order
    (multiple-value-bind (sec nsec) (floor (parse-integer stamp) #.(expt 10 6))
      (%make-order :time (local-time:unix-to-timestamp sec :nsec nsec)
                   :price (/ (parse-integer price_int) #.(expt 10 5))
                   :amount (/ (parse-integer amount_int) #.(expt 10 8))))))

(defmethod request :around ((connection mtgox-connection) (method order-book))
  (with-json-slots (now bids asks) (call-next-method)
    (let ((asks (mapcar #'make-order asks))
          (bids (reduce (lambda (bids order) (cons (make-order order) bids))
                        bids :initial-value nil)))
      (jso "now" now
           "asks" (remove 1/100000000 asks :key #'order-amount)
           "bids" (remove 1/100000000 bids :key #'order-amount)))))
