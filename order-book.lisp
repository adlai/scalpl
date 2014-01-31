;;;; utils.lisp

(defpackage #:glock.order-book
  (:use #:cl #:glock.util #:glock.connection #:st-json))

(in-package #:glock.order-book)

(defun make-depth-channel (&optional (output (make-instance 'chanl:channel)))
  (chanl:pexec ()
    (let ((glue (external-program:process-output-stream (external-program:start "node" '("-e" "require('goxstream').createStream({ticker: false, depth: true}).pipe(process.stdout)") :output :stream))))
      (read-line glue)
      (read-line glue)
      (loop (chanl:send output (st-json:read-json glue)))))
  output)

(defun depth-message-price (jso)
  (/ (parse-integer (getjso* "depth.price_int" jso)) (expt 10 5)))

(defun depth-message-volume (jso)
  (/ (parse-integer (getjso* "depth.volume_int" jso)) (expt 10 8)))

;;; Talking to the API

(defclass order-book (post-request)
  (:default-initargs :full t))

(defmethod initialize-instance :around ((book order-book) &key full)
  (call-next-method book :path (format nil "money/depth/~:[fetch~;full~]" full)))

(defmethod request :around ((connection mtgox-connection) (method order-book))
  (let ((book (call-next-method)))
    (setf (getjso "bids" book)
          (reverse (getjso "bids" book)))
    book))

;;; Helper structs
(defstruct (order (:constructor nil)) price amount time)
(defstruct (bid (:include order)))
(defstruct (ask (:include order)))

(defun make-order (type raw-order)
  (with-json-slots (price--int amount--int stamp) raw-order
    (funcall (ccase type
               (bid #'make-bid)
               (ask #'make-ask))
             :amount (parse-integer amount--int)
             :price (parse-integer price--int)
             :time (parse-integer stamp))))

;;; Datastructure to store order book info

;;; How the order book itself should look:
;;; Two alists, one of bids, one of asks
;;; Each one is a list of ( price-int . amount-int )
;;; where amount-int is the total of all orders at that price

;; (defclass order-book ()
;;   ((full :initarg :full)
;;    currency-pair min-price max-price bids asks timestamp))

;; (macrolet ((define-order-adder (name accessor comparison)
;;              `(defun ,name (order book)
;;                 (if (,accessor book))))))

;; (defmethod initialize-instance ((book order-book) &key full (c1 :btc) (c2 :usd) pair)
;;   (with-slots (currency-pair data min-price max-price timestamp) book
;;     (setf currency-pair (or pair (currency-pair c1 c2)))
;;     (with-json-slots (bids asks now cached)
;;         (get-order-book :full full :pair currency-pair)
;;       (flet ((price-int (order) (parse-integer (slot-value order :price--int))))
;;         (setf max-price (reduce #'max asks :key #'price-int)
;;               min-price (reduce #'min bids :key #'price-int)
;;               timestamp (if full now cached)
;;               data (make-hash-table))
;;         (flet ((add-orders (orders type)
;;                  (loop
;;                     for order across orders
;;                     do (push (make-order type order)
;;                              (gethash (price-int order) data)))))
;;           (add-orders bids 'bid)
;;           (add-orders asks 'ask))))))
