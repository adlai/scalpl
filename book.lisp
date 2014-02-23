;;;; utils.lisp

(defpackage #:glock.book
  (:use #:cl #:glock.util #:glock.connection #:st-json)
  (:export #:book))

(in-package #:glock.book)

(defun make-depth-channel (&optional (output (make-instance 'chanl:unbounded-channel)))
  (chanl:pexec ()
    (let ((glue (external-program:process-output-stream (external-program:start "node" '("-e" "(function(gox){gox.setup('pubnub');gox.connect('usd');gox.subscribe('depth');gox.on('depth',console.log)})(require('mtgox-orderbook'))") :output :stream))))
      ;; (read-line glue)                  ; connected to: wss://websocket.mtgox.com
      ;; (read-line glue)                  ; subscribing to channel: depth.BTCUSD
      (loop (chanl:send output (parse-depth-message (st-json:read-json glue)) :blockp nil))))
  ;; Block until we've started receiving depth messages
  (loop (unless (chanl:recv-blocks-p output) (return)))
  output)

(defun make-order-book-channel (&key
                                  (name "order book channel")
                                  (output (make-instance 'chanl:channel))
                                  (connection (make-instance 'mtgox-connection))
                                  (full nil))
  (values output
          (chanl:pexec (:name name)
            ;; make-depth-channel waits until the channel contains messages
            (let* ((depth (make-depth-channel (make-instance 'chanl:unbounded-channel)))
                   ;; we'll update this book every time orders are requested
                   (book (request connection (make-instance 'book :full full))))
              ;; updater loop
              (loop
                 (chanl:select
                   ;; try sending
                   ((chanl:send output book))
                   ;; or just update our book
                   ((chanl:recv depth depth)
                    ;; apply it
                    (setf book (apply-depth-message depth book)))))))))

(defun parse-depth-message (raw-jso)
  (with-json-slots (type_str price_int volume_int total_volume_int now)
      raw-jso ;; (getjso "depth" raw-jso)
    (jso "now" (goxstamp (parse-integer now))
         "type" (concatenate 'string type_str "s")
         "price" (parse-integer price_int)
         "delta" (parse-integer volume_int) ; TODO: Verify order book state
         "total" (parse-integer total_volume_int))))

;;; Talking to the API

(defclass book (get-request)
  (:default-initargs :full t))

(defmethod initialize-instance :around ((book book) &key full)
  (call-next-method book :path (format nil "money/depth/~:[fetch~;full~]" full)))

(defstruct (order (:constructor %make-order)) price amount time)

(defun make-order (raw-order)
  (with-json-slots (stamp price_int amount_int) raw-order
    (multiple-value-bind (sec nsec) (floor (parse-integer stamp) #.(expt 10 6))
      (%make-order :time (local-time:unix-to-timestamp sec :nsec nsec)
                   :price (parse-integer price_int)
                   :amount (parse-integer amount_int)))))

(defmethod request :around ((connection mtgox-connection) (method book))
  (with-json-slots (now bids asks) (call-next-method)
    (let ((asks (mapcar #'make-order asks))
          (bids (reduce (lambda (bids order) (cons (make-order order) bids))
                        bids :initial-value nil)))
      (jso "now" now
           "asks" (remove 1/100000000 asks :key #'order-amount)
           "bids" (remove 1/100000000 bids :key #'order-amount)))))

;;; This function is now thread safe!
(defun apply-depth-message (depth book)
  ;; First, pick apart the depth message
  (with-json-slots (now type price total) depth
    ;; Basic sanity
    (assert (member type '("asks" "bids") :test #'string=))
    ;; ;; Debugging
    ;; (if (zerop total)
    ;;     (format t "~&~D REM ~:[ASK~;BID~] @ ~10D"
    ;;             now (string= type "bids") price)
    ;;     (format t "~&~D ~:[ASK~;BID~] ~12D @ ~10D"
    ;;             now (string= type "bids") total price))
    (let (
          ;; We'll be updating one of the order lists later on, so we need to
          ;; create a copy of the order book object itself.
          (new-book  (jso "now"  (getjso "now"  book)
                          "asks" (getjso "asks" book)
                          "bids" (getjso "bids" book)))
          ;; Nothing special here, just the order we'll be inserting later
          (new-order (%make-order :price price :amount total :time now))
          ;; Lets us know when we find the right spot for the new order
          (test      (if (string= type "bids") #'> #'<)))
      (labels (
               ;; This helper function is used to express the following flow
               ;; recursively, instead of a terrible DO (or worse, LOOP)
               ;; TODO: use tail calls
               (updated-orders (remaining-orders &aux (first (first remaining-orders)))
                 (cond
                   ;; Did we reach the spot where this order belongs?
                   ((or (not remaining-orders) (funcall test price (order-price first)))
                    ;; Put it before them and return
                    (cons new-order remaining-orders))
                   ;; Does the depth change affect the first remaining order?
                   ((= price (order-price first))
                    ;; Is the data on the books more recent than this new order?
                    (if (local-time:timestamp< now (order-time first))
                        ;; If so, return the book unchanged
                        (return-from apply-depth-message book)
                        ;; If not, put the new data in place of the old
                        (cons new-order (rest remaining-orders))))
                   ;; Continue on down the order list
                   (t (cons first (updated-orders (rest remaining-orders)))))))
        (setf (getjso type new-book)
              (if (< total (expt 10 6)) ; anything less is dust or deletion
                  (remove price (getjso type book) :key 'order-price :count 1)
                  (updated-orders (getjso type book))))
        new-book))))

(defun spread (book)
  (- (order-price (first (getjso "asks" book)))
     (order-price (first (getjso "bids" book)))))

(defun truncate-to-size (book number-of-orders)
  (jso "bids" (subseq (getjso "bids" book) 0 number-of-orders)
       "asks" (subseq (getjso "asks" book) 0 number-of-orders)))

(defun print-book (book &optional (rows 5))
  (format t "     Sum        Size        Bid        Ask        Size         Sum~%")
  (setf rows (min rows (length (getjso "bids" book)) (length (getjso "asks" book))))
  (loop
     for bid in (subseq (getjso "bids" book) 0 rows)
     for ask in (subseq (getjso "asks" book) 0 rows)
     sum (order-amount bid) into bid-total
     sum (order-amount ask) into ask-total
     do (format t "~&~12D ~11D ~10D ~10D ~11D ~12D~%"
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
