;;;; utils.lisp

(defpackage #:glock.book
  (:use #:cl #:glock.util #:glock.connection #:st-json)
  (:export #:book))

(in-package #:glock.book)

(defun make-depth-channel (&optional (output (make-instance 'chanl:unbounded-channel)))
  (chanl:pexec ()
    (let ((glue (external-program:process-output-stream (external-program:start "node" '("-e" "require('pubnub').init({subscribe_key:'sub-c-50d56e1e-2fd9-11e3-a041-02ee2ddab7fe',ssl:true}).subscribe({channel:'24e67e0d-1cad-4cc0-9e7a-f8523ef460fe',message:function(m){console.log(m)},connect:function(){console.log('connected to depth')}})") :output :stream))))
      (chanl:send output (read-line glue))
      (loop (chanl:send output (parse-depth-message (st-json:read-json glue)) :blockp t))))
  output)

(defun make-trade-channel (&optional (output (make-instance 'chanl:unbounded-channel)))
  (chanl:pexec ()
    (let ((glue (external-program:process-output-stream (external-program:start "node" '("-e" "require('pubnub').init({subscribe_key:'sub-c-50d56e1e-2fd9-11e3-a041-02ee2ddab7fe',ssl:true}).subscribe({channel:'dbf1dee9-4f2e-4a08-8cb7-748919a71b21',message:function(m){console.log(m)},connect:function(){console.log('connected to trade')}})") :output :stream))))
      (chanl:send output (read-line glue))
      (loop (chanl:send output (parse-depth-message (st-json:read-json glue)) :blockp t))))
  output)

(defun make-order-book-channel (&key
                                  (name "order book channel")
                                  (output (make-instance 'chanl:channel))
                                  (connection (make-instance 'mtgox-connection))
                                  (full nil))
  (values output
          (chanl:pexec (:name name)
            (let ((depth (make-depth-channel))
                  (trade (make-trade-channel)))
              ;; we have to buffer both streams before requesting the book
              (loop
                 (when (and (not (chanl:recv-blocks-p depth))
                            (not (chanl:recv-blocks-p trade)))
                   (return)))
              ;; we'll update this book every time orders are requested
              (book (request connection (make-instance 'book :full full)))
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
  (if (string= "trade" (getjso "private" raw-jso))
      (with-json-slots (trade_type price_int amount_int properties tid primary)
          (getjso "trade" raw-jso)
        (format t "~&~D ~:[BUY~;SELL~] ~12D @ ~D ~A ~A"
                (goxstamp (parse-integer tid))
                (string= "ask" trade_type) amount_int price_int
                properties primary)
        (jso "now" (goxstamp (parse-integer tid))
             "type" (concatenate 'string trade_type "s")
             "price" (parse-integer price_int)
             "delta" (- (parse-integer amount_int))
             "total" (when (string= properties "market") 0)))
      (with-json-slots (type_str price_int volume_int total_volume_int now)
          (getjso "depth" raw-jso)
        (jso "now" (goxstamp (parse-integer now))
             "type" (concatenate 'string type_str "s")
             "price" (parse-integer price_int)
             "delta" (parse-integer volume_int) ; TODO: Verify order book state
             "total" (parse-integer total_volume_int)))))

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
           "asks" (remove (expt 10 6) asks :key #'order-amount :test #'>)
           "bids" (remove (expt 10 6) bids :key #'order-amount :test #'>)))))

;;; This function is now thread safe!
(defun apply-depth-message (depth book)
  ;; First, pick apart the depth message
  (with-json-slots (now type price total delta) depth
    ;; Basic sanity
    (assert (member type '("asks" "bids") :test #'string=))
    ;; ;; Debugging
    ;; (if (or (null total) (zerop total))
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
          ;; Lets us know when we find the right spot for the new order
          (before    (if (string= type "bids") #'> #'<)))
      (flet ((new-order (total) (%make-order :price price :amount total :time now)))
        (labels (
                 ;; This helper function is used to express the following flow
                 ;; recursively, instead of a terrible DO (or worse, LOOP)
                 ;; TODO: use tail calls
                 (updated-orders (remaining-orders &aux (first (first remaining-orders)))
                   (cond
                     ;; Did we reach the spot where this order belongs?
                     ((or (not remaining-orders) (funcall before price (order-price first)))
                      ;; Put it before them and return
                      (cons (new-order total) remaining-orders))
                     ;; Does the depth change affect the first remaining order?
                     ((= price (order-price first))
                      ;; Build the appropriate modification
                      (cons (new-order (if (not total)
                                           (+ delta (order-amount first))
                                           total))
                            (rest remaining-orders)))
                     ;; Continue on down the order list
                     (t (cons first (updated-orders (rest remaining-orders)))))))
          (setf (getjso type new-book)
                (if (or (not total) (< total (expt 10 6))) ; dust or deletion
                    (remove price (getjso type book) :key 'order-price :count 1)
                    (updated-orders (getjso type book))))
          new-book)))))

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
