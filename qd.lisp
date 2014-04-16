;;;; qd.lisp - quick and dirty. kraken's api... wow

(defpackage #:glock.qd
  (:use #:cl #:st-json #:glock.util #:glock.connection))

(in-package #:glock.qd)

(defvar *auth*)

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

(defun dumbot-oneside (book resilience funds &aux (acc 0))
  ;; calculate cumulative depths
  (do ((cur book (cdr cur)))
      ((> acc resilience)
       ;; drop the tail
       (rplaca (rplacd cur nil) nil))
    ;; modifies the book itself
    (push (incf acc (cdar cur))
          (car cur)))
  (mapcar (lambda (order)
            (cons (* funds (/ (cddr order) acc))
                  (cadr order)))
           (print (butlast book))))
