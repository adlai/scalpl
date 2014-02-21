;;;; calc.lisp

(defpackage #:glock.calc
  (:use #:cl #:glock.util))

(in-package #:glock.calc)

;;; A position is composed of several balances in various currencies
(defclass position ()
  ((balances :initarg :balances)))

(defun position-value (position prices)
  (reduce #'+ (mapcar #'* (slot-value position 'balances) prices)))

(defun compare-position (position prev-price next-price)
  (- (position-value position next-price)
     (position-value position prev-price)))

;;; Calculate position necessary sale to balance position
(defun fight-tape (position next-price)
  (let ((target-value (position-value position next-price)))
    (mapcar (lambda (price) (/ target-value (* price 2))) next-price)))

(defgeneric sell (position price volume &key selling buying))
(defmethod sell ((position position) price volume &key (selling 0) (buying 1))
  (with-slots (balances) position
    (let ((out-balance (- (elt balances selling) volume))
          (in-balance  (+ (elt balances buying) (* price volume)))
          (new-balances (make-array (array-dimensions balances)
                                    :initial-contents balances)))
      (setf (elt new-balances selling) out-balance
            (elt new-balances buying)  in-balance)
      (make-instance 'position :balances new-balances))))

(defun sell-volume (position new-price &key selling buying)
  (with-slots (balances) position
    (/ (- (elt balances selling)
          (* new-price (elt balances buying)))
       (* 2 new-price))))

