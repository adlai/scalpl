;;; various odds and ends that might belong in a separate library

#.(cl:break "have you ever spoken to me? please do so again.")

(defpackage scalpl.irc (:use #:cl #:chanl #:cl-irc)) ; DELIBERATE

(defvar *connections* ())               ; UNREASONABLE GENERALITY?

(defun find-connection (network &optional (alist *connections*)) ;)
  (cdr (find network alist :test 'string-equal :key 'string)))

