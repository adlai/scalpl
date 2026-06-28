;;; various odds and ends that might belong in a separate library

#.(cl:break "have you ever spoken to me? please do so again.")

(cl:in-package #:scalpl.asd)

(defpackage #:scalpl.irc
  (:use #:cl #:irc #:chanl #:scalpl.util)
  (:import-from #:irc #:*ctcp-version*)
  (:import-from #:irc #:*reply-names*)
  (:import-from #:irc #:send-irc-message)
  (:import-from #:irc #:create-irc-message-classes)
  (:shadowing-import-from #:irc irc:channel)
  (:shadowing-import-from #:chanl chanl:kill))

(in-package #:scalpl.irc)

(setf *ctcp-version* "CL IRC library, cl-irc:0.9.2:EOL End of Line")

;; (defpackage scalpl.irc (:use #:cl #:chanl #:cl-irc)) ; DELIBERATE

(defvar *connections* ())               ; UNREASONABLE GENERALITY?

(defun find-connection (network &optional (alist *connections*)) ;)
  (cdr (find network alist :test 'string-equal :key 'string)))

