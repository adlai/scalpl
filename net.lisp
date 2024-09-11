(defpackage #:scalpl.net
  (:use #:cl #:chanl #:scalpl.actor
        #:anaphora #:local-time #:scalpl.util
        #:websocket-driver-client)
  (:nicknames #:net ;) #:io #:oi
              )
  (:export #:pprint-json #:http-request))

(in-package #:scalpl.net)

;;; poached out of cjhunt; useful for debugging the sewage flooded by exchanges
;;; it only works for the alist representation, because that is least ambiguous

(defun pprint-json (*standard-output* json)
  (typecase json ; see cl-json's source, or at least util.lisp, for the format
    (complex (error "Congratulations, you are wasting your own time."))
    (real (format t "~A" json))  ; TODO: error instead of exceeding JS precision
    (list (pprint-logical-block (*standard-output* json :prefix "{" :suffix "}")
            (loop for (key . value) = (pop json) while key do
                 (pprint-logical-block (*standard-output* ())
                   (format t "\"~(~A~)\": ~@_" key)
                   (pprint-json *standard-output* value))
                 (when json (format t ", ") (pprint-newline :fill)))))
    (string (format t "~S" json))
    (vector (pprint-logical-block (nil nil :prefix "[" :suffix "]")
              (let ((end (length json)) (i 0))
                (when (plusp end)
                  (loop
                     (pprint-json *standard-output* (aref json i))
                     (if (= (incf i) end) (return nil))
                     (format t ", ")
                     (pprint-newline :fill))))))
    (t (format t "~(~A~)" json))))

;;; HTTP (mostly a wrapper around Drakma)
;;; TODO wrap Drakma in a bunch of abstractions that implement:
;;;   ordering, logging, rate limits, caching, and... spiders?

;;; Networking... dump it here, later should probably split into net.lisp

(defun http-request (path &rest keys &key (backoff 1) &allow-other-keys)
  (loop (handler-case                   ; est sed nihil potestandum erat
            (return (apply #'drakma:http-request path
                           :user-agent "scalpl_drakma^fuszen"
                           (alexandria:remove-from-plist keys :backoff)))
          ((or simple-error drakma::drakma-simple-error
            usocket:socket-condition	; most specific superclass...
            usocket:ns-try-again-error  ; least-informative-response!
            chunga::input-chunking-unexpected-end-of-file
            cl+ssl::ssl-error
            end-of-file)
            (condition)
            (unless (= backoff 1)    ; tolerate first timeout silently
              (warn (with-output-to-string (warning)
                      (format warning "~A ~A ~A #~D"
                              (now) (class-name (class-of condition))
                              path (integer-length backoff)))))
            (if (zerop backoff) (return) ; might prevent nonce reuse...
                (sleep (exp (incf backoff backoff)))))))) ; probably won't

;;; Websocket

;;; there's a good chance that the sane priority is to leave the
;;; implementation of generic websocket functions until improving
;;; the hierarchical process control, sketched out in actor.lisp

;;; the alternative, where progress is made despite the Babylon
;;; of blocking bugs, unwritten specifications, and open sores,
;;; is to use this opportunity as a case study for the spec.
