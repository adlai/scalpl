(defpackage #:scalpl.net
  (:use #:cl #:chanl #:scalpl.actor
        #:anaphora #:local-time #:scalpl.util
        #:websocket-driver-client)
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

(defun http-request (path &rest keys &key (backoff 3) &allow-other-keys)
  (loop (handler-case
            (return (apply #'drakma:http-request path
                           :user-agent "scalpl_301461a..00f612c/"
                           (alexandria:remove-from-plist keys :backoff)))
          ((or simple-error drakma::drakma-simple-error
               usocket:socket-condition	; most specific superclass...
	       usocket:ns-try-again-error  ; this one is a nuisance !
               chunga::input-chunking-unexpected-end-of-file
               cl+ssl::ssl-error
               end-of-file)
            (condition)
            (warn (with-output-to-string (warning)
                    ;; (describe condition warning)
                    (format warning "~A ... ~D"
                            (class-of condition)
                            (floor (integer-length backoff)
                                   (or (ignore-errors) 2)))))
            (if (zerop backoff) (return) ; might prevent nonce reuse...
                (sleep (incf backoff backoff))))))) ; probably won't

;;; Websocket

;;; there's a good chance that the sane priority is to leave the
;;; implementation of generic websocket functions until improving
;;; the hierarchical process control, sketched out in actor.lisp

;;; the alternative, where progress is made despite the Babylon
;;; of blocking bugs, unwritten specifications, and open sores,
;;; is to use this opportunity as a case study for the spec.
