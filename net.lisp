(defpackage #:scalpl.net
  (:use #:cl #:chanl #:scalpl.actor
        #:anaphora #:local-time #:scalpl.util
        #:websocket-driver-client)
  (:export #:http-request))

(in-package #:scalpl.net)

;;; HTTP (mostly a wrapper around Drakma)

(defun http-request (path &rest keys &aux (backoff 3))
  (loop (handler-case (return (apply #'drakma:http-request path keys))
          ((or simple-error drakma::drakma-simple-error
            usocket:ns-try-again-condition
            usocket:deadline-timeout-error usocket:timeout-error
            usocket:timeout-error usocket:ns-host-not-found-error
            end-of-file chunga::input-chunking-unexpected-end-of-file
            cl+ssl::ssl-error usocket:connection-refused-error)
              (error) (describe error) (sleep (incf backoff backoff))))))

;;; Websocket
