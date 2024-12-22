(defpackage #:scalpl.fx-feed
  (:use #:cl #:chanl #:string-case #:anaphora #:local-time
	#:json #:scalpl.util #:scalpl.actor
        #:scalpl.net #:scalpl.exchange))

(in-package #:scalpl.fx-feed)

(defvar *websocket-url* "wss://api.tiingo.com/fx")

;;; TODO: fail gracefully if token is not found
(defvar *api-token*
  (with-open-file (file "secrets/tiingo")
    (read-line file)))

(defvar *subscription-id*)

(defun handle-websocket-raw (raw)
  (let* ((json:*json-array-type* 'vector)
         (json (read-json raw)))
    (flet ((arrange-json (json)
             (with-output-to-string (*standard-output*)
               (let ((*print-pretty* t)
                     (*print-right-margin* 67))
                 (pprint-json *standard-output* json)))))
      (with-json-slots ((type "messageType") data response) json
        (case (char type 0)
          (#\H (format t "~&~A WS Heartbeat~%" (now)))
          (#\I (when (= 200 (getjso "code" response))
                 (setf *subscription-id* (getjso "subscriptionId" data)))
           (format t "~&~A WS Info~%~A~%~A" (now)
                   (arrange-json data)
                   (arrange-json response)))
          (#\A (format t "~&~A WS Data~%~A" (now)
                       (arrange-json data)))
          (#\E (format t "~&~A WS Error~%~A" (now)
                       (arrange-json response)))
          (t (error "Unexpected WebSocket Response:~%~A" json)))))))

(defun build-websocket-json (alist)
  (with-output-to-string (string)
    (pprint-json string alist)))

;;; TODO: extract the pattern of Tiingo websocket request JSONs

(defun subscribe-auth (client token)
  (wsd:send client (build-websocket-json
                    `(("eventName" . "subscribe")
                      ("authorization" . ,token)
                      ;; TODO: is this the only allowed thresholdLevel ?
                      ("eventData" . (("thresholdLevel" . "5"))))))
  client)

(defun create-feed-websocket ()
  (let ((client (wsd:make-client *websocket-url*)))
    (wsd:on :message client 'handle-websocket-raw)
    (wsd:start-connection client)
    (subscribe-auth client *api-token*)))

(defun subscribe-tickers (client token id &optional (tickers #("*")))
  (wsd:send client (build-websocket-json
                    `(("eventName" . "subscribe")
                      ("authorization" . ,token)
                      ("eventData" . (("thresholdLevel" . 5)
                                      ("subscriptionId" . ,id)
                                      ("tickers" . ,tickers))))))
  client)

(defun unsubscribe-tickers (client token id &optional (tickers #("*")))
  (wsd:send client (build-websocket-json
                    `(("eventName" . "unsubscribe")
                      ("authorization" . ,token)
                      ("eventData" . (("thresholdLevel" . 5)
                                      ("subscriptionId" . ,id)
                                      ("tickers" . ,tickers))))))
  client)
