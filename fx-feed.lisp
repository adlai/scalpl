(defpackage #:scalpl.fx-feed
  (:use #:cl #:chanl #:string-case #:anaphora #:local-time
        #:scalpl.util #:scalpl.actor #:scalpl.numerics
        #:scalpl.net #:scalpl.exchange)
  (:export #:forex-feed #:tiingo-feed #:feed-table #:market-ticker))

(in-package #:scalpl.fx-feed)

;;; Feed Management Object

(defclass forex-feed (actor)
  ((url :reader url :initarg :url)
   (socket :accessor feed-socket))
  (:documentation "Superclass for websocket data feeds"))

;;; From https://www.tiingo.com/documentation/appendix/developers
;; Each user must have their own API token. Once a user registers
;; for Tiingo, guide them to this page to obtain their API Token:
;; "Account - API Token" https://www.tiingo.com/account/api/token

(defvar *tiingo-fx-firehose-url* "wss://api.tiingo.com/fx")

(defclass tiingo-feed (forex-feed)
  ((id :accessor subscription-id)
   (url :allocation :class :initform *tiingo-fx-firehose-url*)
   (tickers :reader tickers :initarg :tickers)
   (token :reader token :initarg :token
          :initform (error "must supply token! [string or pathname]"))
   (table :reader feed-table :initform (make-hash-table :test 'equal))))

;;; The best way to save your token is within the "secrets/" directory;
;;; then use something like the following at the REPL:
;; (defvar *feed*
;;   (make-instance 'tiingo-feed :token #p"secrets/tiingo"))

;;; copied from venues' make-key
(defgeneric make-token (key)
  (:method ((key string)) key)
  (:method ((stream stream)) (read-line stream))
  (:method ((path pathname))
    (with-open-file (stream path)
      (make-token stream))))

;;; FIXME: this pattern seems to break :default-initargs
(defmethod shared-initialize ((feed tiingo-feed) names &rest args &key token)
  (multiple-value-call #'call-next-method feed names
    (mvwrap token make-token) (apply 'values args)))

;;; TODO: include semicolons in a logical block prefix
(defun arrange-json (json)
  (with-output-to-string (*standard-output*)
    (let ((*print-pretty* t)
          (*print-right-margin* 67))
      (pprint-json *standard-output* json))))

(defun raw-websocket-handler (feed)
  (lambda (raw &aux (json (read-json raw)))
    (with-json-slots (data response (type "messageType")) json
      (case (char type 0)
        (#\H (format t "~&;; ~A WS Heartbeat~%" (now)))
        (#\I (when (= 200 (getjso "code" response))
               (awhen (getjso "subscriptionId" data)
                 (setf (subscription-id feed) it))
               (awhen (getjso "tickers" data)
                 (aand (coerce it 'list)
                       (setf (slot-value feed 'tickers) it))))
         (format t "~&;; ~A WS Info~%~A~%~A" (now)
                 (arrange-json data)
                 (arrange-json response)))
        (#\A (setf (gethash (elt data 1) (feed-table feed)) (subseq data 2))
         ;; (format t "~&;; ~A WS Data~%~A" (now)
         ;;             (arrange-json data))
         )
        (#\E (format t "~&;; ~A WS Error~%~A" (now)
                     (arrange-json response)))
        (t (error "Unexpected WebSocket Response:~%~A" json))))))

(defun build-websocket-json (alist)
  (with-output-to-string (string)
    (pprint-json string alist)))

;;; TODO: extract the pattern of Tiingo websocket request JSONs

(defun subscribe-auth (client token &optional tickers)
  (wsd:send client (build-websocket-json
                    `(("eventName" . "subscribe")
                      ("authorization" . ,token)
                      ;; TODO: is this the only allowed thresholdLevel ?
                      ("eventData" . (("thresholdLevel" . "5")
                                      ,@(when tickers
                                          `(("tickers" . ,tickers))))))))
  client)

(defun create-feed-websocket (feed)
  (with-slots (url token socket tickers) feed
    (setf socket (wsd:make-client url))
    (wsd:on :message socket (raw-websocket-handler feed))
    (wsd:start-connection socket)
    (if (slot-boundp feed 'tickers)
        (subscribe-auth socket token (coerce tickers 'vector))
        (subscribe-auth socket token))))

(defun subscribe-tickers (client token id &optional (tickers #("*")))
  (wsd:send client (build-websocket-json
                    `(("eventName" . "subscribe")
                      ("authorization" . ,token)
                      ("eventData" . (("thresholdLevel" . 5)
                                      ("subscriptionId" . ,id)
                                      ("tickers" . ,tickers))))))
  client)

;;; the default argument will unsubscribe from all tickers
;;; causing the remote server to disconnect the websocket.
(defun unsubscribe-tickers (client token id &optional (tickers #("*")))
  (wsd:send client (build-websocket-json
                    `(("eventName" . "unsubscribe")
                      ("authorization" . ,token)
                      ("eventData" . (("thresholdLevel" . 5)
                                      ("subscriptionId" . ,id)
                                      ("tickers" . ,tickers))))))
  client)

;;; Normalizing Market Name (currently only tested for Kraken and Tiingo)

(defgeneric market-ticker (market feed)
  (:method ((market market) (feed tiingo-feed))
    (with-slots (primary counter) market
      (flet ((trim-prefix (string)
               (if (char= #\Z (char string 0))
                   (subseq string 1) string)))
        (string-downcase (concatenate 'string
                                      (trim-prefix (name primary))
                                      (trim-prefix (name counter))))))))
