;;;; glock.lisp

(defpackage #:glock.connection
  (:use #:cl #:glock.util)
  (:export #:mtgox-connection
           #:get-request
           #:post-request
           #:path #:pair #:data
           #:request))

(in-package #:glock.connection)

;;; General Parameters
(defparameter +base-path+ "https://data.mtgox.com/api/2/")

;;; Bastardized shamelessly from #'drakma::alist-to-url-encoded-string
(defun urlencode-params (params)
  (with-output-to-string (out)
    (loop for first = t then nil
       for (name . value) in params
       unless first do (write-char #\& out)
       do (format out "~A~:[~;=~A~]"
                  (drakma:url-encode name drakma::*drakma-default-external-format*)
                  value
                  (drakma:url-encode value drakma::*drakma-default-external-format*)))))

(defun hmac-sha512 (message secret)
  (let ((hmac (ironclad:make-hmac (base64:base64-string-to-usb8-array secret) 'ironclad:sha512)))
    (ironclad:update-hmac hmac (map '(simple-array (unsigned-byte 8) (*)) #'char-code message))
    (base64:usb8-array-to-base64-string (ironclad:hmac-digest hmac))))

(defun build-message (path data)
  (concatenate 'string path (string #\Null) (urlencode-params data)))

(defgeneric make-signer (secret)
  (:method ((secret string))
    (lambda (path data)
      (hmac-sha512 (build-message path data) secret)))
  (:method ((stream stream))
    (make-signer (read-line stream)))
  (:method ((path pathname))
    (with-open-file (stream path)
      (make-signer stream))))

(defgeneric make-key (key)
  (:method ((key string)) key)
  (:method ((stream stream)) (read-line stream))
  (:method ((path pathname))
    (with-open-file (stream path)
      (make-key stream))))

(define-condition mtgox-api-error (error)
  ((token :initarg :token)
   (error :initarg :error))
  (:report (lambda (condition stream)
             (with-slots (token error) condition
               (format stream "~A: ~A" token error)))))

(defun decode-json (json-stream)
  (json:with-decoder-simple-clos-semantics
    (json:bind-custom-vars
        (:array-type 'list)
      (json:decode-json json-stream))))

(defun mtgox-path-request (path &rest keys)
  (let ((response (decode-json (apply #'drakma:http-request
                                      (concatenate 'string +base-path+ path)
                                      :want-stream T
                                      :user-agent "Glockbot"
                                      keys))))
    (with-json-slots (result data error token) response
      (if (string= result "success") data
          (error 'mtgox-api-error :token token :error error)))))

(defun mtgox-get-request (path &optional data)
  (mtgox-path-request path :parameters data))

(defun mtgox-post-request (path key signer &optional data)
  (push (cons "tonce" (prin1-to-string (* 1000000 (local-time:timestamp-to-unix (local-time:now)))))
        data)
  (mtgox-path-request path
                      :method :post
                      :parameters data
                      :additional-headers `(("Rest-Key"  . ,key)
                                            ("Rest-Sign" . ,(funcall signer path data))
                                            ("Content-Type" . "application/x-www-form-urlencoded"))))

;;; Let's try an API object, like with BigQuery's OAuth2Client
(defclass mtgox-connection () ())
(defclass mtgox-authentication (mtgox-connection) (key signer))

(defmethod initialize-instance ((conn mtgox-connection) &key key secret)
  (when (and key secret)
    (change-class conn 'mtgox-authentication :key key :secret secret)))
(defmethod update-instance-for-different-class ((conn mtgox-connection)
                                                (auth mtgox-authentication)
                                                &key key secret)
  (setf (slot-value auth 'key)    (make-key    key)
        (slot-value auth 'signer) (make-signer secret)))

(defclass api-request ()
  ((path :initarg :path)
   (pair :initarg :pair :initform "BTCUSD")
   (data :initarg :data :initform '())))
(defclass get-request (api-request) ())
(defclass post-request (api-request) ())

(defgeneric request (connection method)
  (:method ((conn mtgox-connection) (request api-request))
    (error "~S must be either a GET-METHOD or a POST-METHOD" request))
  (:method ((conn mtgox-connection) (request get-request))
    (with-slots (path pair data) request
      (mtgox-get-request (format nil "~A/~A" pair path) data)))
  (:method ((conn mtgox-connection) (request post-request))
    (with-slots (path pair data) request
      (with-slots (key signer) conn
        (mtgox-post-request (format nil "~A/~A" pair path) key signer data)))))
