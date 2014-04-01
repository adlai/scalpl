;;;; glock.lisp

(defpackage #:glock.connection
  (:use #:cl #:glock.util)
  (:export #:get-request
           #:post-request
           #:path #:pair #:data
           #:request))

(in-package #:glock.connection)

;;; General Parameters
(defparameter +base-path+ "https://api.kraken.com/0/")

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

(defun decode-json (json-stream)
  (st-json:read-json json-stream))

(defun raw-request (path &rest keys)
  (decode-json (apply #'drakma:http-request
                      (concatenate 'string +base-path+ path)
                      :want-stream T
                      :user-agent "agent smith"
                      keys)))

(defun get-request (path &optional data)
  (raw-request (concatenate 'string "public/" path "?"
                            (urlencode-params data))
               :parameters data))

(defun nonce ()
  (+ (* 1000 (local-time:timestamp-to-unix (local-time:now)))
     ;; hacky hack works on sbcl and ccl = good enough
     (floor (mod (get-internal-real-time) internal-time-units-per-second)
            (/ internal-time-units-per-second 1000))))

(defun post-request (path key signer &optional data)
  (push (cons "tonce" (prin1-to-string (+ (mod (get-internal-run-time) 1000)
                                          (* 1000000 (local-time:timestamp-to-unix (local-time:now))))))
        data)
  (raw-request (concatenate 'string "private/" path)
               :method :post
               :parameters data
               :additional-headers `(("Rest-Key"  . ,key)
                                     ("Rest-Sign" . ,(funcall signer path data))
                                     ("Content-Type" . "application/x-www-form-urlencoded"))))
