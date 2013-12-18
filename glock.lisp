;;;; glock.lisp

(in-package #:glock)

;;; General Parameters
(defparameter +base-path+ "https://data.mtgox.com/api/2/")

(defparameter *nonce* 0)

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

;;; First frobs
(defun ticker (&key (pair "BTCUSD") fast)
  (let ((uri (concatenate 'string +base-path+ pair
                          (if fast
                              "/money/ticker_fast"
                              "/money/ticker"))))
    (json:decode-json (drakma:http-request uri :want-stream t))))

(defun hmac-sha512 (message secret)
  (let ((hmac (ironclad:make-hmac (base64:base64-string-to-usb8-array secret) 'ironclad:sha512)))
    (ironclad:update-hmac hmac (map '(simple-array (unsigned-byte 8) (*)) #'char-code message))
    (base64:usb8-array-to-base64-string (ironclad:hmac-digest hmac))))

(defun build-message (path data)
  (concatenate 'string path (string #\Null) (urlencode-params data)))

(defun make-signer (secret-path)
  (let ((secret (with-open-file (f secret-path) (read-line f))))
    (lambda (path data)
      (hmac-sha512 (build-message path data) secret))))

(define-condition mtgox-api-error (error)
  ((token :initarg :token)
   (error :initarg :error))
  (:report (lambda (condition stream)
             (with-slots (token error) condition
               (format stream "~A: ~A" token error)))))

(defun mtgox-path-request (path &rest keys)
  (let ((response (json:decode-json (apply #'drakma:http-request
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
