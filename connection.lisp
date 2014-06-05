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
(defparameter +base-domain+ "https://api.kraken.com")

(defun hmac-sha512 (message secret)
  (let ((hmac (ironclad:make-hmac secret 'ironclad:sha512)))
    (ironclad:update-hmac hmac message)
    (base64:usb8-array-to-base64-string (ironclad:hmac-digest hmac))))

(defun hash-sha256 (message)
  (ironclad:digest-sequence :sha256 message))

;;; API-Key = API key
;;; API-Sign = Message signature using HMAC-SHA512 of (URI path + SHA256(nonce + POST data)) and base64 decoded secret API key

(defgeneric make-signer (secret)
  (:method ((secret string))
    (lambda (path data nonce)
      (hmac-sha512 (concatenate '(simple-array (unsigned-byte 8) (*))
                                (map '(simple-array (unsigned-byte 8) (*)) 'char-code path)
                                (hash-sha256 (map '(simple-array (unsigned-byte 8) (*))
                                                  'char-code
                                                  (concatenate 'string nonce (urlencode-params data)))))
                   (base64:base64-string-to-usb8-array secret))))
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

(defun decode-json (arg)
  (st-json:read-json arg))

(defun raw-request (path &rest keys)
  ;; (cond
  ;;   ((or (search "AddOrder" path)
  ;;        (search "CancelOrder" path)))
  ;;   ((or (search "Trades" path)
  ;;        (search "Ledgers" path))
  ;;    (format t "~&API counter +2 from ~A" path))
  ;;   (t (format t "~&API counter +1 from ~A" path)))
  (handler-case
      (multiple-value-bind (body status)
          (apply #'drakma:http-request
                 (concatenate 'string +base-path+ path)
                 ;; Mystery crash on the morning of 2014-06-04
                 ;; entered an infinite loop of usocket:timeout-error
                 ;; lasted for hours, continued upon restart
                 ;; other programs on the same computer not affected - just sbcl
                 :connection-timeout 60
                 keys)
        (case status
          (200 (with-json-slots (result error)
                   (decode-json (map 'string 'code-char body))
                 (values result error)))
          (502 (format t "~&Retrying after 502...~%")
               (sleep 1)
               (apply #'raw-request path keys))
          (t (cerror "Retry request" "HTTP Error ~D" status)
             (apply #'raw-request path keys))))
    (drakma::simple-error ()
      (format t "~&Retrying after drakma crap...~%")
      (sleep 1)
      (apply #'raw-request path keys))
    (cl+ssl::ssl-error-zero-return ()
      (format t "~&Retrying after cl+ssl crap...~%")
      (sleep 1)
      (apply #'raw-request path keys))
    (cl+ssl::ssl-error-syscall ()
      (format t "~&Retrying after cl+ssl crap...~%")
      (sleep 1)
      (apply #'raw-request path keys))
    (usocket:deadline-timeout-error ()
      (format t "~&Retrying after deadline timeout...~%")
      (sleep 1)
      (apply #'raw-request path keys))
    (usocket:timeout-error ()
      (format t "~&Retrying after regular timeout...~%")
      (sleep 1)
      (apply #'raw-request path keys))))

(defun get-request (path &optional data)
  (raw-request (concatenate 'string "public/" path "?"
                            (urlencode-params data))
               :parameters data))

(defun nonce (&aux (now (local-time:now)))
  (princ-to-string (+ (floor (local-time:nsec-of now) 1000)
                      (* 1000000 (local-time:timestamp-to-unix now)))))

(defun post-request (method key signer &optional data &aux (nonce (nonce)))
  (let ((path (concatenate 'string "/0/private/" method)))
    (push (cons "nonce" nonce) data)
    (raw-request (concatenate 'string "private/" method)
                 :method :post
                 :parameters data
                 :additional-headers `(("API-Key"  . ,key)
                                       ("API-Sign" . ,(funcall signer path data nonce))
                                       ("Content-Type" . "application/x-www-form-urlencoded")))))
