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
  (with-json-slots (result error)
      (decode-json (map 'string 'code-char
                        (apply #'drakma:http-request
                               (concatenate 'string +base-path+ path)
                               keys)))
    (values result error)))

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
