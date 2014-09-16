(defpackage #:scalpl.bitfinex
  (:use #:cl #:anaphora #:st-json #:base64 #:scalpl.util)
  (:export #:get-request
           #:post-request
           #:make-key #:make-signer))

(in-package #:scalpl.bitfinex)

;;; General Parameters
(defparameter +base-path+ "https://api.bitfinex.com/v1/")
(defparameter +base-domain+ "https://api.bitfinex.com")

(defun hmac-sha384 (message secret)
  (let ((hmac (ironclad:make-hmac secret 'ironclad:sha384)))
    (ironclad:update-hmac hmac message)
    (usb8-array-to-base64-string (ironclad:hmac-digest hmac))))

;;; X-BFX-APIKEY = API key
;;; X-BFX-PAYLOAD = base64(json(request path, nonce, parameters...))
;;; X-BFX-SIGNATURE = Message signature using HMAC-SHA384 of payload and base64 decoded secret

(defun nonce (&aux (now (local-time:now)))
  (princ-to-string (+ (floor (local-time:nsec-of now) 1000)
                      (* 1000000 (local-time:timestamp-to-unix now)))))

(defun make-payload (data &optional path)
  (let ((payload (if (null path) (jso) (jso "request" path "nonce" (nonce)))))
    (dolist (pair data (string-to-base64-string (write-json-to-string payload)))
      (destructuring-bind (key . val) pair (setf (getjso key payload) val)))))

(defgeneric make-signer (secret)
  (:method ((secret string))
    (lambda (payload)
      (string-downcase
       (format nil "~X"
               (base64-string-to-integer
                (hmac-sha384 (map '(simple-array (unsigned-byte 8) (*)) 'char-code payload)
                             (map '(simple-array (unsigned-byte 8) (*)) 'char-code secret)))))))
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
  (st-json:read-json (map 'string 'code-char arg)))

(defun raw-request (path &rest keys)
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
          (200 (decode-json body))
          ((400 404) (values nil (decode-json body)))
          (t (cerror "Retry request" "HTTP Error ~D" status)
             (apply #'raw-request path keys))))
    (drakma::drakma-simple-error ()
      (format t "~&Retrying after drakma SIMPLE crap...~%")
      (sleep 1)
      (apply #'raw-request path keys))
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
    (usocket:ns-host-not-found-error ()
      (format t "~&Retrying after nameserver crappage...~%")
      (sleep 1)
      (apply #'raw-request path keys))
    (usocket:deadline-timeout-error ()
      (format t "~&Retrying after deadline timeout...~%")
      (sleep 1)
      (apply #'raw-request path keys))
    (usocket:timeout-error ()
      (format t "~&Retrying after regular timeout...~%")
      (sleep 1)
      (apply #'raw-request path keys))
    ))

(defun get-request (path &optional data)
  (raw-request path :additional-headers `(("X-BFX-PAYLOAD" ,(make-payload data)))))

(defun post-request (method key signer &optional data)
  (let* ((path (concatenate 'string "/v1/" method))
         (payload (make-payload data path)))
    (raw-request method :method :post
                 :additional-headers `(("X-BFX-APIKEY"  . ,key)
                                       ("X-BFX-PAYLOAD" . ,payload)
                                       ("X-BFX-SIGNATURE" . ,(funcall signer payload))))))
