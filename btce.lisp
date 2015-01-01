(defpackage #:scalpl.btce
  (:nicknames #:btce)
  (:export #:*btce* #:btce-gate)
  (:use #:cl #:chanl #:anaphora #:st-json #:local-time #:scalpl.util #:scalpl.exchange))

(in-package #:scalpl.btce)

;;; General Parameters
(defparameter +base-path+ "https://btc-e.com/")
(defparameter +public-stub+ "api/3/")
(defparameter +private-stub+ "tapi")

(defun hmac-sha512 (message secret)
  (let ((hmac (ironclad:make-hmac secret 'ironclad:sha512)))
    (ironclad:update-hmac hmac message)
    (ironclad:hmac-digest hmac)))

;;; Key = API key
;;; Sign = POST-parameters (?nonce=1&param0=val0), signed with a Secret key using HMAC-SHA512

(defgeneric make-signer (secret)
  (:method ((signer function)) signer)
  (:method ((secret array))
    (lambda (data)
      (apply #'concatenate 'string
             (map 'list (lambda (byte) (string-downcase (format nil "~2,'0X" byte)))
                  (hmac-sha512 (map '(simple-array (unsigned-byte 8) (*)) 'char-code data)
                               secret)))))
  (:method ((secret string))
    (make-signer (map '(simple-array (unsigned-byte 8) (*)) 'char-code secret)))
  (:method ((stream stream)) (make-signer (read-line stream)))
  (:method ((path pathname)) (with-open-file (stream path) (make-signer stream))))

(defgeneric make-key (key)
  (:method ((key string)) key)
  (:method ((stream stream)) (read-line stream))
  (:method ((path pathname))
    (with-open-file (stream path)
      (make-key stream))))

;;; FIXME: URGENT NEED OF CLEANUP
;;; all this needs to be reflected in the exchange objects
;;;    to reduce code duplication. time for scalpl.net?

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
          (200 (read-json body))
          ;; (404 (with-json-slots (result error)
          ;;          (read-json (map 'string 'code-char body))
          ;;        (format t "~&Aborting after 404...~%")
          ;;        (describe error)
          ;;        (values result error))
          ;;      (values nil t))
          ;; (502 (format t "~&Retrying after 502...~%")
          ;;      (sleep 2)
          ;;      (apply #'raw-request path keys))
          (t (error "HTTP Error ~D~%~A" status body))))
    (nil (e)
      (describe e)
      (sleep 2)
      (apply #'raw-request path keys))))

(defun get-request (path &optional data)
  (raw-request (concatenate 'string +public-stub+ path "?"
                            (urlencode-params data))))

(defconstant +kludge+ -1419931763719)   ; sometimes one can!

(defun nonce (&aux (now (now)))
  (princ-to-string (+ (floor (nsec-of now) 1000000)
                      (* 1000 (timestamp-to-unix now))
                      +kludge+)))

(defun post-request (method key signer &optional params &aux (nonce (nonce)))
  (push (cons "method" method) params)
  (push (cons "nonce" nonce) params)
  (let ((data (urlencode-params params)))
    (raw-request (concatenate 'string +private-stub+)
                 :method :post :content data
                 :additional-headers `(("Key"  . ,key)
                                       ("Sign" . ,(funcall signer data))))))

(defclass btce-market (market)
  ((hidden :initarg :hidden :reader hidden)
   (epsilon :initarg :epsilon :reader epsilon)
   (fee :initarg :fee :reader fee)
   (minimum :initarg :minimum :reader minimum)
   (maximum :initarg :maximum :reader maximum)))

(defun get-info (&aux assets)
  (awhen (get-request "info")
    (flet ((ensure-asset (name)
             (or (find name assets :key #'name :test #'string=)
                 (aprog1 (make-instance 'asset :name name :decimals 8)
                   (push it assets)))))
      (values (mapcar-jso (lambda (pair data)
                            (with-json-slots ((epsilon  "min_amount")
                                              (maximum  "max_price")
                                              (minimum  "min_price")
                                              (decimals "decimal_places")
                                              fee hidden)
                                data
                              (make-instance 'btce-market :name pair
                                             :decimals decimals :fee fee
                                             :minimum minimum :maximum maximum
                                             :epsilon epsilon :hidden hidden
                                             :primary (ensure-asset (subseq pair 0 3))
                                             :counter (ensure-asset (subseq pair 4)))))
                          (getjso "pairs" it))
              assets))))

(defvar *btce*
  (multiple-value-bind (markets assets) (get-info)
    (make-instance 'exchange :name "BTC-e" :assets assets :markets markets)))
