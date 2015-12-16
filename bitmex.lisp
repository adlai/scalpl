(defpackage #:scalpl.bitmex
  (:nicknames #:bitmex) (:export #:*bitmex* #:bitmex-gate)
  (:use #:cl #:base64 #:chanl #:anaphora #:local-time #:scalpl.util
        #:scalpl.actor #:scalpl.exchange))

(in-package #:scalpl.bitmex)

;;; General Parameters
(defparameter +base-path+ "https://www.bitmex.com/api/v1/")

(defun hmac-sha256 (message secret)
  (let ((hmac (ironclad:make-hmac secret 'ironclad:sha256)))
    (ironclad:update-hmac hmac message)
    (ironclad:hmac-digest hmac)))

;;; api-nonce: A constantly increasing 64-bit integer. Each nonce can only be
;;; used once. Commonly, API consumers will start with 1 and increment per
;;; request, or use something simpler that is constantly increasing, like the
;;; current microsecond time.
;;;
;;; api-key: Your public API key
;;;
;;; api-signature: A signature of the request you are making. It is calculated
;;; as hex (HMAC_SHA256 (verb + url + nonce + data)).
;;;
;;; When multiple processes are using the same API key, requests may be
;;; received out of order and the nonce will not look like it's increasing
;;; from the server-side. In that case, you may send the header api-expires
;;; set to a UNIX timestamp in the future. The call, if replayed, will not be
;;; accepted if the current time is past the value in api-expires.
;;;
;;; This could potentially open you up to replay attacks for a short time if
;;; HTTPS were somehow broken, so choose a very small time in the future. We
;;; recommend less than a minute.
;;;
;;; If using api-expires, substitute the expires value for nonce in the HMAC
;;; construction above. The api-nonce value becomes optional and may be
;;; omitted. It will be ignored if provided.

(defgeneric make-signer (secret)
  (:method ((signer function)) signer)
  (:method ((array array)) (lambda (verb path data nonce) (error "TODO")))
  (:method ((string string))
    (make-signer (base64-string-to-usb8-array string)))
  (:method ((stream stream)) (make-signer (read-line stream)))
  (:method ((path pathname)) (with-open-file (data path) (make-signer data))))

(defgeneric make-key (key)
  (:method ((key string)) key)
  (:method ((stream stream)) (read-line stream))
  (:method ((path pathname))
    (with-open-file (stream path)
      (make-key stream))))

(defun post-request (method key signer &optional data &aux (nonce (nonce)))
  (error "TODO TOO"))
