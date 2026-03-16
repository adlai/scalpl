;;; optimistic sasl authentication prototype

;;; usage:
;; (irc:connect :nickname "scalpl" :username "scalpl" :realname "scalpl"
;;              :server "chat.freenode.net" :port 6697 :connection-security :ssl
;;              :password (base64:string-to-base64-string
;;                         (concatenate 'string "scalpl" (string #\Null)
;;                                      "scalpl" (string #\Null)
;;                                      "suchsecretmuchwow"))
;;              :connection-type 'irc::sasl-connection)

(cl:cerror "Go ahead and monkey-patch it..." "CL-IRC does not support SASL")

(cl:in-package :cl-irc)

(defclass sasl-connection (connection) ())

(create-irc-message-classes
 (:cap :authenticate :rpl_loggedin :rpl_saslsuccess))

(pushnew '(900 :rpl_loggedin) *reply-names* :test #'equal)
(pushnew '(903 :rpl_saslsuccess) *reply-names* :test #'equal)

(defmethod pass ((connection sasl-connection) (password string))
  (add-hook connection 'irc-cap-message
            (lambda (message)
              (assert (equal (arguments message) '("*" "ACK" "sasl")))
              (send-irc-message connection "AUTHENTICATE PLAIN")))
  (add-hook connection 'irc-authenticate-message
            (lambda (message)
              (assert (equal (arguments message) '("+")))
              (send-irc-message
               connection (format () "AUTHENTICATE ~A" password))))
  (add-hook connection 'irc-rpl_saslsuccess-message
            (lambda (message) (send-irc-message connection "CAP END")))
  (send-irc-message connection "CAP REQ" "sasl"))
