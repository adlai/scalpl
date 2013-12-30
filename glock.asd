;;;; glock.asd

(asdf:defsystem #:glock
  :serial t
  :description "MtGox API"
  :author "Adlai Chandrasekhar <munchking@gmail.com>"
  :license "CC0"
  :depends-on (#:drakma
               #:cl-json
               #:cl-base64
               #:ironclad
               #:local-time)
  :components ((:file "util")
               (:file "connection" :depends-on ("util"))
               (:file "orders" :depends-on ("connection" "util"))
               (:file "glock" :depends-on ("connection" "orders"))
               (:file "calc" :depends-on ("util"))))

