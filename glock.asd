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
  :components ((:file "utils")
               (:file "connection" :depends-on ("utils"))
               (:file "orders" :depends-on ("connection" "utils"))
               (:file "glock" :depends-on ("connection" "orders"))))

