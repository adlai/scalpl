;;;; glock.asd

(asdf:defsystem #:glock
  :serial t
  :description "MtGox API"
  :author "Adlai Chandrasekhar <munchking@gmail.com>"
  :license "CC0"
  :depends-on (#:drakma
               #:st-json
               #:cl-base64
               #:ironclad
               #:local-time)
  :serial t
  :components ((:file "util")
               (:file "connection")
               (:file "orders")
               (:file "order-book")
               (:file "calc")
               (:file "glock")))

