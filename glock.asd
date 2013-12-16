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
  :components ((:file "package")
               (:file "glock")))

