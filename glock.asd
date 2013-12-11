;;;; glock.asd

(asdf:defsystem #:glock
  :serial t
  :description "MtGox API"
  :author "Adlai Chandrasekhar <munchking@gmail.com>"
  :license "CC0"
  :depends-on (#:drakma
               #:hunchentoot
               #:cl-json
               #:cl-base64)
  :components ((:file "package")
               (:file "glock")))

