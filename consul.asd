;;;; consul.asd

(asdf:defsystem #:consul
  :description "A CL library for working with consul."
  :author "Jeff Francis <jeff@gritch.org>"
  :license "MIT, see file LICENSE"
  :depends-on (#:drakma
               #:cl-json
               #:cl-base64
               #:babel)
  :serial t
  :components ((:file "package")
               (:file "consul")))

