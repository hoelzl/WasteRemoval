;;;; waste.asd

(asdf:defsystem #:waste
  :serial t
  :description "Waste-collecting robots"
  :version "0.0.1"
  :author "Matthias Hoelzl <tc@xantira.com>"
  :license "MIT, see file LICENSE"
  :depends-on (#:hrl)
  :components ((:file "waste-package")
               (:file "waste-env")
               (:file "waste-prog")
               (:file "waste-example")))
