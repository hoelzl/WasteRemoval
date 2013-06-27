;;;; waste.asd

(asdf:defsystem #:waste
  :serial t
  :description "Waste-collecting robots"
  :version "0.0.2"
  :author "Matthias Hoelzl <tc@xantira.com>"
  :license "MIT, see file LICENSE"
  :depends-on (#:hrl)
  :components ((:file "utils-package")
               (:file "epsilon-policy")
               (:file "simple-package")
               (:file "simple-env")
               (:file "simple-prog")
               (:file "simple-features")
               (:file "simple-example")
               (:file "waste-package")
               (:file "waste-env")
               (:file "waste-prog")
               (:file "waste-features")
               (:file "waste-example")))
