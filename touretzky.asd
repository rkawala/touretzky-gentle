;;;; touretzky.asd

(asdf:defsystem #:touretzky
  :description "Code & tests while reading Touretzky's book"
  :author "Rick Kawala <rkawala@gmail.com>"
  :license  "Private"
  :version "0.0.1"
  :serial t
  :depends-on (#:rove #:mockingbird)
  :components ((:file "package")
               (:file "touretzky")
               (:file "ch2-tests")
               (:file "ch3-tests")
               (:file "ch4-tests")
               (:file "ch5-tests")
               (:file "ch6-tests")))
