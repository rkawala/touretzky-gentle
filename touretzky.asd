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
               (:file "ch2-tests")))
