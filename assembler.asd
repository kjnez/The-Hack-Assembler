;;;; assembler.asd

(asdf:defsystem #:assembler
  :description "Assembler for the Hack platform."
  :author "Gin Cui"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:uiop)
  :components ((:file "package")
               (:file "assembler")))
