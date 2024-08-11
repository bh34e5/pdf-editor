(defsystem #:pdf-editor
  :description "A simple PDF editor"
  :version "0.0.1"
  :license "MIT" ; does this need more?
  :depends-on ("alexandria")
  :serial t
  :components ((:file "packages")
               (:file "utils")
               (:file "reader")
               (:file "objects")))
