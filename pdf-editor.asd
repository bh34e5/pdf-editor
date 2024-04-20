(defsystem #:pdf-editor
  :description "A simple PDF editor"
  :version "0.0.1"
  :license "MIT" ; does this need more?
  :depends-on ("alexandria" "chipz")
  :serial t
  :components ((:file "packages")
               (:file "file-utils")
               (:file "objects")
               (:file "constants")
               (:file "reader")
               (:file "testing")))
