(defpackage :my-utilities-system (:use :cl :asdf))
(in-package :my-utilities-system)

(asdf:defsystem :my-utilities
  :name
  "my-utilities"
  :serial
  t
  :version
  "0.0.1"
  :depends-on (:cl-fad :cl-ppcre :my-backquote :st-json)
  :components
  ((:file "package")
   (:file "0-anaphora")
   (:file "1-util")
   (:file "2-environment")
   (:file "3-cl-complement")
   (:file "4-boolean")
   (:file "5-fad")
   (:file "6-plist")
   (:file "7-queue")
   (:file "8-json")))