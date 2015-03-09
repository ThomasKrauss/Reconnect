(defpackage :my-systems-system (:use :cl :asdf))
(in-package :my-systems-system)

(asdf:defsystem :my-systems
  :name
  "my-systems"
  :serial
  t
  :version
  "0.0.1"
  :depends-on (:my-utilities)
  :components
  ((:file "package")
   (:file "0-layout")
   (:file "1-read-and-print")
   (:file "2-load-and-save")
   (:file "3-management")
   (:file "4-documentation")
   (:file "5-backup")
   (:file "6-external-programs")
   (:file "7-wrap-up")))