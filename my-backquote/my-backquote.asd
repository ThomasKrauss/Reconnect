(defpackage :my-backquote-system (:use :cl :asdf))

(in-package :my-backquote-system)

(asdf:defsystem :my-backquote
  :name
  "my-backquote"
  :serial
  t
  :version
  "0.0.1"
  :depends-on
  nil
  :components
  ((:file "package")
   (:file "0-backquote")
   (:file "1-print")))

