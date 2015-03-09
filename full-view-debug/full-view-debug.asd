(defpackage :full-view-debug-system (:use :cl :asdf))

(in-package :full-view-debug-system)

(asdf:defsystem :full-view-debug
  :name
  "full-view-debug"
  :serial
  t
  :version
  "0.0.1"
  :depends-on
  (:my-utilities :my-systems :to-web)
  :components
  ((:file "package")
  (:file "1-debugger")
  (:file "2-debugging-view")
  (:file "3-special-debug")))

