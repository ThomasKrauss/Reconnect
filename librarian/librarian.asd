(defpackage :librarian-system (:use :cl :asdf))

(in-package :librarian-system)

(asdf:defsystem :librarian
  :name
  "librarian"
  :serial
  t
  :version
  "0.0.1"
  :depends-on
  (:my-utilities :my-systems :my-lisp-parsing :to-web)
  :components
  ((:file "package")
   (:file "0-api-documentation")))

