(defpackage :formatter-system (:use :cl :asdf))

(in-package :formatter-system)

(asdf:defsystem
 :formatter
 :name
 "formatter"
 :serial
 t
 :version
 "0.0.1"
 :depends-on
 (:my-utilities :my-systems :parenscript)
 :components
 ((:file "package")
  (:file "0-cl-formatter")))


