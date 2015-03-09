(defpackage :overview-system (:use :cl :asdf))

(in-package :overview-system)

(asdf:defsystem
 :overview
 :name
 "overview"
 :serial
 t
 :version
 "0.0.1"
 :depends-on
 (:to-web)
 :components
 ((:file "package")
  (:file "0-main")))

