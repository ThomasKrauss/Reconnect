(defpackage :to-web-system (:use :cl :asdf))
(in-package :to-web-system)

(asdf:defsystem :to-web
  :name
  "to-web"
  :serial
  t
  :version
  "0.0.1"
  :depends-on (:my-utilities :my-systems :formatter :cl-fad :drakma :cl-ppcre :hunchentoot :st-json :parenscript :trivial-utf-8)
  :components
  ((:file "package")
   (:file "0-util")
   (:file "1-from-plain-text")
   (:file "2-html-engine")
   (:file "3-html-macros")
   (:file "4-external-resources")
   (:file "5-watcher-server")
   (:file "6-web-results")
   (:file "7-web-editors")
   (:file "8-html-templates")))