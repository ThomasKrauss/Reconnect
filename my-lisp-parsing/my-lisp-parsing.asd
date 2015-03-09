(defpackage :my-lisp-parsing-system (:use :cl :asdf))
(in-package :my-lisp-parsing-system)

(asdf:defsystem :my-lisp-parsing
  :name
  "my-lisp-parsing"
  :serial
  t
  :version
  "0.0.1"
  :depends-on (:my-graphs :my-systems :my-caches :to-web :cl-ppcre :full-view-debug)
  :components
  ((:file "package")
   (:file "0-parse-forms")
   (:file "1-count-lines-and-forms")
   (:file "2-compile-forms")
   (:file "3-dependency-graphs")
   (:file "4-consolidate-dependencies")
   (:file "5-parse-sources")
   (:file "6-package-symbols")
   (:file "7-usages")))