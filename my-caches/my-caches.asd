(defpackage :my-caches-system (:use :cl :asdf))

(in-package :my-caches-system)

(asdf:defsystem :my-caches
  :name
  "my-caches"
  :serial
  t
  :version
  "0.0.1"
  :depends-on (:my-utilities :my-systems)
  :components
  ((:file "package")
   (:file "0-key-wording")
   (:file "1-key-structure")
   (:file "2-internal-accessors")
   (:file "3-external-accessors")
   (:file "4-building-actions")))

