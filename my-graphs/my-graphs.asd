(defpackage :my-graphs-system (:use :cl :asdf))

(in-package :my-graphs-system)

(asdf:defsystem
    :my-graphs
  :name
  "my-graphs"
  :serial
  t
  :version
  "0.0.1"
  :depends-on
  (:my-utilities :my-systems)
  :components
  ((:file "package")
   (:file "0-data-structures")
   (:file "1-extended-data-structures")
   (:file "2-transformations")
   (:file "3-search")
   (:file "4-other")
   (:file "5-to-dot")))
