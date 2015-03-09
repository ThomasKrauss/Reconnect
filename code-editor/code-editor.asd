(defpackage :code-editor-system (:use :cl :asdf))

(in-package :code-editor-system)

(asdf:defsystem
 :code-editor
 :name
 "code-editor"
 :serial
 t
 :version
 "0.0.1"
 :depends-on
 (:cl-fad :cl-ppcre :my-systems :to-web :my-utilities :my-caches :my-lisp-parsing :librarian)
 :components
 ((:file "package")
  (:file "0-main")))