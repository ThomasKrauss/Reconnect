(in-package :cl)
(defpackage :my-utilities (:use :cl :cl-fad :cl-ppcre :my-backquote)
  (:export
	; environment
	:make-binding
	:binding-name
	:binding-value
	:make-bindings
        :parse-destructuring-lambda-list
        :bindings<-lambda-list
	:make-let-scope
	:make-flet-scope
	:make-labels-scope
	:make-environment
	:add-scope
	:add-binding
	:remove-scope
	:modify-binding
	:find-action
	:find-symbol-macrolet
	:embed-in-environment
	; cl-complement
	:cl-action?
	:cl-macro?
	:cl-function?
	:cl-special-action?
	:cl-self-evaluating?
	:get-action-lambda-list
	:get-action-body

   ; util
   :enlist
   :def-chainable-setf
   :def-access
   :get-bound-symbols
   :normalize-string
   :group
   :identity-ignore-rest
   :map-plist
   :push-each-new
   :plist-keys
   :plist-values
   :getf+
   :rec-getf
   :get-name
   :get-in
   :get-named-plist
   :*setf-named-plist-indexes*
   :copy-plist
   :plist<-list
   :rest-or-one
   :mac
   :flatten
   :mklist
   :join-str
   :with-gensyms
   :once-only
   :mkstr
   :symb
   :as-keyword
   :get-canonical-key
   :group-values-by-keys
   :reduce-conc
   :dlambda
   :with-machineries
   :_
   :no-backslashes
   :call-command
   :as-plist
   :as-titled-plist
   :my-bind
   :plist-case
   :manual-list-transform
   :plist-overwrite
   :put
   :put-each
   :putnew
   :put-each-new
   
   ; json
   :json-escape
   :nil-is-json-null
   :lisp<-json
   :json<-lisp
   
   ; queue
   :make-queue
   :queue-contents
   :empty-queue-p
   :queue-front
   :enqueue
   :dequeue

   ; boolean
   :not-null
   :list=
   :list-equal
   :plistp
   :titled-plistp
   :pathname=
   :my-equal
   :eq-diff
   :eql-diff
   :equal-diff
   :my-equal-diff
   :pathname-diff
   :pathname=-diff
   :string=-diff
   :string-equal-diff
   :bit-vector-diff
   :file-equal
   :with-tested-files
   :values-equal
   :directory-equal

   ; anaphora
   :it
   :this
   :let-a
   :aif
   :awhen
   :alet
   :alet-fsm
   :state
   :acond

   ; web
   :cl-name<-js-name
   :js-name<-cl-name

   ; fad
   :file-directory
   :last-directory
   :copy-directory-and-files
   :load-file-content
   :save-file-content
   :read-obvious-links-from-file
   :escape-filename
   :parse-filename
   ))