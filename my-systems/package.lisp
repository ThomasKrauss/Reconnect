(in-package :cl)
(defpackage :my-systems (:use :cl :cl-fad :cl-ppcre :my-utilities)
  (:export
		; layout
   :*base-dir*
   :system-source-directory
   :system-work-directory
   :system-logs-directory
   :system-usages-directory
   :system-documentation-directory
   :system-resources-directory
   :system-watcher-base-directory
   :system-watcher-directory
   :system-watcher-site-directory
   :system-interacter-directory
   :system-package
   :system-module-file
   :system-module-files
   :system-module-name
   :system-module-names
   :get-system-declared-dependencies
   :system-resource
   :ensure-all-system-directories-exist
   :symbol-system-name
   :json-preformatter
		
		; read-and-print
   :with-custom-io-syntax
   :with-system
   :write-lisp-form
   :with-lisp-file
   :read-code-chunk
   :print-code-chunk
   :form-id
   :form-type
   :is-definition?

                ; load-and-save
   :with-rewrite-overwrite
   :load-module-actions
   :load-module-definitions
   :filter-out-non-action-forms
   :filter-out-usage-forms
   :filter-in-usage-forms
   :save-module-definitions
   :load-action-definition
   :module-action-names
		
		; management
   :system-name?
   :system-loaded?
   :system-list-all
   :system-list-all-loaded
   :load-system
   :complete-asd-file
   :create-system
   :create-module
   :delete-system
		
		; documentation
   :make-doc-dictionnary
   :get-symbol-documentation
   :set-symbol-documentation
   :get-system-documentation
		
                ; backup
   :print-backup-files

                ; packing
   :do-pack
   
   :defun-to-external-program
   :cmd-escape
   ))