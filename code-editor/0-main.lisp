(in-package :code-editor)

(defparameter *debugged-usages-file* (ensure-directories-exist
                                      (merge-pathnames "debug/debugged-usages.json"
                                                       (system-work-directory "code-editor"))))

(defparameter *module-documentation-file* (ensure-directories-exist
                                           (merge-pathnames "doc/module-doc.json"
                                                            (system-work-directory "code-editor"))))

(defun make-watcher-page (filename title js-filename target-directory)
  (with-html-output-to-file ((merge-pathnames filename target-directory))
    (base-page (title :css '("pure-min.css" "charts.css" "components.css" "jquery-ui-1.10.3.custom.css" "debug2.css" "main2.css")
                      :js `("react.min.js" "JSXTransformer.js" "jquery-1.10.2.min.js" "jquery-ui-1.10.3.custom.min.js"
                                           "d3.min.js" "functions.js" "charts.js" "graph.js" "components.js" "bar-chart.js"
                                           ,js-filename))
      (:div :id "layout"))))

(defun make-index-page (target-directory &rest names)
  (declare (ignore names))
  (make-watcher-page "index.html" "Code information" "code-information.js" target-directory))

(define-web-result ("code-editor" :static-site)
  :font (copy-file "Anonymous-Pro.ttf" "Anonymous-Pro-B.ttf")
  :html (make-index-page "index.html"))

(defun make-chunk (form module-name system-name &key action-name)
  (multiple-value-bind
      (type id)
      (form-type form)
    (list :code (print-code-chunk form system-name)
          :id (print-code-chunk id system-name)
          :type (string-downcase (symbol-name type))
          :module-name module-name :system-name system-name
          :action-name (let-a (aif action-name it id)
                         (if (stringp it)
                           it
                           (print-code-chunk it system-name))))))

(defun form-key<-chunk-key (chunk-key system-name)
  (list (as-keyword (getf chunk-key :type))
        (first (read-code-chunk (getf chunk-key :id) system-name))))

(defun load-chunks (module-name system-name)
  (when (file-exists-p (system-module-file module-name system-name))
    (multiple-value-bind
        (forms message)
        (load-module-definitions module-name system-name)
      (list :chunks (mapcar (lambda (form)
                              (make-chunk form module-name system-name))
                            forms)
            :messages (when message (list (make-message "Facts in module" (list message) nil)))))))

(defun group-definitions-chunks-into-forms (additions replacements deletions ordered-chunk-keys)
  (let (result messages)
    (macrolet ((group-per-action (key chunks)
                 `(dolist (chunk ,chunks)
                    (handler-case (let* ((system-name (getf chunk :system-name))
                                         (action-name (first (read-code-chunk (getf chunk :action-name) system-name)))
                                         (info (read-code-chunk (getf chunk :code) system-name)))
                                    (setf (getf+ result
                                                 system-name :in
                                                 (getf chunk :module-name) :in ,key
                                                 action-name :in)
                                          (append (getf+ result
                                                         system-name :in
                                                         (getf chunk :module-name) :in ,key
                                                         action-name :in)
                                                  info)))
                      (condition (c)
                        (push (make-message (getf chunk :action-name)
                                            nil
                                            (list (format nil "~A" c)))
                              messages)))))
               (group-per-module (key chunks)
                 `(dolist (chunk ,chunks)
                    (handler-case (let* ((system-name (getf chunk :system-name))
                                         (info (list (form-key<-chunk-key chunk system-name))))
                                    (setf (getf+ result
                                                 system-name :in
                                                 (getf chunk :module-name) :in ,key)
                                          (append (getf+ result
                                                         system-name :in
                                                         (getf chunk :module-name) :in ,key)
                                                  info)))
                      (condition (c)
                        (push (make-message (getf chunk :action-name)
                                            nil
                                            (list (format nil "~A" c)))
                              messages))))))
      (group-per-action :additions additions)
      (group-per-action :replacements replacements)
      (group-per-action :deletions deletions)
      (group-per-module :ordered-keys ordered-chunk-keys))
    (values result messages)))

(defun group-usages-chunks-into-forms (additions replacements deletions ordered-chunk-keys)
  (let (result messages)
    (macrolet ((group-per-action (key chunks &optional extract-form)
                 `(dolist (chunk ,chunks)
                    (handler-case (let* ((system-name (getf chunk :system-name))
                                         (action-name (first (read-code-chunk (getf chunk :action-name) system-name)))
                                         (info ,(aif extract-form
                                                     it
                                                     '(read-code-chunk (getf chunk :code) system-name))))
                                    (setf (getf+ result
                                                 system-name :in
                                                 (getf chunk :module-name) :in
                                                 action-name :in ,key)
                                          (append (getf+ result
                                                         system-name :in
                                                         (getf chunk :module-name) :in
                                                         action-name :in ,key)
                                                  info)))
                      (condition (c)
                        (push (make-message (getf chunk :action-name)
                                            nil
                                            (list (format nil "~A" c)))
                              messages))))))
      (group-per-action :additions additions)
      (group-per-action :replacements replacements)
      (group-per-action :deletions deletions)
      (group-per-action :ordered-keys ordered-chunk-keys (list (form-key<-chunk-key chunk system-name))))
    (values result messages)))

(defun sort-grouped-forms (grouped-forms)
  (let ((system-and-module-list (get-system-and-module-list)))
    (loop for system-items in (sort grouped-forms (lambda (a b)
                                                    (< (position (get-name a) system-and-module-list
                                                                 :test #'string= :key #'first)
                                                       (position (get-name b) system-and-module-list
                                                                 :test #'string= :key #'first))))
          collect (let ((system-name (get-name system-items)))
                    (list :name system-name
                          :in (let ((module-list
                                     (second (find system-name system-and-module-list :test #'string= :key #'first))))
                                (sort (get-in system-items)
                                      (lambda (a b)
                                        (< (position (get-name a) module-list :test #'string=)
                                           (position (get-name b) module-list :test #'string=))))))))))

(defun save-complete-forms (additions replacements deletions ordered-keys module-name system-name)
  (save-module-definitions (filter-out-usage-forms additions)
                           (filter-out-usage-forms replacements)
                           (filter-out-usage-forms deletions)
                           ordered-keys module-name system-name))

(defun update-package-symbols-cache (system-names)
  (with-hierarchical-cache (:package-symbols :write t)
    (dolist (system-name system-names)
      (refresh system-name))))

(defun save-chunks (additions replacements deletions ordered-chunk-keys)
  (multiple-value-bind
      (grouped-forms messages)
      (group-definitions-chunks-into-forms (lisp<-json additions) (lisp<-json replacements)
                                           (lisp<-json deletions) (lisp<-json ordered-chunk-keys))
    (let (saved-chunks system-names)
      (with-hierarchical-caches ((:compile-problems :usages) :write t)
        (flet ((group-all-action-information (key data)
                 (reduce #'append (mapcar #'get-in (getf data key))))
               (collect-saved-forms (forms module-name system-name)
                 (setf saved-chunks (append saved-chunks
                                            (mapcar (lambda (form)
                                                      (make-chunk form module-name system-name))
                                                    forms))))
               (clean (action-names module-name system-name)
                 (let ((package (system-package system-name)))
                   (dolist (action-name action-names)
                     (delete-usages action-name module-name system-name)
                     (delete :usages system-name module-name action-name)
                     (fmakunbound action-name)
                     (unintern action-name package)))))
          (dolist (system-items (sort-grouped-forms grouped-forms))
            (let ((system-name (get-name system-items)))
              (dolist (module-items (get-in system-items))
                (let ((module-name (get-name module-items)))
                  (multiple-value-bind
                      (added-forms replaced-forms deleted-forms)
                      (save-complete-forms
                       (group-all-action-information :additions (get-in module-items))
                       (group-all-action-information :replacements (get-in module-items))
                       (group-all-action-information :deletions (get-in module-items))
                       (getf (get-in module-items) :ordered-keys)
                       module-name system-name)
                    (let ((forms (append added-forms replaced-forms)))
                      (collect-saved-forms forms module-name system-name)
                      (dolist (form forms)
                        (multiple-value-bind
                            (type id)
                            (form-type form)
                          (if (or (eq :function type)
                                  (eq :macro type))
                            (progn
                              (refresh :compile-problems system-name module-name id)
                              (refresh :usages system-name module-name id))
                            (let ((result (with-system (system-name)
                                            (eval-compile-form form))))
                              (when (and (symbolp result)
                                         (fboundp result))
                                (refresh :usages system-name module-name result)))))))
                    (clean (mapcar #'form-id (filter-out-non-action-forms deleted-forms)) module-name system-name))))
              (pushnew system-name system-names :test #'string=)))))
      (update-package-symbols-cache system-names)
      (list :chunks saved-chunks
            :messages (nreverse messages)))))

(defun load-usage-chunks (action-name module-name system-name)
  (list :chunks (mapcar (lambda (form)
                          (make-chunk form module-name system-name :action-name action-name))
                        (load-usages action-name module-name system-name))))

(defun save-usage-chunks (additions replacements deletions ordered-chunk-keys)
  (multiple-value-bind
      (grouped-forms messages)
      (group-usages-chunks-into-forms (lisp<-json additions) (lisp<-json replacements)
                                      (lisp<-json deletions) (lisp<-json ordered-chunk-keys))
    (let (saved-chunks)
      (flet ((collect-saved-forms (saved-forms action-name module-name system-name)
               (setf saved-chunks (append saved-chunks
                                          (mapcar (lambda (form)
                                                    (make-chunk form module-name system-name :action-name action-name))
                                                  saved-forms)))))
        (with-hierarchical-caches ((:usages) :write t)
          (dolist (system-items (sort-grouped-forms grouped-forms))
            (let ((system-name (get-name system-items)))
              (dolist (module-items (get-in system-items))
                (let ((module-name (get-name module-items)))
                  (dolist (action-items (get-in module-items))
                    (let ((action-name (get-name action-items)))
                      (multiple-value-bind
                          (added-forms replaced-forms)
                          (save-usages (getf (get-in action-items) :additions)
                                       (getf (get-in action-items) :replacements)
                                       (getf (get-in action-items) :deletions)
                                       (getf (get-in action-items) :ordered-keys)
                                       action-name module-name system-name)
                        (collect-saved-forms (append added-forms replaced-forms)
                                             action-name module-name system-name))
                      (refresh :usages system-name module-name action-name)))))))))
      (list :chunks saved-chunks
            :messages messages))))

(defun debug-some-code (perimeter system-name &key environment ignore-top-level skip-expansion highlight-action)
  (with-open-file (stream *debugged-usages-file*
                          :direction :output
                          :if-exists :supersede)
    (write-string
     (json<-lisp
      (list :html
            (with-html-output-to-string (:prologue nil)
              (multiple-value-bind
                  (result errors environment)
                  (full-view-debug:full-view-debug perimeter environment)
                (declare (ignore environment))
                (full-view-debug:layout-full-view-debug result errors system-name
                                        :ignore-top-level ignore-top-level
                                        :skip-expansion skip-expansion
                                        :highlight-action highlight-action)))))
     stream)))

(defun make-debugged-usages-file (action-name module-name system-name)
  (with-open-file (stream *debugged-usages-file*
                          :direction :output
                          :if-exists :supersede)
    (write-string
     (json<-lisp
      (list :html (with-html-output-to-string (:prologue nil)
                    (layout-debugged-usages (first (read-code-chunk action-name system-name))
                                            module-name system-name))))
     stream)))

(defun delete-debugged-usages-file ()
  (delete-file *debugged-usages-file* nil))

(defun make-module-documentation-file (module-name system-name)
  (with-open-file (stream *module-documentation-file*
                          :direction :output
                          :if-exists :supersede)
    (write-string
     (json<-lisp
      (list :html (with-html-output-to-string (:prologue nil)
                    (librarian::html-module-view system-name module-name))))
     stream)))

(defun delete-module-documentation-file ()
  (delete-file *module-documentation-file* nil))

(define-web-editor ("code-editor" :code-editor)
  (load-chunks :async false)
  (save-chunks :async false)
  (load-usage-chunks :async false)
  (save-usage-chunks :async false)
  (make-debugged-usages-file)
  (delete-debugged-usages-file)
  (make-module-documentation-file)
  (delete-module-documentation-file))
  

(defun reload-those-things ()
  (refresh-hierarchical-cache :usages "full-view-debug" "1-debugger" 'full-view-debug::debug-perimeter)
  (make-debugged-usages-file "debug-perimeter" "1-debugger" "full-view-debug")
  nil)