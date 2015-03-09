(in-package :to-web)

(defparameter *common-editor-port* 9999)

(defvar *web-editors* nil)

(defun find-web-editor (system-name)
  (find system-name *web-editors* :key #'first :test #'string=))

(defun list-web-editors ()
  (mapcar #'first *web-editors*))

(defun web-editor-name (web-editor)
  (first web-editor))

(defun web-editor-type (web-editor)
  (second web-editor))

(defun web-editor-communication-fns (web-editor)
  (rest (rest web-editor)))

(defun wrap-html-emitter (fn)
  (lambda ()
    (multiple-value-bind
        (html-result)
        (funcall fn)
      html-result)))

(defun wrap-published-function (action-name)
  (let ((args (third (load-action-definition action-name))))
    (compile nil
             `(lambda ()
                (with-post-parameters ,args
                  (write-string (json<-lisp (,action-name ,@args))
                                *standard-output*))))))

(defun make-single-editor-html (&rest args)
  (declare (ignore args))
  (with-html-output-to-string (:prologue t)
    (html
      (:html
       (:head
        (:css :relative "jquery-ui.min.css")
        (:css :relative "single-editor.css"))
       (:body
        (:textarea :id "editor")
        (:js :relative "functions.js")
        (:js :relative "jquery-1.10.1.min.js")
        (:js :relative "jquery.color-2.1.2.min.js")
        (:js :relative "jquery-ui.min.js")
        (:com)
        (:js :relative "editor-choice-dialog.js")
        (:js :relative "single-editor.js"))))))

(defun make-multiple-editor-html (&rest args)
  (declare (ignore args))
  (with-html-output-to-string (:prologue t)
    (html
      (:html
       (:head
        (:css :relative "jquery-ui.min.css")
        (:css :relative "multiple-editor.css")
        (:css :relative "editor-menu.css"))
       (:body
        (:ul :id "menu")
        (:textarea :id "firstEditor")
        (:textarea :id "secondEditor")
        (:js :relative "jquery-1.10.1.min.js")
        (:js :relative "jquery.color-2.1.2.min.js")
        (:js :relative "jquery-ui.min.js")
        (:com)
        (:js :relative "functions.js")
        (:js :relative "editor-choice-dialog.js")
        (:js :relative "multiple-editor.js"))))))

(defun make-code-editor-html (&rest args)
  (declare (ignore args))
  (with-html-output-to-string (:prologue t)
    (html
      (:html
       (:head
        (:css :relative "codemirror-4.7.css")
        (:css :relative "jquery-ui-1.11.2.custom.min.css")
        (:css :relative "show-hint.css")
        (:css :relative "ide.css"))
       (:body
        (:div :id "layout")
        (:div :id "menu")
        (:js (mkstr (get-rest-watcher-url) "/socket.io/socket.io.js"))
        (:js :relative "jquery-1.10.2.min.js")
        (:js :relative "jquery.color-2.1.2.min.js")
        (:js :relative "jquery-ui-1.11.2.custom.min.js")
        (:js :relative "JSXTransformer.js")
        (:js :relative "react.min.js")
        (:js :relative "codemirror-4.7.js")
        (:js :relative "commonlisp-4.7.js")
        (:js :relative "matchbrackets.js")
        (:js :relative "show-hint.js")
        (:com)
        (:js :relative "functions.js")
        (:js :relative "killer-functions.js")
        (:js :relative "cl-format.js")
        (:js :relative "components.js")
        (:js :relative "charts.js")
        (:js :relative "editors.js")
        ;(:js :relative "8-the-killer.js")
        (:js :relative "code-editor.js"))))))

(defun communication-url (action-name system-name)
  (string-downcase (mkstr "/" system-name "/" action-name)))

(defun build-web-editor-dispatchers (web-editor)
  (let ((system-name (web-editor-name web-editor))
        (type (web-editor-type web-editor)))
    (append (case type
              (:single-editor
               (list (create-prefix-dispatcher (communication-url :editor system-name)
                                               (wrap-html-emitter (lambda ()
                                                                    (watch system-name)
                                                                    (make-single-editor-html))))))
              (:multiple-editor
               (list (create-prefix-dispatcher (communication-url :editor system-name)
                                               (wrap-html-emitter (lambda ()
                                                                    (watch system-name)
                                                                    (make-multiple-editor-html))))))
              (:code-editor
               (list (create-prefix-dispatcher (communication-url :editor system-name)
                                               (wrap-html-emitter (lambda ()
                                                                    (watch system-name)
                                                                    (make-code-editor-html)))))))
            (loop for fn in (web-editor-communication-fns web-editor)
                  collect (let ((fn (if (atom fn) fn (first fn))))
                            (create-prefix-dispatcher (communication-url fn system-name)
                                                      (wrap-published-function fn))))
            (mapcar (lambda (args)
                      (let ((name (mkstr (first args) "/"))
                            (content-type (second args)))
                        (create-folder-dispatcher-and-handler
                         (communication-url name system-name)
                         (ensure-directories-exist
                          (merge-pathnames name (system-interacter-directory system-name)))
                         content-type)))
                    '(("css" "text/css")
                      ("js" "application/javascript")
                      ("fonts" "application/x-font-ttf"))))))

(defun list-editors ()
  (json<-lisp (sort (mapcar #'web-editor-name *web-editors*) #'string<)))

(defun list-watcher-urls ()
  (json<-lisp (list :core (get-core-watcher-url)
                    :rest (get-rest-watcher-url))))

(defun update-dispatch-table% ()
  (setf hunchentoot:*dispatch-table*
        (append (list (create-prefix-dispatcher "/list-editors" 'list-editors)
                      (create-prefix-dispatcher "/list-watcher-urls" 'list-watcher-urls))
                (reduce-conc (mapcar #'build-web-editor-dispatchers *web-editors*)))))

(defmacro define-web-editor ((system-name type) &body body)
  (with-gensyms (web-editor)
    `(let ((,web-editor (list ,system-name ,type ,@(loop for fn in body
                                                         collect `',fn))))
       (aif (position ,system-name *web-editors* :test #'string= :key #'first)
            (setf (elt *web-editors* it) ,web-editor)
            (push ,web-editor *web-editors*))
       (update-dispatch-table%))))

(defun build-web-editor (system-name)
  (let* ((web-editor (find-web-editor system-name))
         (type (web-editor-type web-editor))
         (target-directory (system-interacter-directory system-name)))
    (delete-directory-and-files target-directory :if-does-not-exist :ignore)
    (with-open-file (out (ensure-directories-exist
                          (merge-pathnames *js-communication-filename* target-directory))
                         :direction :output
                         :if-exists :supersede)
      (loop for fn in (web-editor-communication-fns web-editor)
            for i from 0
            when (< 0 i)
            do (write-string (mkstr #\Newline #\Newline) out)
            do (write-string (ajax-communication-code
                              (if (atom fn) fn (first fn))
                              system-name
                              (unless (atom fn) (rest fn)))
                             out)))
    (let ((html-editor-resource
           (awhen (case type
                    (:single-editor 'make-single-editor-html)
                    (:multiple-editor 'make-multiple-editor-html)
                    (:code-editor 'make-code-editor-html))
             (make-web-resource it :html ""))))
      (when html-editor-resource
        (build-with-resources (list html-editor-resource
                                    (make-web-resource 'copy-file :font "Anonymous-Pro.ttf")
                                    (make-web-resource 'copy-file :font "Anonymous-Pro-B.ttf"))
                              system-name target-directory)))))

(defun ajax-communication-code (action-name system-name options)
  (aif (load-action-definition action-name)
    (generate-ajax-call action-name
                        (communication-url action-name system-name)
                        (third it)
                        options)
    ""))

(defun generate-ajax-call (name url data &optional options)
  (let ((default-options (list :async t
                               :type "POST"))
        (data-json `(create ,@(reduce #'append (loop for arg in data
                                                     collect `(,(as-keyword arg) ,arg)))))
        (synchronous? (awhen (getf options :async) (string= "FALSE" (symbol-name it)))))
    (flet ((callback-code ()
             `(lambda (str)
                ,@(enlist
                  (unless synchronous? '(var json))
                  `(try
                    (setf json (chain *json* (parse str)))
                    (:catch (error)
                      (chain console
                             (error
                              (concatenate 'string
                                           ,(mkstr "JSON parse error during ajax communication " url " : ")
                                           error)))
                      (chain console
                             (error
                              (concatenate 'string
                                           ,(mkstr "ajax communication " url " response content is [")
                                           str
                                           "]")))))
                  (if synchronous?
                    'json
                    '(when (and json (is-function callback))
                       (chain callback (apply nil (array json)))))))))
      (ps*
       `(defun ,name ,(append data (unless synchronous? '(callback)))
          ,@(enlist
            (when synchronous? '(var json))
            `(chain $ (ajax ,url
                            (create ,@(plist-overwrite
                                       (plist-overwrite default-options options)
                                       (append (awhen (second data-json)
                                                 (list :data data-json))
                                               (list :success (callback-code)))))))
            (when synchronous? 'json)))))))

(defun start-common-editor-server ()
  (start (make-instance 'easy-acceptor
                        :port *common-editor-port*
                        :access-log-destination (merge-pathnames "access.log" (system-logs-directory "to-web"))
                        :message-log-destination (merge-pathnames "message.log" (system-logs-directory "to-web")))))

(defun check-interacter-resources (system-name)
  (check-resources (system-interacter-directory system-name) system-name))

(defun build-all-web-results (&key except)
  (let ((except (mklist except)))
    (dolist (system-name (remove-if (lambda (system-name)
                                      (member system-name except :test #'string=))
                                    (list-web-results)))
      (build-web-result system-name))))

(defun build-all-web-editors (&key except)
  (let ((except (mklist except)))
    (dolist (system-name (remove-if (lambda (system-name)
                                      (member system-name except :test #'string=))
                                    (list-web-editors)))
      (build-web-editor system-name))))

(defun rebuild-web-result (system-name)
  (commit-resources (system-watcher-directory system-name) system-name)
  (build-all-web-results)
  (build-all-web-editors))

(defun rebuild-web-editor (system-name)
  (commit-resources (system-interacter-directory system-name) system-name)
  (build-all-web-results)
  (build-all-web-editors))