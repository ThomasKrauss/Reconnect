(in-package :to-web)

(defvar *core-watcher-system-name* nil)

(defparameter *static-site-node-modules* (list "inertia" "socket.io" "watch"))

(defvar *watched* nil
  "Indicate in HTML template based on base-page if the page is supposed to be watched by a watcher server.
If so, it needs to include socket.io.js and refresh.js")

(defparameter *watcher-ports* '(:core 8078 :rest 8079))

(defun get-rest-watcher-url ()
  (format nil "http://localhost:~a" (getf *watcher-ports* :rest)))

(defun get-core-watcher-url ()
  (format nil "http://localhost:~a" (getf *watcher-ports* :core)))

(defun install-watcher (&key install-node-modules)
  (let ((directory (system-watcher-base-directory)))
    (generate-nodejs-server directory)
    (generate-refresh-js directory)
    (generate-watcher-index directory)
    (generate-resources-json directory)
    (when install-node-modules
      (let ((target-directory (merge-pathnames "node_modules/" (system-watcher-base-directory))))
        (delete-directory-and-files target-directory :if-does-not-exist :ignore)
        (ensure-directories-exist target-directory)
        (dolist (name *static-site-node-modules*)
          (copy-directory-and-files (merge-pathnames (concatenate 'string "node_modules/" name "/")
                                                     (system-resources-directory "to-web"))
                                    (merge-pathnames (concatenate 'string name "/")
                                                     target-directory)))))))

(defun launch-watcher (&key (watcher-type :rest) guarded)
  (node *nodejs-server-filename*
        :arguments (cons (mkstr (getf *watcher-ports* watcher-type))
                         (when (eq :core watcher-type)
                           (aif *core-watcher-system-name*
                                (list it)
                                (warn "Core watcher asked to be launched but no system name is associated to it"))))
        :current-directory (system-watcher-base-directory)
        :guarded guarded))

(defun watch (system-name)
  (drakma:http-request (mkstr (get-rest-watcher-url) "/watch")
                       :method :post
                       :parameters (list (cons "systemName" system-name))))

(defun string<-resource-type (type)
  "Put icons into css folder since my hosting serves its own icons folder instead of mine. Sweet."
  (case type
    (:js "js")
    (:css "css")
    (:font "fonts")
    (:icon "css/icons")
    (:image "images")
    (:file "files")))

(defun resource-filename (type filename &optional (system-name "to-web"))
  (merge-pathnames (mkstr (string<-resource-type type) "/" filename)
                   (system-resources-directory system-name)))

(defun target-directory (type target-directory)
  (ensure-directories-exist
   (merge-pathnames (mkstr (string<-resource-type type) "/")
                    target-directory)))

(defun target-filename (type filename target-directory)
  (merge-pathnames filename
                   (target-directory type target-directory)))

(defun wrap-html-emitter-by-map (fn)
  "An html-emitter like (filename target-directory) becomes (target-directory &rest names)."
  (lambda (target-directory &rest names)
    (let (css-output js-output)
      (dolist (name names)
        (multiple-value-bind
            (file css js)
            (funcall fn name target-directory)
          (declare (ignore file))
          (setf css-output (append css-output css)
                js-output (append js-output js))))
      (values names
              css-output
              js-output))))

(defun wrap-by-map (fn)
  (lambda (target-directory &rest names)
    (dolist (name names)
      (funcall fn name target-directory))
    names))

(defun install-resources (system-name type target-directory &rest filenames)
  (flet ((install-resource (type filename target-directory)
           (awhen (or (file-exists-p (resource-filename type filename system-name))
                      (find-external-resource filename)
                      (file-exists-p (resource-filename type filename)))
             (let ((pathname (target-filename type filename target-directory)))
               (copy-file it pathname :overwrite t)
               pathname))))
    (loop for filename in filenames
          collect (install-resource type filename target-directory))))

(defvar *web-results* nil
  "List of (system-name &rest (type resource process)).")

(defun find-web-result (system-name)
  (find system-name *web-results* :key #'first :test #'string=))

(defun list-web-results ()
  (mapcar #'first *web-results*))

(defun get-final-web-result-pathname (system-name)
  (merge-pathnames "site/" (system-resources-directory system-name)))

(defun layout-web-result-definition-body (body)
  (let (layout)
    (dolist (clause (group body 2))
      (let ((type (first clause))
            (body (second clause)))
        (push (append (list 'list
                            type
                            (if (symbolp (first body))
                              (if (eq :map-wrapped (second body))
                                (case type
                                  (:html
                                   `(wrap-html-emitter-by-map ',(first body)))
                                  (t
                                   `(wrap-by-map ',(first body))))
                                `',(first body))
                              'copy-file))
                      (if (symbolp (first body))
                        (if (symbolp (second body))
                          (nthcdr 2 body)
                          (rest body))
                        body))
              layout)))
    (cons 'list layout)))

(defmacro define-web-result ((system-name web-result-type &key under-core-watcher?) &body body)
  (declare (ignore web-result-type))
  (with-gensyms (web-result web-resources collect-web-resources clause)
    `(let (,web-resources)
       ,(when under-core-watcher?
          `(if (or (null *core-watcher-system-name*)
                   (string= *core-watcher-system-name* ,system-name))
             (setf *core-watcher-system-name* ,system-name)
             (warn "The web-result about the system ~a is defined to be under the core watcher but the system ~a is already under it" ,system-name *core-watcher-system-name*)))
       (labels ((,collect-web-resources (type process lst)
                  (dolist (name lst)
                    (if (atom name)
                      (push (make-web-resource process type name) ,web-resources)
                      (,collect-web-resources type process name)))))
         (dolist (,clause ,(layout-web-result-definition-body body))
           (,collect-web-resources (first ,clause) (second ,clause) (rest (rest ,clause)))))
       (let ((,web-result (cons ,system-name ,web-resources)))
         (aif (position ,system-name *web-results* :key #'first :test #'string=)
              (setf (elt *web-results* it) ,web-result)
              (push ,web-result *web-results*)))
       ,system-name)))

(defun make-web-resource (process type name)
  (list process type name))

(defun web-resource-process (web-resource)
  (first web-resource))

(defun web-resource-type (web-resource)
  (second web-resource))

(defun web-resource-name (web-resource)
  (third web-resource))

(defun extended-web-resource-names (extended-web-resource)
  (nthcdr 2 extended-web-resource))

(defun list-web-resources (name web-result)
  (remove-if-not (lambda (x)
                   (string= name x))
                 (rest web-result) :key #'web-resource-name))

(defun generate-html-resource (extended-web-resource target-directory)
  (apply (web-resource-process extended-web-resource)
         target-directory
         (extended-web-resource-names extended-web-resource)))

(defun build-with-extended-resource (extended-web-resource system-name target-directory)
  (let-a (web-resource-type extended-web-resource)
    (case it
      (:html
       (generate-html-resource extended-web-resource target-directory))
      (t
       (if (eq 'copy-file (web-resource-process extended-web-resource))
         (apply #'install-resources
                system-name
                (web-resource-type extended-web-resource)
                target-directory
                (extended-web-resource-names extended-web-resource))
         (apply (web-resource-process extended-web-resource)
                (target-directory it target-directory)
                (extended-web-resource-names extended-web-resource)))))))

(defun extend-web-resources (web-resources)
  "Extend web resources to hold all the names related to each couple (process type). Eliminate duplicate names. Web-resources without a process stand alone."
  (let (extended-web-resources)
    (dolist (web-resource web-resources)
      (aif (position web-resource extended-web-resources :test (lambda (x y)
                                                                 (and (eq (web-resource-process x)
                                                                          (web-resource-process y))
                                                                      (eq (web-resource-type x)
                                                                          (web-resource-type y)))))
           (unless (member (web-resource-name web-resource)
                           (extended-web-resource-names (elt extended-web-resources it))
                           :test #'string=)
             (setf (elt extended-web-resources it) (append (elt extended-web-resources it)
                                                           (list (web-resource-name web-resource)))))
           (push web-resource extended-web-resources)))
    extended-web-resources))

(defun build-with-resources (web-resources system-name target-directory &key (install-associated t))
  (let (css-output js-output)
    (dolist (extended-web-resource (extend-web-resources web-resources))
      (multiple-value-bind
          (file css js)
          (build-with-extended-resource extended-web-resource system-name target-directory)
        (declare (ignore file))
        (setf css-output (append css-output css)
              js-output (append js-output js))))
    (when (and install-associated
               (or css-output js-output))
      (build-with-resources (append
                             (mapcar (lambda (css-resource-name)
                                       (make-web-resource 'copy-file :css css-resource-name))
                                     css-output)
                             (mapcar (lambda (js-resource-name)
                                       (make-web-resource 'copy-file :js js-resource-name))
                                     js-output))
                            system-name
                            target-directory
                            :install-associated install-associated))))

(defvar *final-built* nil)

(defun is-built-final? ()
  *final-built*)

(defun build-web-result (system-name &key final)
  (let ((web-result (find-web-result system-name))
        (target-directory (if final
                            (aif (and (pathnamep final)
                                      (directory-pathname-p final))
                                 it
                                 (get-final-web-result-pathname system-name))
                            (system-watcher-directory system-name))))
    (delete-directory-and-files target-directory :if-does-not-exist :ignore)
    (let ((*watched* (not final))
          (*final-built* final))
      (build-with-resources (rest web-result) system-name target-directory))))

(defun update-web-result (system-name &rest names)
  (awhen (find-web-result system-name)
    (awhen (reduce #'append (loop for name in names
                                  collect (list-web-resources name it)))
      (let ((*watched* t))
        (build-with-resources (extend-web-resources it) system-name (system-watcher-directory system-name)
                              :install-associated nil)))))

(defun check-web-result (system-name)
  (let ((target-directory (merge-pathnames (concatenate 'string system-name "-site/")
                                           (system-work-directory "to-web"))))
    (unwind-protect (progn
                      (build-web-result system-name :final target-directory)
                      (directory-equal target-directory (get-final-web-result-pathname system-name)))
      (delete-directory-and-files target-directory :if-does-not-exist :ignore))))

(defun check-resources (target-directory system-name)
  (let ((target-directory (merge-pathnames "js/" target-directory)))
    (remove-if #'null
               (loop for file in (list-directory target-directory)
                     collect (let* ((filename (file-namestring file))
                                    (resource-file (or (file-exists-p (resource-filename :js filename system-name))
                                                       (file-exists-p (resource-filename :js filename)))))
                               (when (and resource-file
                                          (not (file-equal file resource-file)))
                                 filename))))))

(defun commit-resources (target-directory system-name)
  (flet ((commit (type target-directory)
           (remove-if #'null
                      (loop for file in (list-directory target-directory)
                            collect (let* ((filename (file-namestring file))
                                           (resource-file (or (file-exists-p (resource-filename type filename system-name))
                                                              (file-exists-p (resource-filename type filename)))))
                                      (when (and resource-file
                                                 (not (file-equal file resource-file)))
                                        (copy-file file resource-file :overwrite t)
                                        filename))))))
    (awhen (append (commit :js (merge-pathnames "js/" target-directory))
                   (commit :css (merge-pathnames "css/" target-directory)))
      (format t "Commit back the following file~:[s~;~]:~%~{- ~a~^~%~}"
              (= 1 (length it))
              it)
      it)))

(defun check-watcher-resources (system-name)
  (check-resources (system-watcher-directory system-name) system-name))