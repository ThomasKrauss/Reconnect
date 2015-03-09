(in-package :my-systems)

(defun system-loaded? (system-name)
  (when (and (system-name? system-name)
             (system-package system-name))
    system-name))

(defun system-list-all ()
  "Return the list of systems available for edition.
Ordered by dependencies.
The current edited system is at the head of the list."
  (let ((results (remove-if-not #'system-name?
                                (mapcar (lambda (path)
                                          (first (last (pathname-directory path))))
                                        (remove-if-not #'directory-pathname-p
                                                       (list-directory (system-source-base-directory)))))))
    (sort (remove-duplicates results :test #'string= :from-end t)
          #'string<)))

(defun system-list-all-loaded ()
  (remove-if #'null (mapcar #'system-loaded? (system-list-all))))

(defun ensure-system-directories-exist (system-name)
  (mapcar (lambda (path)
            (list path (ensure-directories-exist path)))
          (list (system-work-directory system-name)
                (system-logs-directory system-name)
                (system-usages-directory system-name)
                (system-documentation-directory system-name)
                (system-resources-directory system-name)
                (system-watcher-directory system-name)
                (system-interacter-directory system-name))))

(defun ensure-all-system-directories-exist ()
  (mapcar (lambda (system-name)
            (list system-name
                  (ensure-system-directories-exist system-name)))
          (system-list-all)))

(defun load-system (system-name)
  (with-output-to-string (s)
    (let ((*standard-output* s))
      (asdf:load-system system-name))))

(defun complete-asd-file (system-name &key depends-on components)
  (let ((asd-file (system-asd-file system-name))
        actual-depends-on actual-components)
    (with-custom-io-syntax
      (when (file-exists-p asd-file)
        (with-open-file (s asd-file :direction :input)
          (loop for sexpr = (read s nil nil)
                while sexpr
                when (and (equal (first sexpr) 'defsystem)
                          (equal (second sexpr) (as-keyword system-name)))
                do (setf actual-depends-on (getf (cddr sexpr) :depends-on)
                         actual-components (mapcar #'second (getf (cddr sexpr) :components)))))
        (delete-file asd-file))
      (with-lisp-file asd-file system-name
        `(defpackage ,(as-keyword (concatenate 'string system-name "-system")) (:use :cl :asdf))
        `(in-package ,(as-keyword (concatenate 'string system-name "-system")))
        `(asdf:defsystem ,(as-keyword system-name) :name ,system-name :serial t :version "0.0.1"
           :depends-on ,(remove-duplicates (append actual-depends-on depends-on))
           :components ,(loop for component in (remove-duplicates (append actual-components components))
                              collect `(:file ,component)))))))

(defun get-system-declared-dependencies (system-name)
  (with-open-file (in (system-asd-file system-name))
    (loop for sexpr = (read in nil)
          while sexpr
          when (string= "DEFSYSTEM" (symbol-name (first sexpr)))
          return (mapcar (lambda (symbol)
                           (string-downcase (symbol-name symbol)))
                         (getf (rest (rest sexpr)) :depends-on)))))
  
(defun create-system (system-name &key load depends-on)
  "Create the system of given name unless a source-directory of the same name already exists.
By default, the system is not loaded."
  (macrolet ((mkdir (&rest directories)
               `(progn ,@(mapcar (lambda (x)
                                   `(ensure-directories-exist ,x))
                                 directories))))
    (let ((source-dir (system-source-directory system-name)))
      (unless (directory-exists-p source-dir)
        (mkdir source-dir (system-work-directory system-name)
               (system-usages-directory system-name))
        (with-custom-io-syntax
          ; asd file
          (complete-asd-file system-name :components (list +package-filename+))
          ; package file
          (with-lisp-file (system-package-file system-name)
              system-name
            `(in-package :cl)
            `(defpackage ,(as-keyword system-name) ,(append '(:use :cl :cl-fad)
                                                     depends-on))
            `(in-package ,(as-keyword system-name))))
        (unless (find source-dir asdf:*central-registry* :test (lambda (x y)
                                                                 (equal (pathname-directory x)
                                                                        (pathname-directory y))))
          (push source-dir asdf:*central-registry*)))
      (when load
        (load-system system-name)
        (unless (system-package system-name)
          (load (system-package-file system-name)
                :package (find-package "CL"))))
      asdf:*central-registry*)))

(defun create-module (module-name system-name)
  (let ((module-file (merge-pathnames (concatenate 'string module-name ".lisp") (system-source-directory system-name))))
    (when (and (system-name? system-name)
               (not (file-exists-p module-file)))
      (with-custom-io-syntax
        (with-lisp-file module-file
          `(in-package ,(as-keyword system-name)))
        (complete-asd-file system-name :components (list module-name)))
      module-name)))
    
(defun delete-system (system-name)
  "Delete the system of given name.
Exceptions are the big-kill system itself which cannot be deleted.
And the draft system which will only be re-created immediately after deletion.
So in effect, it will be brand new, cleared of any draft work."
  (macrolet ((rmdir (&rest directories)
               `(progn ,@(mapcar (lambda (x)
                                   `(delete-directory-and-files ,x :if-does-not-exist :ignore))
                                 directories))))
    (let ((source-dir (system-source-directory system-name)))
      (rmdir source-dir (system-work-directory system-name)
             (system-usages-directory system-name))
      (aif (system-package system-name)
           (delete-package it))
        (setf asdf:*central-registry*
              (remove-if (lambda (path)
                           (when (pathnamep path)
                             (string= (namestring path) (namestring source-dir))))
                         asdf:*central-registry*)))))