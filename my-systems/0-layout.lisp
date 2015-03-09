(in-package :my-systems)

(defparameter *base-dir* cl-user::*basedir*
  "*base-dir* is the location of my home for Lisp development.")

(defun system-external-resources-directory ()
  "=> directory
Give the directory holding the external resources.
The location is outside of the source base directory of the Reconnect project."
  (merge-pathnames "external-resources/" *base-dir*))

(defconstant +package-filename+ "package")

(defun system-source-base-directory ()
  "=> directory
Give the directory at the base of all source code."
  (merge-pathnames "dev/" *base-dir*))

(defun system-source-directory (system-name)
  "=> directory
  Give the directory holding the source files of the system of given name."
  (merge-pathnames (concatenate 'string system-name "/")
                   (system-source-base-directory)))

(defun system-package-file (system-name)
  "=> package-file
  Give the file defining the package associated to the system of given name."
  (merge-pathnames (concatenate 'string +package-filename+ ".lisp")
                   (system-source-directory system-name)))

(defun system-asd-file (system-name)
  "=> asd-file
  Give the file defining the ASDF system associated to the system of given name."
  (merge-pathnames (concatenate 'string system-name ".asd")
                   (system-source-directory system-name)))

(defun system-work-directory (system-name)
  "=> directory
  Give the directory holding all work files produced by using the system of given name."
  (merge-pathnames (concatenate 'string "work/" system-name "/")
                   *base-dir*))

(defun system-logs-directory (system-name)
  "=> directory
  Give the directory holding the log files produced by using the system of given name."
  (merge-pathnames "logs/" (system-work-directory system-name)))

(defun system-usages-directory (system-name)
  "=> directory
  Give the directory holding the usages of the actions of the system of given name."
  (merge-pathnames "usages/" (system-source-directory system-name)))

(defun system-documentation-directory (system-name)
  "=> directory
Give the directory holding the documentation about the system of given name."
  (merge-pathnames "doc/" (system-source-directory system-name)))

(defun system-resources-directory (system-name)
  "=> directory
  Give the directory holding the resources needed by the system of given name."
  (merge-pathnames (concatenate 'string "resources/")
                   (system-source-directory system-name)))

(defun system-resource (resource-name system-name)
  "=> file
  Give the file named resource-name that resides in the resource directory of the system of given name."
  (merge-pathnames resource-name
                   (system-resources-directory system-name)))

(defun system-watcher-base-directory ()
  "=> directory
  Give the directory at the base of all directories holding watched web-results."
  (merge-pathnames "watcher/" *base-dir*))

(defun system-watcher-directory (system-name)
  "=> directory
  Give the watched directory holding the web-result of the system of given name."
  (merge-pathnames (concatenate 'string system-name "/")
                   (system-watcher-base-directory)))

(defun system-interacter-directory (system-name)
  "=> directory
  Give the directory holding the web-editor for the system of given name."
  (merge-pathnames (concatenate 'string "interacter/" system-name "/")
                   *base-dir*))

(defun system-package (system-name)
  "=> package
  Give the package associated to the system of given name."
  (find-package (string-upcase system-name)))

(defun system-module-file (module-name system-name)
  "=> file
  Give the file of the module of given name that is supposed to be part of the system of given name.
  Whether the file really exists or not is irrelevant."
  (merge-pathnames (concatenate 'string module-name ".lisp")
                   (system-source-directory system-name)))

(defun system-module-files (system-name)
  "=> files
  Give all the module files of the system of given name."
  (mapcar (lambda (name) (system-module-file name system-name))
          (asd-components (system-asd-file system-name))))

(defun system-name? (name)
  "=> system-name
  A name is a system-name when the source base directory holds a directory of that name and that directory must contain a package definition file and a ASDF system definition file."
  (and (directory-exists-p (system-source-directory name))
       (file-exists-p (system-asd-file name))
       (file-exists-p (system-package-file name))
       name))

(defun system-module-name (module-file)
  "=> module-name
  Get the name of the module out of the given module-file"
  (let-a (file-namestring module-file)
    (subseq it 0 (position #\. it :from-end t))))

(defun system-module-names (system-name)
  "=> module-names
  Give the name of all the modules of the system of given name."
  (mapcar #'system-module-name (system-module-files system-name)))

(defun module-name? (name system-name)
  "=> module-name
  A name is a module-name when it is declared as a ASDF component in the ASDF definition file associated to the system of given name. The associated module file may or may not exist. The only thing that matter is that the module is considered when loading the system it lives in."
  (and (member name
               (asd-components (system-asd-file system-name))
               :test
               #'string=)
       name))

(defun asd-components (asd-file)
  "=> names
  Give the names of all the components of the ASDF system definition present in the given asd-file."
  (when (file-exists-p asd-file)
    (with-open-file (s asd-file)
      (loop for expr = (read s nil)
            while expr
            do (when (eq 'asdf:defsystem (first expr))
                 (return (remove-if (lambda (path)
                                      (string= +package-filename+
                                               path))
                                    (mapcar #'second
                                            (getf expr
                                                  :components)))))))))

