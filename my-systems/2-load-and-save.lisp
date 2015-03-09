(in-package :my-systems)

(defun load-module-actions (module-name system-name)
  (with-system (system-name)
    (with-custom-io-syntax
      (with-open-file (file (system-module-file module-name system-name)
                            :if-does-not-exist :create)
        (loop for sexpr = (read file nil)
              while sexpr
              when (let-a (form-type sexpr)
                     (or (eq :function it)
                         (eq :macro it)))
              collect sexpr)))))

(defun load-module-definitions (module-name system-name)
  (with-system (system-name)
    (with-custom-io-syntax
      (with-open-file (file (system-module-file module-name system-name))
        (loop for sexpr = (read file nil)
              for i from 0
              while sexpr
              when (is-definition? sexpr)
              collect sexpr into definitions
              else when (and (< 0 i)
                             (eq (form-type sexpr) :fact))
              collect (first (mklist sexpr)) into fact-top-actions
              finally (return
                       (values definitions
                               (awhen (remove-duplicates fact-top-actions)
                                 (format nil "The module contains ~:[a fact. The top action is~;several facts. The top actions are:~] ~{~a~^ ~}" (< 1 (length it)) it)))))))))

(defun filter-out-non-action-forms (lst)
  (remove-if-not (lambda (form)
                   (let-a (form-type form)
                     (or (eq :function it)
                         (eq :macro it))))
                 lst))

(defun filter-out-usage-forms (lst)
  (remove-if (lambda (form)
               (eq :usage (form-type form)))
             lst))

(defun filter-in-usage-forms (lst)
  (remove-if-not (lambda (form)
                   (eq :usage (form-type form)))
                 lst))

(defun module-action-names (module-name system-name)
  (mapcar #'form-id (load-module-actions module-name system-name)))

(defun rewrite-source (source-file target-file system-name &key additions replacements deletions ordered-keys)
  "Write the source-file to the target-file identically except for the given exceptions.
If any comparison of the key of an exception form to the key of a form read from the source returns true, this form will not be reproduced in target-file but replaced by the exception form which has matched.
For every exception form provided whose key is not null and for which no comparison returned true, these are written at the end of the target-file, in the order they have been provided.
Return the exceptions."
  (let ((replacement-keys (mapcar #'form-key replacements))
        (deletion-keys (mapcar #'form-key deletions))
        (rewritten-forms (make-array (length ordered-keys) :initial-element nil))
        tail-forms)
    (labels ((collect-form (form key keys)
               (aif (position key keys :test #'equal)
                    (setf (elt rewritten-forms it) form)
                    (push form tail-forms)))
             (collect-from-file ()
               (when (file-exists-p source-file)
                 (with-open-file (src source-file)
                   (loop for form = (read src nil)
                         while form
                         do (let ((key (form-key form)))
                              (aif (position key replacement-keys :test #'equal)
                                   (collect-form (elt replacements it) key ordered-keys)
                                   (unless (find key deletion-keys :test #'equal)
                                     (collect-form form key ordered-keys))))))))
             (collect-additions ()
               (let ((ordered-keys (mapcar #'form-key-pure-name<-form-key ordered-keys)))
                 (dolist (form additions)
                   (collect-form form (form-key-pure-name form) ordered-keys))))
             (rewrite ()
               (with-open-file (out target-file
                                    :direction :output
                                    :if-exists :supersede)
                 (loop for form across rewritten-forms
                       when form
                       do (write-lisp-form form system-name out))
                 (loop for form in (nreverse tail-forms)
                       do (write-lisp-form form system-name out)))))
      (when (or replacements deletions ordered-keys)
        (with-system (system-name)
          (with-custom-io-syntax
            (collect-from-file)
            (collect-additions)
            (rewrite)))
        (values additions replacements deletions)))))

(defmacro with-rewrite-overwrite (file system-name &key additions replacements deletions ordered-keys)
  (with-gensyms (source-file rewritten-source)
    `(let* ((,source-file ,file)
            (,rewritten-source (merge-pathnames (concatenate 'string (file-namestring ,source-file) ".tmp")
                                                ,source-file)))
       (multiple-value-prog1
           (rewrite-source ,source-file ,rewritten-source ,system-name
                           :additions ,additions
                           :replacements ,replacements
                           :deletions ,deletions
                           :ordered-keys ,ordered-keys)
         (when (file-exists-p ,rewritten-source)
           (copy-file ,rewritten-source ,source-file :overwrite t)
           (delete-file ,rewritten-source))))))

(defun save-module-definitions (additions replacements deletions ordered-keys module-name system-name)
  (with-rewrite-overwrite (system-module-file module-name system-name)
                          system-name
                          :additions additions
                          :replacements replacements
                          :deletions deletions
                          :ordered-keys (cons (form-key `(in-package ,(as-keyword system-name)))
                                              ordered-keys)))

(defun load-action-definition (action-name &optional module-name system-name)
  (let* ((system-name (or system-name (symbol-system-name action-name))))
    (flet ((scan (file)
             (let (found)
               (with-system (system-name)
                 (with-custom-io-syntax
                   (with-open-file (s file)
                     (loop for sexpr = (read s nil)
                           while (and sexpr (not found))
                           if (and (or (string= "DEFUN" (symbol-name (first sexpr)))
                                       (string= "DEFMACRO" (symbol-name (first sexpr))))
                                   (eq action-name (second sexpr)))
                           do (setf found sexpr)))))
               found)))
      (let ((found (when module-name
                     (scan (system-module-file module-name system-name)))))
        (loop for module-file in (system-module-files system-name)
              while (not found)
              do (setf found (scan module-file)))
        found))))

