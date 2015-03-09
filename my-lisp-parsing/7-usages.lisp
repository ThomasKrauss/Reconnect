(in-package :my-lisp-parsing)

(defun get-usage-file (action-name module-name system-name)
  (merge-pathnames (mkstr module-name "/" (escape-filename (format nil "~(~a~).lisp" action-name)))
                   (system-usages-directory system-name)))

(defun load-usages (action-name module-name system-name)
  "Return the list of known usages of the function of given id.
Return nil if the usage file does not exist or is empty."
  (let ((usage-file (get-usage-file action-name module-name system-name)))
    (when (file-exists-p usage-file)
      (with-system (system-name)
        (with-open-file (s usage-file)
          (with-custom-io-syntax
            (loop for form = (read s nil nil)
                  while form
                  collect form)))))))

(defun save-usages (additions replacements deletions ordered-keys action-name module-name system-name)
  (with-rewrite-overwrite (ensure-directories-exist (get-usage-file action-name module-name system-name))
                          system-name
                          :additions additions
                          :replacements replacements
                          :deletions deletions
                          :ordered-keys ordered-keys))

(defun delete-usages (action-name module-name system-name)
  (awhen (file-exists-p (get-usage-file action-name module-name system-name))
    (delete-file it)))

(defun map-to-usages (fn usages)
  (labels ((rec (usage)
             (if (stringp (first usage))
               (cons (first usage)
                     (loop for sub-usage in (rest usage)
                           collect (rec sub-usage)))
               (funcall fn usage))))
    (loop for usage in usages
          collect (rec usage))))

(defun length-of-usages (usages)
  (labels ((rec (usage)
             (if (stringp (first usage))
               (loop for sub-usage in (rest usage)
                     sum (rec sub-usage))
               1)))
    (loop for usage in usages
          sum (rec usage))))

(defun make-usage-eval-environment (action-name system-name)
  (let ((temporary-directory (merge-pathnames (concatenate 'string (symbol-name (gensym "tmp")) "/")
                                              (system-usages-directory system-name))))
    (with-gensyms (filename)
      (values `((flet ((,(intern "USAGE-TEMPORARY-DIRECTORY" (system-package system-name)) ()
                         (ensure-directories-exist ,temporary-directory))
                       (,(intern "USAGE-FILE" (system-package system-name)) (,filename)
                         (merge-pathnames
                          ,filename
                          ,(merge-pathnames        
                            (concatenate 'string (string-downcase (symbol-name action-name)) "/")
                            (system-usages-directory system-name))))))
                (let ((*package* (system-package ,system-name)))))
              temporary-directory))))

(defun wrap-usage-for-evaluation (usage action-name system-name)
  (multiple-value-bind
      (environment temporary-directory)
      (make-usage-eval-environment action-name system-name)
    (values `(unwind-protect ,usage
               (delete-directory-and-files ,temporary-directory
                                           :if-does-not-exist :ignore))
            environment
            (lambda ()
              (delete-directory-and-files temporary-directory :if-does-not-exist :ignore)))))

(defun eval-usages (action-name module-name system-name)
  (with-system (system-name)
    (let-a (load-usages action-name module-name system-name)
      (values (map-to-usages (lambda (usage)
                               (multiple-value-bind
                                   (usage-eval-code environment)
                                   (wrap-usage-for-evaluation usage action-name system-name)
                                 (multiple-value-bind
                                     (result errors)
                                     (handler-case (eval (embed-in-environment usage-eval-code environment))
                                       (error (e) (values nil (format nil "~a: ~a" (type-of e) e)))
                                       (:no-error (result) result))
                                   (if errors
                                     (cons nil (list errors))
                                     (list result)))))
                             it)
              (length-of-usages it)))))

(defun get-usage-debug-info (action-name module-name system-name &key failed-only)
  (with-system (system-name)
    (let-a (map-to-usages (lambda (usage)
                            (multiple-value-bind
                                (usage-eval-code environment cleanup-function)
                                (wrap-usage-for-evaluation usage action-name system-name)
                              (let ((result
                                     (handler-case (eval (embed-in-environment usage-eval-code environment))
                                       (error (e) (values nil (format nil "~a: ~a" (type-of e) e)))
                                       (:no-error (result) result))))
                                (unless (and failed-only result)
                                  `(,result ,usage ,environment ,cleanup-function)))))
                          (load-usages action-name module-name system-name))
      (if failed-only
        (remove-if #'null it)
        it))))

(defun equality-predicate? (symbol)
  (member symbol '(eq eql equal equalp my-equal = string= char= not-null null)))

(defun present-usage (usage system-name action-name)
  (multiple-value-bind
      (result env errors)
      (let* ((result (first usage))
             (code (second usage))
             (top-action (first code)))
        (if (equality-predicate? top-action)
          (if result
            (full-view-debug (let ((to-debug-code (second code)))
                               (if (member action-name (flatten to-debug-code))
                                 to-debug-code
                                 (third code)))
                             (third usage))
            (case top-action
              (t
               (full-view-debug (second usage) (third usage)))))
          (full-view-debug (second usage) (third usage))))
      (declare (ignore env))
      (layout-full-view-debug result errors system-name
                              :ignore-top-level t
                              :skip-expansion (lambda (to-expand-action-name)
                                                (eq action-name to-expand-action-name)))))

(defun layout-debugged-usages (action-name module-name system-name &key failed-only)
  (with-system (system-name)
    (labels ((rec (usage)
               (if (stringp (first usage))
                 (html (:li (:ul
                             (:li :class "title" (:print (first usage)))
                             (loop for sub-usage in (rest usage)
                                   do (rec sub-usage)))))
                 (prog1
                     (html
                       (:li :class "usage"
                        (present-usage usage system-name action-name)))
                   (awhen (fourth usage) (funcall it))))))
      (dolist (usage (get-usage-debug-info action-name module-name system-name :failed-only failed-only))
        (html (:ul (rec usage)))))))

(defun trim-passed-usages (evaluated-usages &optional (transform-failed #'identity))
  (labels ((rec (usage)
             (if (stringp (first usage))
               (cons (first usage)
                     (loop for sub-usage in (rest usage)
                           if (rec sub-usage)
                           collect it))
               (when (null (first usage))
                 (funcall transform-failed (second usage))))))
    (loop for usage in evaluated-usages
          collect (rec usage))))

(defun compute-action-usage-stat (usages)
  (let ((passed 0) (failed 0) (failed-to-be-performed 0))
    (labels ((rec (usage)
               (if (stringp (first usage))
                 (dolist (sub-usage (rest usage))
                   (rec sub-usage))
                 (if (first usage)
                   (incf passed)
                   (if (second usage)
                     (incf failed-to-be-performed)
                     (incf failed))))))
      (dolist (usage usages)
        (rec usage)))
    (list :ok passed :warning failed-to-be-performed :error failed)))

(define-hierarchical-cache :usages (((merge-pathnames "usages/"
                                                      (cache-directory "my-lisp-parsing"))
                                     (cache-directory "my-lisp-parsing"))
                                    :root (nil
                                           ("usages" print-root-stats))
                                    :system ((lambda (item &rest names)
                                               (declare (ignore names))
                                               (print-only item :name :stats)))
                                    :module ((lambda (item &rest names)
                                               (declare (ignore names))
                                               (print-only item :name :stats)))
                                    :action ((lambda (action-item &rest names)
                                               (declare (ignore names))
                                               action-item)))
  :action (let* ((usages (eval-usages action-name module-name system-name))
                 (stats (compute-action-usage-stat usages)))
            (list :usages usages :stats stats
                  :messages (list (make-message action-name
                                                (when (< 0 (getf stats :warning))
                                                  (list (format nil "~a usage~:[s~;~] failed to be verified"
                                                                (getf stats :warning)
                                                                (= 1 (getf stats :warning)))))
                                                (when (< 0 (getf stats :error))
                                                  (list (format nil "~a usage~:[s~;~] failed"
                                                                (getf stats :error)
                                                                (= 1 (getf stats :error)))))))))
  :module (list :stats (totalize-encompassed-stats (get-from-hierarchical-cache :usages system-name module-name :in)))
  :system (list :stats (totalize-encompassed-stats (get-from-hierarchical-cache :usages system-name :in)))
  :root (system-list-all-loaded))

(defun refresh-usages-cache ()
  (with-hierarchical-cache (:usages :write t)
    (refresh)))