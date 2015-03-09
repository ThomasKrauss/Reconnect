(in-package :my-lisp-parsing)

(defun make-message (id warnings errors)
  (as-plist id warnings errors))

(defun eval-compile-form (form)
  "Return multiple values: result, errors, warnings"
  (labels ((collect-msg (lst)
             (when (and lst
                        (listp lst))
               (mapcar (lambda (item)
                         (format nil "~A" item))
                       lst)))
           (compile-action (id definition form)
             (multiple-value-bind
                 (name warnings errors)
                 (compile id definition)
               (declare (ignore name))
               (when (and (stringp (fourth form))
                          (fifth form))
                 (setf (documentation id 'function) (fourth form)))
               (values id
                       (collect-msg (cdar warnings))
                       (collect-msg errors)))))
    (when (is-definition? form)
      (multiple-value-bind
          (type id)
          (form-type form)
        (case type
          (:function
           (compile-action id `(lambda ,@(cddr form)) form))
          (:macro
           (let (eval-errors)
             (handler-case (eval form)
               (condition (c)
                 (push (format nil "~A" c) eval-errors)))
             (aif eval-errors
                  (values id nil it)
                  (compile-action id (macro-function id) form))))
          (t
           (handler-case (values (eval form)
                                 nil
                                 nil)
             (condition (c)
               (values nil
                       (list (format nil "~A" c))
                       nil)))))))))

(defun eval-action (action-name module-name system-name)
  (multiple-value-bind
      (name warnings errors)
      (with-system (system-name)
        (eval-compile-form (load-action-definition action-name module-name system-name)))
    (declare (ignore name))
    (make-message (print-code-chunk action-name system-name)
                  warnings
                  errors)))

(define-hierarchical-cache :compile-problems (((merge-pathnames "compile-problems/"
                                                               (cache-directory "my-lisp-parsing"))
                                              (cache-directory "my-lisp-parsing"))
                                             :root (nil
                                                    ("compile-problems" print-root-stats))
                                             :system ((lambda (item &rest names)
                                                        (declare (ignore names))
                                                        (print-only item :name :stats)))
                                             :module ((lambda (item &rest names)
                                                        (declare (ignore names))
                                                        (print-only item :name :stats)))
                                             :action ((lambda (action-item &rest names)
                                                        (declare (ignore names))
                                                        action-item)))
  :action (let* ((message (eval-action action-name module-name system-name))
                 (stats (list :warning (length (getf message :warnings))
                              :error (length (getf message :errors)))))
            (list :messages (list message) :stats stats))
  :module (list :stats (totalize-encompassed-stats
                        (get-from-hierarchical-cache :compile-problems system-name module-name :in)))
  :system (list :stats (totalize-encompassed-stats (get-from-hierarchical-cache :compile-problems system-name :in)))
  :root (system-list-all-loaded))

(defun refresh-compile-problems-cache ()
  (with-hierarchical-cache (:compile-problems :write t)
    (refresh)))