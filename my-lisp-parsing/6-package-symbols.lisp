(in-package :my-lisp-parsing)

(defun list-package-symbols (package-name)
  (let (internal-symbols
        external-symbols
        inherited-symbols)
    (awhen (or (find-package package-name)
               (find-package (string-upcase package-name)))
      (with-package-iterator (generator-fn it
                                           :internal :external :inherited)
        (loop     
         (multiple-value-bind (more? symbol accessibility)
             (generator-fn)
           (unless more? (return))
           (case accessibility
             (:internal
              (push symbol internal-symbols))
             (:external
              (push symbol external-symbols))
             (:inherited
              (push symbol inherited-symbols)))))))
    (list :internal internal-symbols
          :external external-symbols
          :inherited (delete-duplicates inherited-symbols :test #'eq))))

(defun printable-package-symbols (package-symbols)
  (flet ((sort-symbol-names (lst)
           (sort (mapcar (lambda (symbol)
                           (string-downcase (symbol-name symbol)))
                         lst)
                 #'string<)))
    (list :internal (sort-symbol-names (getf package-symbols :internal))
          :external (sort-symbol-names (getf package-symbols :external))
          :inherited (sort-symbol-names (getf package-symbols :inherited)))))

(define-hierarchical-cache :package-symbols (((merge-pathnames "package-symbols/"
                                                               (cache-directory "my-lisp-parsing")))
                                                :root (nil)
                                                :system ((lambda (item &rest names)
                                                           (declare (ignore names))
                                                           (let ((printable-item (printable-package-symbols item)))
                                                             (setf (getf printable-item :name) (get-name item))
                                                             printable-item))))
  :system (list-package-symbols system-name)
  :root (cons "keyword" (system-list-all-loaded)))

(defun refresh-package-symbols-cache ()
  (with-hierarchical-cache (:package-symbols :write t)
    (refresh)))

(defun is-symbol-external? (symbol)
  (multiple-value-bind
      (symbol status)
      (find-symbol (symbol-name symbol) (symbol-package symbol) )
    (declare (ignore symbol))
    (eq :external status)))

(defun get-eligible-system-names-for-package-symbols-update (symbols)
  (delete-duplicates (loop for symbol in symbols
                           append (acond
                                   ((keywordp symbol)
                                    (list "keyword"))
                                   ((symbol-system-name symbol)
                                    (cons it (get-dependent-systems it)))))
                     :test #'string=))