(in-package :to-web)

(defun-to-external-program npm ("NodeJS" "node" "-v")
  (cmd arguments &key current-directory)
  "Call NPM."
  (call-command (cons "npm" (cons cmd arguments))
                :current-directory current-directory))

(defun-to-external-program node ("NPM" "npm" "-v")
  (filename &key arguments current-directory output-stream guarded wait)
  "Execute the given file (which must be a Javascript file) with NodeJS."
  (call-command (cons "node" (cons filename arguments))
                :current-directory current-directory
                :guarded guarded
                :output-stream output-stream
                :wait wait))

(defmacro with-post-parameters ((&rest symbols) &body body)
  `(let ,(let (bindings)
           (dolist (sym symbols)
             (if (atom sym)
              (push `(,sym
                      (post-parameter ,(string-downcase (symbol-name sym))))
                    bindings)
               (dolist (n (rest sym))
                 (push `(,n
                         (funcall ,(first sym)
                                  (post-parameter ,(string-downcase (symbol-name n)))))
                       bindings))))
           (nreverse bindings))
     ,@body))

(defmacro with-get-parameters ((&rest symbols) &body body)
  `(let ,(let (bindings)
           (dolist (sym symbols)
             (if (atom sym)
              (push `(,sym
                      (get-parameter ,(string-downcase (symbol-name sym))))
                    bindings)
               (dolist (n (rest sym))
                 (push `(,n
                         (funcall ,(first sym)
                                  (get-parameter ,(string-downcase (symbol-name n)))))
                       bindings))))
           (nreverse bindings))
     ,@body))

(defmacro as-jsonp ((&rest symbols) &body answer)
  `(with-get-parameters ,(append '(callback) symbols)
     (setf (content-type*) "application/javascript")
     (write-string (concatenate 'string callback "("
                                (progn ,@answer)
                                ")")
                   *standard-output*)))
