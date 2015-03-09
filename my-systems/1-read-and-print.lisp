(in-package :my-systems)

(defmacro with-custom-io-syntax (&body body)
  "My own io syntax convention : pretty print is turned on
  and the chosen case is downcase."
  `(let ((*print-array* t)
                                (*print-base* 10)
                                (*print-case* :downcase)
                                (*print-circle* nil)
                                (*print-escape* t)
                                (*print-gensym* t)
                                (*print-length* nil)
                                (*print-level* nil)
                                (*print-lines* nil)
                                (*print-miser-width* nil)
                                (*print-pretty* t)
                                (*print-radix* nil)
                                (*print-readably* t)
                                (*print-right-margin* nil)
                                (*read-base* 10)
                                (*read-eval* t)
                                (*read-suppress* nil))
                            ,@body))

(defmacro with-system ((system-name) &body body)
  "Wrap the execution of the body with *package* bound to the package of the given system.
  If nil, we fall back on the draft system."
  (with-gensyms (name package)
    `(let* ((,name
             ,system-name)
            (*package*
             (let ((,package (system-package ,system-name)))
               (if ,package ,package (find-package "CL-USER")))))
       ,@body)))

(defmacro json-preformatter ((system-name &key with-system) &body body)
  (if with-system
      (with-gensyms (name)
        `(let ((,name
                ,system-name))
           (with-system (,name)
             (json<-lisp (progn
                           ,@body)))))
    `(json<-lisp (progn
                   ,@body))))

(defun read-code-chunk (code-chunk system-name)
  "Return a list of Lisp forms from the given string 'chunk'.
          The given chunk is assumed to come from an editor.
          This imply a fundamental property : Lisp forms are enumerated
          rather than listed in a global s-expression."
  (with-system (system-name)
    (with-input-from-string (s code-chunk)
      (with-custom-io-syntax
        (loop for form = (read s nil nil) while form collect form)))))

(defun print-code-chunk
       (form system-name &key print-unreadably prevent-circularity)
  "Return a string obtained by printing the given form."
  (let ((str
         (with-system (system-name)
           (with-output-to-string (s)
             (with-custom-io-syntax
               (let ((*print-readably* (not print-unreadably))
                     (*print-circle* prevent-circularity))
                 (write form :stream s)))))))
    (if (string= system-name "my-backquote")
        str
      (my-backquote:print-backquote str))))

(defmacro write-lisp-form
          (form system-name &optional (stream *standard-output*))
  `(progn
     (write-string (print-code-chunk ,form
                                     ,system-name)
                   ,stream)
     (write-char #\Newline ,stream)
     (write-char #\Newline ,stream)))

(defmacro with-lisp-file (filename system-name &body body)
  (with-gensyms (stream)
    `(with-open-file (,stream
                      ,filename
                      :direction
                      :output
                      :if-exists
                      :append
                      :if-does-not-exist
                      :create)
       ,@(loop for form in body
               collect `(write-lisp-form ,form
                                         ,system-name
                                         ,stream)))))

(defun symbol-system-name (symbol)
  (system-name? (string-downcase (package-name (symbol-package symbol)))))

(defun form-type (form)
  "One of: :variable, :parameter, :constant, :function, :macro, :usage or :fact.
          Return as the second value the id which is the name of the variable/parameter/etc or the header string of a usage or the whole form if it is a fact, except for an atom."
  (if (atom form)
      (values :fact nil)
    (if (stringp (first form))
        (values :usage (first form))
      (case (first form)
        (defvar (values :variable (second form)))
        (defparameter (values :parameter (second form)))
        (defconstant (values :constant (second form)))
        (defun (values :function (second form)))
        (defmacro (values :macro (second form)))
        (defsetf (values :simple-setf (second form)))
        (def-access (values :access (second form)))
        (t (values :fact form))))))

(defun is-definition? (form)
  (member (form-type form) '(:variable :parameter :constant :function :macro :simple-setf :access)))

(defun form-id (form)
  "Get the id of the given form."
  (multiple-value-bind
      (type id)
      (form-type form)
    (declare (ignore type))
    id))

(defun form-key (form)
  (multiple-value-bind
      (type id)
      (form-type form)
    (list type id)))

(defun form-key-pure-name (form)
  (multiple-value-bind
      (type id)
      (form-type form)
    (list type
          (if (symbolp id)
            (symbol-name id)
            id))))

(defun form-key-pure-name<-form-key (form-key)
  (list (first form-key)
        (let-a (second form-key)
          (if (symbolp it)
            (symbol-name it)
            it))))