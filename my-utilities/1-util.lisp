(in-package :my-utilities)

(defun enlist (&rest args)
  (apply #'append
         (mapcar (lambda (item) (when item (list item))) args)))

(defun format-date (year month day)
  "Example:
(format-date 2013 06 10)
gives
June 10th, 2013"
  (let ((last-day-number
         (let-a (parse-integer (let-a (format nil "~a" day)
                                 (subseq it (1- (length it)))))
           (if (< 3 it) 0 it)))
        (month-format
         "~[January~;February~;March~;April~;May~;June~;July~;August~;September~;October~;November~;December~]"))
    (format nil
            (mkstr month-format " ~a~[th~;st~;nd~;rd~], ~a")
            (1- month)
            day
            last-day-number
            year)))

(defmacro def-chainable-setf (name args place reading-form)
  (let ((gensym-bindings
         (mapcar (lambda (name)
                   `(,(symb 'internal- name)
                               (gensym)))
                 (append args `(,place)))))
    `(define-setf-expander ,name
                                     ,(append args
                                              `(,place
                                                          &environment
                                                          env))
                 (multiple-value-bind (vars
                                       vals
                                       store-vars
                                       writer-form
                                       reader-form)
                     (get-setf-expansion ,place env)
                   (declare (ignore writer-form))
                   (let (,@gensym-bindings
                         (result (gensym)))
                     (values (append vars
                                     (list ,@(mapcar #'first
                                                    gensym-bindings)
                                           (first store-vars)))
                             (append vals
                                     (list ,@(append args
                                                    `(,place))
                                           reader-form))
                             `(,result)
                             `(setf ,,(cons 'list
                                                      (loop for expr in
                                                                reading-form
                                                            if (eq place
                                                                   expr)
                                                              collect 'reader-form
                                                            else
                                                              collect (list 'quote
                                                                            expr)))
                                              ,result)
                             `(,',name
                                         ,,@(append (butlast (mapcar #'first
                                                                    gensym-bindings))
                                                   '(reader-form)))))))))

(defmacro def-access (name args place reading-form)
  `(progn
               (defun ,name
                      ,(append args (list place))
                 ,reading-form)
               (def-chainable-setf ,name
                                   ,args
                                   ,place
                                   ,reading-form)))

(defun get-bound-symbols (bindings)
  "Gets the symbols used in bindings such as let, let*, flet and labels."
  (mapcar (lambda (binding)
            (if (listp binding) (first binding) binding))
          bindings))

(defun normalize-string (lst nb)
  (let (result)
    (flet ((normalize (str)
             (let ((size 0) word sentence result)
               (labels ((commit-sentence ()
                          (push (join-str (nreverse sentence) #\Space)
                                result)
                          (setf sentence nil size (length word)))
                        (commit-word ()
                          (let ((target-size
                                 (+ (length word)
                                    (if (/= 0 size) (1+ size) 0))))
                            (if (< target-size nb)
                                (progn
                                  (push (concatenate 'string
                                                     (nreverse word))
                                        sentence)
                                  (setf word nil size target-size))
                              (progn
                                (commit-sentence)
                                (push (concatenate 'string
                                                   (nreverse word))
                                      sentence)
                                (setf word nil))))))
                 (loop for char across str
                       if (char= char #\Space)
                         do (commit-word)
                       else
                         do (push char word))
                 (when word
                   (commit-word)
                   (when sentence (commit-sentence)))
                 (nreverse result)))))
      (dolist (str lst)
        (if (= 0 (length str))
            (push str result)
          (dolist (normalized-string (normalize str))
            (push normalized-string result)))))
    (nreverse result)))

(defun identity-ignore-rest (value &rest to-ignore)
  (declare (ignore to-ignore))
  value)

(defun rest-or-one (lst)
  (if (= (length lst) 1)
      (first lst)
    `(list ,@lst)))

(defun group (source n)
  (when (< n 0) (error "negative length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                 (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defun mklist (obj) (if (listp obj) obj (list obj)))

(defun join-str (string-list char)
  (with-output-to-string (s)
    (loop for str in string-list
          for i from 0
          if (< 0 i) do (write-char char s)
          do (write-string str s))))

(defmacro with-gensyms ((&rest names) &body body)
  "Just indispensable. It is known for a long time that macros leak
on variable capture. Generally, it is something we do not want hence
with-gensyms."
  `(let ,(loop for n in names collect
                    `(,n
                                (make-symbol ,(string n))))
               ,@body))

(defmacro once-only ((&rest names) &body body)
  (let ((gensyms
         (loop for n in names collect (gensym (symbol-name n)))))
    `(let (,@(loop for g in gensyms
                            collect `(,g
                                                (gensym))))
                 `(let (,,@(loop for g in gensyms
                                          for n in names
                                          collect ``(,,g
                                                                         ,,n)))
                              ,(let (,@(loop for n in names
                                            for g in gensyms
                                            collect `(,n
                                                                ,g)))
                                 ,@body)))))

(defmacro mac (expr)
  (with-gensyms (s)
    `(with-output-to-string (,s)
                 (pprint (macroexpand-1 ',expr)
                         ,s))))

(defun mkstr (&rest args)
  (with-output-to-string (s) (dolist (a args) (princ a s))))

(defun symb (&rest args) (values (intern (apply #'mkstr args))))

(defun as-keyword (&rest args)
  (values (intern (string-upcase (apply #'mkstr args)) :keyword)))

(defun get-canonical-key (key similar-keys)
  (loop for spec in similar-keys
        if (member key spec) return (first spec)))

(defun group-values-by-keys (alist &rest similar-keys)
  (let (ignored result)
    (dolist (e alist)
      (let ((canonical-key (get-canonical-key (first e) similar-keys)))
        (setf (getf result canonical-key)
              (concatenate 'list (getf result canonical-key) (rest e)))
        (push e ignored)))
    (values result (nreverse ignored))))

(defun reduce-conc (lst)
  (reduce (lambda (&optional (x nil) (y nil)) (concatenate 'list x y))
          lst))

(defmacro dlambda (&body procedures)
  (with-gensyms (args)
    `(lambda (&rest ,args)
                 (case (car ,args)
                   ,@(mapcar (lambda (proc)
                              `(,(if (eq t (car proc))
                                               t
                                             (list (car proc)))
                                          (apply (lambda
                                                     ,@(cdr proc))
                                                 ,(if (eq t (car proc))
                                                      args
                                                    `(cdr ,args)))))
                            procedures)))))

(defmacro with-machineries (bindings &body body)
  (with-gensyms (args)
    (let ((gensyms
           (loop for binding in bindings
                 collect (gensym (symbol-name (first binding))))))
      `(let ,(loop for binding in bindings for i from 0
                        collect
                        `(,(elt gensyms i)
                                    ,@(rest binding)))
                   (macrolet ,(loop for
                                binding
                                in
                                bindings
                                for
                                i
                                from
                                0
                                collect
                                `(,(first binding)
                                            (&rest ,args)
                                            (list* 'funcall
                                                   ',(elt gensyms i)
                                                   ,args)))
                     ,@body)))))

(defun no-backslashes (str)
  "Substitute any backslash with a slash in the given string."
  (substitute #\/ #\\ str))

(defmacro call-command
          (commands &key guarded current-directory
           (wait nil wait-supplied-p) output-stream (prefix "")
           show-cmd extra-environ)
  "if output-stream is informed, use call-system-showing-output and take into account prefix and show-cmd.
But forces wait to true and ignore guarded.
If no output-stream, use call-system."
  (with-gensyms (call-commands s arg index)
    (append `(,(if output-stream
                             'system:call-system-showing-output
                           'system:call-system)
                        ,(if (null guarded)
                             commands
                           `(let ((,call-commands
                                             ,commands))
                                        (if (and ,guarded
                                                 (not ,output-stream))
                                            (list "cmd"
                                                  "/k"
                                                  (if (stringp ,call-commands)
                                                      ,call-commands
                                                    (with-output-to-string (,s)
                                                      (loop for ,arg
                                                                in
                                                                ,call-commands
                                                            for ,index
                                                                from 0
                                                            if (< 0
                                                                  ,index)
                                                              do (write-char #\Space
                                                                             ,s)
                                                            do (write-string ,arg
                                                                             ,s)))))
                                          ,call-commands))))
            `(:current-directory
                        ,current-directory
                        :wait
                        ,(if (and (not wait-supplied-p) output-stream)
                             t
                           wait)
                        :extra-environ
                        ,extra-environ)
            (when output-stream
              `(:output-stream
                          ,output-stream
                          :prefix
                          ,prefix
                          :show-cmd
                          ,show-cmd)))))

(defmacro my-bind (symbols form &body body)
  (multiple-value-bind (binding-gensyms post-function-calls)
      (let (binding-gensyms post-function-calls)
        (dolist (s symbols)
          (if (atom s)
              (push s binding-gensyms)
            (let ((post-function (first s)))
              (dolist (s (rest s))
                (let ((s-gensym (gensym (symbol-name s))))
                  (push s-gensym binding-gensyms)
                  (push (list s
                              `(funcall ,post-function
                                                  ,s-gensym))
                        post-function-calls))))))
        (values (nreverse binding-gensyms)
                (nreverse post-function-calls)))
    `(multiple-value-bind ,binding-gensyms
                   ,form
                 ,@(if post-function-calls
                      (list `(let ,post-function-calls
                                         ,@body))
                    body))))

(defmacro manual-list-transform (lst &body body)
  "On fixed size lists!"
  (with-gensyms (inner-list)
    (flet ((paste? (clause index)
             (if (equal clause '_)
                 `(elt ,inner-list ,index)
               `,(substitute `(elt ,inner-list
                                                       ,index)
                                       '_
                                       clause
                                       :test
                                       #'equal))))
      `(let ((,inner-list ,lst))
                   (list ,@(loop for clause in body
                                for index from 0
                                collect (paste? clause index)))))))

(defun test-pour (x) (+ x 1))

