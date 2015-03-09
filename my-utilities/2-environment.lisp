(in-package :my-utilities)

(defun make-binding
       (binding-name &optional
        (binding-value nil binding-value-provided-p))
  "Make a binding out of the given binding name and binding value.
Return nil if no binding name is provided, return only a binding name if no value is provided.
Otherwise return the binding as a classic pair (name value)."
  (when binding-name
    (if binding-value-provided-p
        (list binding-name binding-value)
      binding-name)))

(defun binding-name (binding)
  "Give the name of the binding."
  (if (atom binding) binding (first binding)))

(defun binding-value (binding)
  "Give the value of the binding (nil by default)."
  (unless (atom binding) (second binding)))

(defun make-bindings (binding-names binding-values)
  "Collect as bindings the separate binding names and binding values.
Stops as soon it has exhausted the binding names.
However, exhausting the binding values does not stop the process but rather make binding of all the rest of the names to nil."
  (loop for binding-name in binding-names
        for binding-value in
            (append binding-values
                    (loop for i from 1 to
                              (- (length binding-names)
                                 (length binding-values))
                          collect nil))
        collect (make-binding binding-name binding-value)))

(defun parse-destructuring-lambda-list (lst allow-recursive-call?)
  (let (symbols required-variables-parsed?)
    (flet ((collect-variable (element)
             (if (listp element)
                 (if required-variables-parsed?
                     (error "")
                   (if allow-recursive-call?
                       (setf symbols
                             (append (nreverse (parse-destructuring-lambda-list element
                                                                                nil))
                                     symbols))
                     (error "")))
               (push element symbols)))
           (collect-first (element)
             (push (first (mklist element)) symbols)))
      (let ((fn #'collect-variable))
        (dolist (element lst)
          (case element
            (&allow-other-keys)
            ((&aux &key &optional)
             (setf fn #'collect-first required-variables-parsed? t))
            ((&body &rest)
             (setf fn #'collect-variable required-variables-parsed? t))
            ((&environment &whole) (setf fn #'collect-variable))
            (t (funcall fn element))))))
    (nreverse symbols)))

(defun bindings<-lambda-list (lambda-list lst)
  (let (bindings (lambda-list-index 0) (lst-index 0))
    (labels ((collect-variables (exclude-list)
               (loop for variable in
                         (subseq lambda-list lambda-list-index)
                     while (not (member variable exclude-list))
                     collect variable))
             (parse-required-parameters ()
               (let ((variables
                      (collect-variables '(&optional &rest &key))))
                 (if (< (length lst) (length variables))
                     (error "~a values for ~a variables: ~a <-> ~a"
                            (length lst)
                            (length variables)
                            lst
                            variables)
                   (progn
                     (loop for variable in variables
                           for index from lst-index
                           do (push (make-binding variable
                                                  (elt lst index))
                                    bindings))
                     (incf lambda-list-index (length variables))
                     (incf lst-index (length variables))))))
             (collect-optional-binding (special-variable lst-index)
               (let ((variable (first (mklist special-variable)))
                     (default-value (second (mklist special-variable)))
                     (supplied? (< lst-index (length lst))))
                 (if supplied?
                     (push (make-binding variable (elt lst lst-index))
                           bindings)
                   (push (make-binding variable default-value)
                         bindings))
                 (when (and (listp special-variable)
                            (= 3 (length special-variable)))
                   (push (make-binding (third special-variable)
                                       supplied?)
                         bindings))))
             (next-key? (key)
               (eq key
                   (when (< lambda-list-index (length lambda-list))
                     (elt lambda-list lambda-list-index))))
             (parse-optional-parameters ()
               (when (next-key? '&optional)
                 (incf lambda-list-index)
                 (let ((variables (collect-variables '(&rest &key))))
                   (loop for variable in variables
                         for index from lst-index
                         do (collect-optional-binding variable index))
                   (incf lambda-list-index (length variables))
                   (incf lst-index (length variables)))))
             (parse-rest-parameters ()
               (when (next-key? '&rest)
                 (incf lambda-list-index)
                 (awhen (collect-variables '(&key))
                   (push (make-binding (first it)
                                       (when (< lst-index (length lst))
                                         `',(subseq lst
                                                              lst-index)))
                         bindings)
                   (incf lambda-list-index))))
             (collect-key-binding (special-variable lst)
               (let* ((variable (first (mklist special-variable)))
                      (default-value
                       (second (mklist special-variable)))
                      (found? (position (as-keyword variable) lst)))
                 (if found?
                     (push (make-binding variable
                                         (elt lst (1+ found?)))
                           bindings)
                   (push (make-binding variable default-value)
                         bindings))
                 (when (and (listp special-variable)
                            (= 3 (length special-variable)))
                   (push (make-binding (third special-variable)
                                       (when found? t))
                         bindings))))
             (parse-key-parameters ()
               (when (next-key? '&key)
                 (incf lambda-list-index)
                 (let ((variables
                        (subseq lambda-list lambda-list-index))
                       (lst (subseq lst lst-index)))
                   (loop for variable in variables
                         do (collect-key-binding variable lst))))))
      (parse-required-parameters)
      (parse-optional-parameters)
      (parse-rest-parameters)
      (parse-key-parameters)
      (nreverse bindings))))

(defun make-let-scope (bindings)
  "Make a let scope from the given bindings."
  `(let ,bindings))

(defun make-flet-scope (local-function-definitions)
  `(flet ,local-function-definitions))

(defun make-labels-scope (local-function-definitions)
  `(labels ,local-function-definitions))

(defun make-environment () (list))

(defun add-scope (scope environment)
  "Add the given scope to the given environment as its last scope and, consequently, as the one of highest priority."
  (push scope environment))

(defun add-binding (binding environment)
  "Add the given binding to the environment by including it in the last scope, provided it is a let scope.
Return the resulting environment."
  (when (and binding (eq 'let (first (first environment))))
    (setf (first environment)
          `(let ,(append (second (first environment))
                            (list binding)))))
  environment)

(defun remove-scope (environment)
  "Remove the last scope from the given environment (meaning the one of highest priority) and return the resulting environment."
  (pop environment)
  environment)

(defun modify-binding (binding-name binding-value environment)
  "Modify the currently active binding of given name, if present, by setting it to the given value.
Return the modified environment."
  (let (found)
    (loop for scope in environment
          for scope-position from 0
          while (not found)
          if (eq 'let (first scope))
            do (loop for binding in (second scope)
                     for binding-position from 0
                     while (not found)
                     if (eq binding-name (binding-name binding))
                       do (setf (elt (second scope) binding-position)
                                (list binding-name binding-value)
                                found t))))
  environment)

(defun find-action (name environment)
  (let (found type code)
    (loop for scope in environment
          while (not found)
          if (or (eq 'flet (first scope))
                 (eq 'labels (first scope))
                 (eq 'macrolet (first scope)))
            do (loop for definition in (second scope)
                     while (not found)
                     do (when (eq name (first definition))
                          (setf found t
                                type
                                (if (eq 'macrolet (first scope))
                                    :install
                                  :run)
                                code definition))))
    (values type code)))

(defun find-symbol-macrolet (name environment)
  (let (found code)
    (loop for scope in environment
          while (not found)
          if (eq 'symbol-macrolet (first scope))
            do (loop for definition in (second scope)
                     while (not found)
                     do (when (eq name (first definition))
                          (setf found t code definition))))
    code))

(defun embed-in-environment (code environment)
  "Embed the given code in the given environment, if any."
  (labels ((rec (scopes acc)
             (if (null scopes)
                 acc
               (rec
                (rest scopes)
                (append (first scopes) (list acc))))))
    (if environment (rec environment code) code)))

