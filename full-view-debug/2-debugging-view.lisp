(in-package :full-view-debug)

(defvar *skip-expansion* nil
  "A function bound to *skip-expansion* that returns true when passed a given action-name will prevent the debugged view to hold details about the inner computations of that action.")

(defvar *highlight-action* nil
  "A function bound to *highlight-expansion* that returns true when passed a given action-name will emphasize the call to that action.")

(defun quote-mklist (lst)
  (if (or (atom lst)
          (eq 'quote (first lst)))
    (list lst)
    lst))

(defun abstract-function-value? (value)
  (and (not (atom value))
       (= 2 (length value))
       (eq :function (first value))))

(defun layout-value (value system-name &key secondary-value)
  "Layout a value, printing into as a string as if in the system if given name.
If an atom, wrap in a span; if a list, wrap in a preformatted tag to have the value printed prettily.
If secondary-value is true, the class used is secondary-value instead of value."
  (let ((value-class-name (cond
                           (secondary-value "secondary-value")
                           ((keywordp value) "keyword")
                           (t "value")))
        (string (print-code-chunk value system-name :print-unreadably t :prevent-circularity t)))
    (if (atom value)
      (html
        (:span :class (if (null value) "nil" value-class-name)
         (:print string)))
      (html
        (:div :class value-class-name
         (:pre (:print string)))))))

(defun layout-bindings (bindings system-name)
  "Layout the given bindings, meaning it layouts each according to the top layout function but declaring the variable name as the block name of the layout."
  (html
    (:ul :class "bindings"
     (dolist (binding bindings)
       (html
         (:li
          (if (= (length binding) 2)
            (layout (quote-mklist (second binding)) system-name :block-name (first binding))
            (html
              (:span :class "name" (first binding))
              (:span :class "action" (second binding))
              (:span :class "value" (print-code-chunk (third binding) system-name :prevent-circularity t))))))))))

(defun layout-scope (code system-name)
  "Layout the given code as a new scope: first layout the bindings of the scope, then layout according to the top layout function.
In addition, if the first element of the given code is a function name, it means the scope we layout is for a function whose inner details we know. Therefore, it changes the system-name to the birth system of that action in order for all further code to be printed as if from inside the function."
  (let* ((action-name (first code))
         (as-source (and (symbolp action-name)
                         (cl-function? action-name)))
         (skip? (when (and as-source (functionp *skip-expansion*))
                  (funcall *skip-expansion* action-name))))
    (html
      (:div :class "pure-u"
       (:div :class "evaluation"
        (if skip?
          (layout-operation action-name system-name)
          (layout-value action-name system-name)))
       (unless skip?
         (let ((system-name (if as-source (symbol-system-name action-name) system-name)))
           (html (:div :class "new-scope"
                  (layout-bindings (second code) system-name)
                  (layout (third code) system-name)))))))))

(defun layout-arguments (code system-name)
  "Layout the given code as arguments, meaning it layouts each according to the top layout function.
In addition, it detects if the action is progn and if so, de-emphasize all values except the ones of the last perimeter to mirror the fact the result of a progn perimeter is the result of the last perimeter."
  (let ((progn-discard (eq 'progn (first code))))
    (html
      (:ul :class "arguments pure-u"
       (loop for sub-code in (rest code)
             for i from 1
             do (html (:li :class (when (and progn-discard
                                             (< i (length (rest code))))
                                    "discard")
                       (layout sub-code system-name))))))))

(defun layout-operation (scope system-name &key lexically-bound macro)
  "Layout the given operation, usually just a symbol.
In that classic case, the key arguments serve to adapt the class-name to a variable name or a macro, the default being an action. To be clear, operation here means a macro use, another action use or a variable lookup.
Unclassical cases are:
- handling a setq perimeter: by printing the bindings that have changed
- handling a lambda form: by building the layout of a new scope
- handling let, let*, a top-level form: it is also by build the layout of a new scope"
  (cond
   ((atom scope)
    (html
      (:span :class (cond
                     (lexically-bound "name")
                     (macro "macro")
                     (t "action"))
       (:print (print-code-chunk scope system-name)))))
   ((eq 'setq (first scope))
    (html
      (:span :class "action" "setq")
      (layout-bindings (second scope) system-name)))
   (t
    (layout-scope scope system-name))))

(defun layout-result (result system-name)
  "Layout the given result, by default forwarding the job to layout-value.
Beforehand, catches:
- a list of two element, the first being :function
- multiple values (each being printed with layout-value)."
  (cond
   ((functionp result)
    nil)
   ((abstract-function-value? result)
    (layout-operation (second result) system-name))
   ((and (not (atom result))
         (eq 'values-list (first result)))
    (html
      (:ul :class "pure-u values-list"
       (loop for value in (second (second result))
             for i from 0
             do (html (:li (layout-value value system-name :secondary-value (< 0 i))))))))
   (t
    (layout-value result system-name))))

(defun layout-operation-call (code system-name)
  (awhen (second code)
    (if (abstract-function-value? it)
      (layout-operation (second it) system-name)
      (layout-operation it system-name :lexically-bound (and (null (third code))
                                                             (not (functionp (first code)))))))
  (awhen (rest code)
    (layout-arguments it system-name)))

(defun layout (code system-name &key block-name)
  "Layout the given code as if being in the given system.
The basic layout is: result, operation-name, each arguments. That covers only function though and variable lookup (the operation-name being the name of the variable and arguments are ignored).
This function also:
- detect the :error tag and add the in-error class-name to the whole layout
- detect the :macro tag: the macro name is inserted between the result and the operation-name it is a replacement of
- accept a block-name to layout a binding"
  (let* ((in-error (equal :error (second code)))
         (as-macro (equal :macro (second code)))
         (inner-code (if (or in-error as-macro)
                       (rest code)
                       code))
         (highlight? (let ((action-name (second inner-code)))
                       (when (and (symbolp action-name)
                                  (cl-action? action-name)
                                  (functionp *highlight-action*))
                         (funcall *highlight-action* action-name)))))
    (html
      (:div :class "pure-g"
       (:div :class (mkstr "evaluation pure-u"
                           (if in-error " in-error" "")
                           (if as-macro " in-macro" "")
                           (if highlight? " highlight" ""))
        (when block-name
          (html (:span :class "name"
                 (:print (print-code-chunk (or block-name (third code)) system-name)))))
        (layout-result (first code) system-name)
        (if as-macro
          (progn
            (layout-operation (second inner-code) system-name :macro t)
            (layout-operation-call (rest inner-code) system-name))
          (layout-operation-call inner-code system-name)))))))

(defun layout-full-view-debug (result errors system-name &key ignore-top-level skip-expansion highlight-action)
  (html
    (:div :class "debug"
     (when errors
       (html (:ul
              (dolist (error-message errors)
                (html (:li (:h4 :class "error" (:print error-message))))))))
     (let ((*skip-expansion* skip-expansion)
           (*highlight-action* highlight-action))
       (layout (if ignore-top-level
                 (ignore-top-level result)
                 result)
               system-name)))))

(defun make-test-file ()
  (flet ((my-debug-layout (perimeter environment &optional (system-name "full-view-debug"))
           (multiple-value-bind
               (result environment errors)
               (full-view-debug perimeter environment)
             (declare (ignore environment))
             (layout-full-view-debug result errors system-name))))
    (macrolet ((test (lst &optional (system-name "full-view-debug"))
                 (with-gensyms (perimeter i)
                   `(loop for ,perimeter in ,lst
                          for ,i from 0
                          when (< 0 ,i)
                          ;do (html (:hr))
                          do (my-debug-layout ,perimeter nil ,system-name)))))
      (with-html-output-to-file ((merge-pathnames "full-view-debug.html" (system-work-directory "full-view-debug")))
        (base-page ("Full view debug")
          (:h2 "Atoms")
          (test (list nil t :test 1 #\a "test" ''test #'+))
          (:h2 "Variable")
          (my-debug-layout 'test '((let ((test 1)))))
          (:h2 "Function calls")
          (test (list '(substitute #\/ #\\ "test/pour\\voir")
                      '(first (last (pathname-directory #p"c:/home/thomas/dev/")))
                      '(equal "test pour voir!" (concatenate
                                                 'string
                                                 (string-downcase
                                                  (mkstr :test
                                                         #\Space
                                                         (as-keyword "pour")
                                                         #\Space
                                                         'voir))
                                                 "!"))
                      '(= (+ 1 (* 2 3)) (+ 3 (* 2 2)))))
          (:h2 "Function values")
          (test (list '(find "test" nil :test #'string= :key (lambda (x) (first (second x))))))
          (:h2 "Using the top-level environment")
          (my-debug-layout '(+ test x) '((let ((test 1) (x 2)))))
          (my-debug-layout '(+ test x) '((let ((test 1))) (let ((x 2)))))
          (my-debug-layout '(+ (foo 2) (bar 2)) '((flet ((foo (x) (* x 2)) (bar (x) (* x 3))))))
          (my-debug-layout '(+ (baz 2) (bulu 2)) '((labels ((baz (x) (+ x 2))
                                                            (bulu (x) (* (baz x)
                                                                         (if (< x 10)
                                                                           (bulu (+ 10 x))
                                                                           3)))))))
          (my-debug-layout '(+ (foo 2) (bar 2) (baz 2) (bulu 2))
                           '((flet ((foo (x) (* x 2)) (bar (x) (* x 3))))
                             (labels ((baz (x) (+ x 2))
                                      (bulu (x) (* (baz x)
                                                   (if (< x 10)
                                                     (bulu (+ 10 x))
                                                     3)))))))
          (:h2 "Lambda")
          (test (list '((lambda (x) (+ x 3)) 5)))
          (:h2 "Values")
          (test (list '(values (+ 1 2) (* 3 4))
                      '(values (list 1 2) (* 3 4))
                      '(values (+ 1 2) (list 3 4))
                      '(values (+ 1 2) (list 1 2) (* 3 4) (list 3 4))))
          (:h2 "Special operators")
          (test (list '(progn (+ 1 2) (* 3 4))
                      '(mapcar #'1+ (list 1 2 3))
                      '(let ((x (+ 1 2)) (y (* 3 4))) (+ x y))
                      '(let ((x (list 1 2)) y) (append x y))
                      '(let (a b c) (setq a 1 b 2 c 3))
                      '(let (a b c) (setq a 1 b (incf a) c (incf b)))
                      '(if (eq 'test 'test) (+ 1 1) (+ 2 2))))
          (:h2 "Calls to function with source code available")
          (test (list '(no-backslashes "test/pour\\voir")
                      '(last-directory #p"c:/home/thomas/dev/")
                      '(get-canonical-key :test '((:true-key :test)))
                      '(file-equal #p"c:/home/a.txt" #p"c:/home/b.txt"))
                "my-utilities")
          (:h2 "Errors")
          (test (list 'test
                      '(+ 1 nil)
                      '(* 2 (+ 1 nil))
                      '(* (+ 2 nil) (+ 1 nil))
                      '(+ 1 (* (+ 2 nil) (+ 1 nil)))
                      '(when (+ 1 nil) (* 2 2))
                      '(function foo)
                      '(progn (+ 1 nil) (* 2 2))
                      '(if (+ 1 nil) (+ 2 nil) (+ 3 3))
                      '(if (+ 1 1) (+ 2 nil) (+ 3 3))
                      '(let ((x (+ 1 nil)) (y (+ 2 2))) (+ x y))
                      '(let ((x (+ 1 1)) y) (+ x y))
                      '(let (a b c) (setq a (+ 1 nil) b 2 c 3))
                      '(let (a b c) (setq (+ 1 1) (+ 1 1) b 2 c 3))
                      '(let* ((x (+ 1 nil)) (y (+ 2 2))) (+ x y))
                      '(let* ((x (+ 1 1)) y) (+ x y))
                      '((lambda (x) (+ x 3)) (+ 5 nil))
                      '((lambda (x) (+ x nil)) 5)
                      )))))))
#|
(defun test-special-debugging-views ()
  (macrolet ((test (lst)
               (with-gensyms (perimeter i)
                 `(loop for ,perimeter in ,lst
                        for ,i from 0
                        when (< 0 ,i)
                        do (with-html (:hr))
                        do (my-debug-layout ,perimeter nil)))))
    (webapp-page ("Test special debugging views" :styles ("web/css/pure-min.css" "web/css/main.css")
                                                 :scripts ("web/js/main.js"))
      (:h2 "when")
      (test (list '(when (= 1 2) 7)
                  '(when (= 1 1) 7)
                  '(when (= 1 1) (+ 3 4))
                  '(when (= 1 1) (+ 3 4) (* 3 4))))
      (:h2 "and")
      (test (list '(and (= 1 1) (= 1 2))
                  '(and (= 1 1) t (= 1 2))))
                  )))
|#