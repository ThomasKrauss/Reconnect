(in-package :full-view-debug)

(defmacro define-debugging (name &body body)
  "Body in two parts: i) how to debug ii) how to get result.
Optional documentation string permitted though I don't really know what to do with it for now."
  (let* ((doc (let-a (first body) (when (stringp it) it)))
         (body (if doc (rest body) body)))
    `(setf (get ',name :debug)
           ,(when (first body)
              `(lambda (perimeter environment)
                 ,(first body)))
           (get ',name :debug-view)
           ,(when (second body)
              (second body)))))

(defun get-debug-function (name)
  (get name :debug))

(defun get-debug-view-function (name)
  (get name :debug-view))

; evaluation: (result fun-name/lambda-expression &rest evaluations)

; A little vocabulary here:
; - a binding is only about variables, not local functions, and is written in the form (x 1)
; - "a" bindings is a list of such individual binding, nothing incredible here
; - a scope is of 3 different forms: (let <fully-computed-bindings>), (flet <local-functions>) and (labels <local-functions>)
; Notes: There are no scope for let* per se, it ends up being flattened as a let scope after all values have been properly computed. Note this most important note: everything has been computed in let scopes. Examples:
;   - this is a let scope: (let ((x 1) (y 2)))
;   - this is NOT a let scope: (let ((x 1) (y (+ 1 1))))
; - Finally, an environment is just a list of such scopes

(defun implicit-progn-body (perimeter)
  "=> perimeter
Make the implicit progn of the body explicit by enlist the progn action if needed."
  (let-a (get-action-body perimeter)
    (if (= 1 (length it))
      (first it)
      (cons 'progn it))))

(defun maybe-quote (perimeter)
  "=> perimeter
Return the given sexpr wrapped in a quote except if it is self-evaluating."
  (if (cl-self-evaluating? perimeter)
    perimeter
    (list 'quote perimeter)))

(defun make-query-binding-code (let-scope &optional already-queried-symbols)
  "Generate a code to recompose the bindings of the given let scope, in the same order, and with avoiding to query for the value of bound variables that have already been queried because of the presence of shadowing bindings.
Return the query-binding-code.
Secondary value: return all the queried symbols, the given already-queried-symbols included."
  (let ((queried-symbols already-queried-symbols))
    (let-a (loop for symbol in (mapcar #'binding-name (second let-scope))
                 for i from 0
                 if (member symbol queried-symbols)
                 collect `',(elt (second let-scope) i)
                 else
                 collect `(list ',symbol (maybe-quote ,symbol))
                 and
                 do (push symbol queried-symbols))
      (values `(list 'let (list ,@it))
              queried-symbols))))

(defun make-recompose-environment-code (environment)
  "Generate a code to recompose the given environment, with refreshed values for all its let scopes."
  (let (queried-symbols)
    (awhen (loop for scope in environment
                 if (eq 'let (first scope))
                 collect (multiple-value-bind
                             (query-binding-code updated-queried-symbols)
                             (make-query-binding-code scope queried-symbols)
                           (setf queried-symbols updated-queried-symbols)
                           query-binding-code)
                 else
                 collect `',scope)
      `(list ,@it))))

; result form
(defun make-result-form (result error &key info)
  "Make a result form, which has an :error tag inserted after the result if error is not nil.
The info, if any and assumed to be a list, is appended after."
  (if error
    `(,result :error ,@info)
    `(,result ,@info)))

(defun values-result (result)
  (case (length result)
    (0 nil)
    (1 (first result))
    (t `(values-list ',result))))

(defun result-form-value (result-form)
  (maybe-quote (let-a (first result-form)
                 (if (and (listp it)
                          (eq 'values-list (first it)))
                   (first (second (second it)))
                   it))))

(defun replace-function-value (result)
  "First, function values cannot be printed readably. And the unreadable output is verbose and ugly compared to just giving the function name.
But for anonymous functions, a problem occurred for me on LispWorks when trying to debug a macro to which I pass some anonymous functions that end up being lexical closures. Because evaluation is programmatic with debugging, the forms giving these lexical closures are stored verbatim with the #. read-macro as a prefix. All this source code stored verbatim has the following problem: the form defining the lexical closure has the code source of my anonymous function -of course- plus all the environment. Just mandatory if one wish to really have a closure. But the environment is the macro-expanded code and this and code hold itself the lexical closure. Which has the source code of the anonymous function and, again, the environment. And so on. This causes the stack to overflow when pretty printing or using my-systems:print-code-chunk.
While setting *print-circle* does work, it produces a very verbose and even uglier code.
Hence this function."
  (if (and (listp result)
           (eq 'values-list (first result)))
    `(values-list (quote ,(mapcar #'replace-function-value
                                  (second (second result)))))
    (if (functionp result)
      (multiple-value-bind
          (lambda-expression closure-p name)
          (function-lambda-expression result)
        (declare (ignore lambda-expression closure-p))
        (if (and name (atom name))
          `(:function ,name)
          :anonymous-function))
      result)))

(defun replace-function-value-in-result-form (result-form)
  "Utility to perform the replacement of function values in a given result-form."
  `(,(replace-function-value (first result-form)) ,@(rest result-form)))

; "Pipes" in order to more easily write code without dealing with the secondary values
; What's more, "dealing with them" is a bit of stretch as there are only 3 different situations to handle and the most common one directly forward them
(defmacro forward-debug (call-form result-form)
  "Use the presumed debug call form and forward directly the resulting environment and the errors.
The basic pipe of debugging."
  `(multiple-value-bind
       (result environment errors)
       ,call-form
     (values ,result-form environment errors)))

(defmacro scoped-forward-debug (call-form)
  "Use the presumed debug call form and forward directly the resulting environment and the errors.
The basic pipe of debugging."
  `(multiple-value-bind
       (result environment errors)
       ,call-form
     (values result (remove-scope environment) errors)))

(defmacro guarded-forward-debug (call-form guarded-call-form result-form)
  "The next complexity level of pipes for debugging.
Some perimeters require the debugging of a first form which acts as a guard of another form and may change the environment fro that form. Any error happening during the debugging of the first form causes the result, environment and error messages to be directly forwarded without debugging the second form.
Needed for debugging a function call (first debug all the arguments then the function call itself), the if action (first the test then what is required) or the setq action (first the values then the assignements themselves)."
  `(let (guarded-result guarded-errors)
     (declare (ignorable guarded-result guarded-errors))
     (multiple-value-bind
         (result environment errors)
         ,call-form
       (if errors
         (values ,result-form environment errors)
         (multiple-value-bind
             (guarded-result environment guarded-errors)
             ,guarded-call-form
           (values ,result-form environment guarded-errors))))))

(defmacro scoped-and-guarded-forward-debug (call-form guarded-call-form binding-names
                                                      &key merge-result-in-bindings remove-scope-if-errors)
  "The ultimate complexity level of pipe for debugging.
The first form is not just able to modify the environment, it is its raison d'être because it will provide new bindings for the next form.
This macro still forwards the result, it still guards the debugging of the second form in case some errors occurred during the debugging of the values of the bindings but it also provides another variable, bindings, for use in the second form, and ensure the final resulting environmment will be trimmed from such bindings as the established scope is exited.
Needed for debugging a lambda form, a let and a let* form.
Note: this macro assume the resource named perimeter is available and will take the first element of it to name the scope. Contrary to the two other forwarding macro, it generates itself the code for the result to enforce the format representing scope."
  (flet ((info-result-form (&optional third-info)
           (let ((bindings (if merge-result-in-bindings
                             `(bindings<-lambda-list ,binding-names
                                                     (mapcar #'replace-function-value-in-result-form result))
                             `(mapcar (lambda (binding)
                                        `(,(first binding) ,(replace-function-value (second binding))))
                                      bindings))))
             `(list* (list (first perimeter) ,bindings ,third-info)
                     ,(unless merge-result-in-bindings '(mapcar #'replace-function-value-in-result-form result))))))
    `(multiple-value-bind
         (result environment errors)
         ,call-form
       (let ((bindings (bindings<-lambda-list ,binding-names (mapcar #'result-form-value result))))
         (declare (ignorable bindings))
         (if errors
           (values (make-result-form
                    nil nil
                    :info ,(info-result-form))
                   ,(if remove-scope-if-errors '(remove-scope environment) 'environment)
                   errors)
           (multiple-value-bind
               (guarded-result environment errors)
               ,guarded-call-form
             (values (make-result-form
                      (first guarded-result) nil
                      :info ,(info-result-form '(replace-function-value-in-result-form guarded-result)))
                     (remove-scope environment) errors)))))))

(defun make-lexical-values-eval-code (perimeter environment)
  (embed-in-environment
   (with-gensyms (result c)
     `(handler-case (multiple-value-list ,perimeter)
        (condition (,c)
          (values nil
                  ,(make-recompose-environment-code environment)
                  (list (format nil "~a" ,c))))
        (:no-error (,result)
          (values ,result
                  ,(make-recompose-environment-code environment)))))
   environment))

(defun lexical-values-eval (perimeter environment)
  "The one and only action doing the real evaluation of a perimeter.
Responsible to insert the given bindings, catching any errors and re-building the environment after the perimeter has been evaluated."
  (forward-debug
   (eval (make-lexical-values-eval-code perimeter environment))
   (values-result result)))

(defun debug-chain (perimeters environment)
  "Debug all the given perimeters in the given order.
Contrary to classic debugging methods, this one does not stop as soon as an error is thrown: it will rather collect all of them, for each perimeter. It will also follow and forward any change to the environment from one perimeter to the next.
Return the list of all debugged information.
Secondary values: the impacted environment and a list of all errors that have occurred."
  (let ((updated-environment environment)
        all-errors)
    (let-a (loop for item in perimeters
                 collect (multiple-value-bind
                             (result new-environment errors)
                             (debug-perimeter item updated-environment)
                           (setf updated-environment new-environment
                                 all-errors (append all-errors errors))
                           result))
      (values it updated-environment all-errors))))

(defun debug-setq-chain (perimeters binding-names environment)
  "Designed solely to debug the special operator setq and its serial assignments.
In fact, it is just like debug-chain with the only exception that after each given perimeter has been debugged, the result gets assigned to the variable in the binding names listed at the same position than the perimeter in perimeters.
Return the list of all debugged information.
Secondary values: the impacted environment and a list of all errors that have occurred.
Note: while in the context of a classic evaluation a setq will stop as soon as an error is encountered, this function will debug all the perimeters and perform all the assignements to provide the maximum amount of information. Up to the reader to assess if an error at some point does affect the next computations or not. As for now, up to the reader too to assess if the information presented is relevant, like in (setq a 1 b (incf a)) which will say: a <- 1 and b <- 2!
A comparison between the debugged information and the real environment should be performed but as I rarely, if ever, program like that and still rely on a binding that has been modified several times like that, I won't try to do that comparison."
  (let ((updated-environment environment)
        all-errors)
    (let-a (loop for binding-name in binding-names
                 for perimeter in perimeters
                 collect (multiple-value-bind
                             (result environment errors)
                             (debug-perimeter perimeter updated-environment)
                           (setf updated-environment (if (symbolp binding-name)
                                                       (modify-binding binding-name (result-form-value result)
                                                                       environment)
                                                       environment)
                                 all-errors (append all-errors errors))
                           (list binding-name result)))
      (values it updated-environment all-errors))))

(defun debug-scoped-chain (perimeters binding-names environment)
  "Designed solely to debug the special operators let* where each binding is available for the next ones, which include the potential modification of these made-available bindings right when making the other ones...
Return the value of the perimeters, each one being bound to the binding-name in the given binding names at the same position than the perimeter in perimeters.
As such, this list of values CANNOT be used directly because of potential side-effect on some bindings can occur while computing the succeeding ones, this list is therefore not a faithful representation of what the current values of the bindings are!
The true values are held in the returned environment.
Use disentangle-scoped-chain to get the real values of all the bindings and the environment trimmed of all of them."
  (let ((updated-environment (add-scope (make-let-scope nil) environment))
        all-errors)
    (let-a (loop for item in perimeters
                 for binding-name in binding-names
                 collect (multiple-value-bind
                             (result new-environment errors)
                             (debug-perimeter item updated-environment)
                           (setf updated-environment (add-binding (make-binding binding-name
                                                                                (result-form-value result))
                                                                  new-environment)
                                 all-errors (append all-errors errors))
                           result))
      (values it updated-environment all-errors))))

(defun no-debug (perimeter environment)
  "Evaluate the given perimeter wrapped in the given bindings, without further analysis of what is in the perimeter."
  (forward-debug (lexical-values-eval perimeter environment)
                 (make-result-form result errors :info `(nil (,perimeter)))))

(defun debug-function-call (perimeter environment)
  "First debug as a chain all the arguments, and if no errors happened, debug the function call itself.
Note that we don't insert all the values themselves. When appropriate, we rather insert the variable name. That way, having clone the environment, we can show the effect of destructive functions on it while not transforming the result themselves. Without such a trick, we would have, for instance, (nreverse test) with test bound to (1 2 3) returning the appropriate environment but also these debugging information: ((3 2 1) nreverse ((1) test))!
As a final note, I am pretty sure this implementation is absolutely not bullet proof against all uses of destructive functions. That comes in mind is the situation when several list values share some parts of their structure and are bound to various variables. But since I am using destructive functions only when I am sure I will not use anymore the data it has worked on and never rely on their side-effects for anything, I don't care about an in-depth simulation of their effects. And that is why no before/after comparison is made on the environment to be provided to the functions building the layout of the debugging information. If a destroyed value is being used, it will be printed out at that precise moment. Since I am never using a destroyed value, I don't see the point of cluttering the debugging output with such concerns."
  (guarded-forward-debug
   (debug-chain (rest perimeter) environment)
   (lexical-values-eval `(,(first perimeter)
                          ,@(mapcar (lambda (result-form)
                                      (if (and (= 2 (length result-form))
                                               (not (functionp (first result-form)))
                                               (symbolp (second result-form)))
                                        (second result-form)
                                        (result-form-value result-form)))
                                    result))
                        (copy-tree environment))
   (make-result-form guarded-result guarded-errors
                     :info `(,(first perimeter) ,@(aif result
                                                       (mapcar #'replace-function-value-in-result-form it)
                                                       (list nil))))))

(defun debug-with-source-code (perimeter environment &optional source-code)
  "Assume the given perimeter to be a function call with, as its first element, either the name of the function or a lambda form.
If it is the name of a function then the source-code argument is definitively not optional for proper debugging but if missing, this function fallbacks on classic debugging.
Otherwise, the first element is assumed to be a lambda form and the given source code is ignored."
  (let ((source-code (if (symbolp (first perimeter))
                       (awhen source-code it)
                       (first perimeter))))
    (if source-code
      (scoped-and-guarded-forward-debug 
       (debug-chain (rest perimeter) environment)
       (debug-perimeter (implicit-progn-body source-code)
                        (add-scope (make-let-scope bindings) environment))
       (get-action-lambda-list source-code))
      (debug-perimeter perimeter environment))))

(defun debug-perimeter (perimeter environment)
  "Debug the given perimeter with the given bindings.
This function cover a good part of Common Lisp: functions, macros (on an individual basis though), the use of lambda form in place of a function name and some special operators.
For that latter category, here's the handled ones: quote, function, progn, if, setq, let, let*, flet and labels.
The other one are debug with no-debug, that is a direct evaluation of the perimeter without a peak at its inside. Here are some notes about them:
- I think my needs will bring me to handle multiple-value-call, multiple-value-prog1 and unwind-protect.
- I have, maybe unreasonably, projected to stay away from specifically handle macrolet, symbol-macrolet, eval-when, load-time-value and progv. This list may seem strange at first but the common point between all these operators is they are not working on an easy-to-picture continuum. The first four are about what I consider to be install time matters and I think handling them should come with its own tools. More precisely, the code is a material to be worked upon and these tools should be external to the code. In other words, since I am planning to do just that, I will shy away from things that will make it harder. So I think the use of macrolet should be avoided and replaced by defmacro if possible, so as to not have the tool working on code inside the very code! I think one can avoid symbol-macrolet altogether while not being that restricted. And the use of eval-when can be circumvented by organizing the code in the files in a more serial manner. As for load-time-value and progv... I really don't know what to do with them.
- All pairs of operators designed to handle the local flow of control, that is: (block return-from), (tagbody go) and (catch throw), are all about a way of programming different than chainable action perimeters. They probably require their own debugging perspective.
- Lastly, I don't plan to specifically handle the operators communicating to the compiler which are: locally and the.
Final note: macros are handled in a very cavalier way. They are just evaluated as is, without even expanding them by falling back on no-debug. Even if it works to debug their expansion, the output is rather convoluted, even for basic macros. Therefore it is not the behavior by default. But this function will first look up if a debug function is associated to the used macro and delegate the debugging to it if there is one. "
  (if (atom perimeter)
    (if (and (symbolp perimeter)
             (not (cl-self-evaluating? perimeter)))
      (forward-debug (lexical-values-eval perimeter environment)
                     (make-result-form result errors :info (list perimeter)))
      (values (list perimeter) environment nil))
    (if (atom (first perimeter))
      (cond
       ((cl-function? (first perimeter))
        (aif (get-debug-function (first perimeter))
             (forward-debug (funcall it perimeter environment)
                            result)
             (debug-function-call perimeter environment)))
       ((cl-macro? (first perimeter))
        (aif (get-debug-function (first perimeter))
             (forward-debug (funcall it perimeter environment)
                            (make-result-form (first result) nil :info `(:macro ,(first perimeter) ,@(rest result))))
             (no-debug perimeter environment)))
       ((cl-special-action? (first perimeter))
        (case (first perimeter)
          ((multiple-value-call multiple-value-prog1 unwind-protect
             macrolet symbol-macrolet eval-when load-time-value progv
             block return-from tagbody go catch throw
             locally the)
           (no-debug perimeter environment))
          ((quote)
           (values (rest perimeter) environment nil))
          ((function)
           (forward-debug (lexical-values-eval perimeter environment)
                          (make-result-form result errors :info (when errors `(,(first perimeter)
                                                                               (,(second perimeter)))))))
          ((progn)
           (forward-debug
            (debug-chain (rest perimeter) environment)
            (make-result-form (unless errors (first (first (last result)))) nil
                              :info `(,(first perimeter)
                                      ,@(aif result
                                             (mapcar #'replace-function-value-in-result-form it)
                                             (list nil))))))
          ((if)
           (guarded-forward-debug
            (debug-perimeter (second perimeter) environment)
            (debug-perimeter (if (first result) (third perimeter) (fourth perimeter))
                             environment)
            (list (first guarded-result) 'if
                  (replace-function-value-in-result-form result)
                  (replace-function-value-in-result-form guarded-result))))
          ((setq)
           (multiple-value-bind
               (binding-names perimeters setq-errors)
               (loop for (binding-name perimeter) in (group (rest perimeter) 2)
                     collect binding-name into binding-names
                     collect perimeter into perimeters
                     when (not (symbolp binding-name))
                     collect (format nil "Cannot setq ~a -- not a symbol." binding-name) into errors
                     finally (return (values binding-names perimeters errors)))
             (multiple-value-bind
                 (result environment errors)
                 (debug-setq-chain perimeters binding-names environment)
               (let ((errors (append setq-errors errors)))
                 (values (make-result-form (unless errors (result-form-value (second (first (last result)))))
                                           errors
                                           :info `((setq
                                                    ,(mapcar (lambda (setq-binding)
                                                               `(,(first setq-binding)
                                                                 ,(replace-function-value-in-result-form
                                                                   (second setq-binding))))
                                                             result))))
                         environment
                         errors)))))
          ((let)
           (scoped-and-guarded-forward-debug
            (debug-chain (mapcar #'binding-value (second perimeter)) environment)
            (debug-perimeter (implicit-progn-body perimeter)
                             (add-scope (make-let-scope bindings) environment))
            (mapcar #'binding-name (second perimeter))
            :merge-result-in-bindings t))
          ((let*)
           (scoped-and-guarded-forward-debug
            (debug-scoped-chain (mapcar #'binding-value (second perimeter))
                                (mapcar #'binding-name (second perimeter))
                                environment)
            (debug-perimeter (implicit-progn-body perimeter) environment)
            (mapcar #'binding-name (second perimeter))
            :merge-result-in-bindings t
            :remove-scope-if-errors t))
          ((flet)
           (scoped-forward-debug
            (debug-perimeter (implicit-progn-body perimeter)
                             (add-scope (make-flet-scope (second perimeter))
                                        environment))))
          ((labels)
           (scoped-forward-debug
            (debug-perimeter (implicit-progn-body perimeter)
                             (add-scope (make-labels-scope (second perimeter))
                                        environment))))
          ))
       (t
        (debug-function-call perimeter environment)))
      (if (eq 'lambda (first (first perimeter)))
        (debug-with-source-code perimeter environment)
        (error "Find the non-lambda expression ~a as action in ~a"
               (first perimeter)
               perimeter)))))

(defun full-view-debug (perimeter &optional environment)
  "=> fully-debugged-result
Debug."
  (let ((source-code (when (and (not (atom perimeter))
                                (symbolp (first perimeter))
                                (cl-function? (first perimeter)))
                       (load-action-definition (first perimeter)))))
    (if source-code
      (forward-debug (debug-with-source-code perimeter environment source-code)
                     result)
      (forward-debug (debug-perimeter perimeter environment)
                     (if environment
                       `(,(first result)
                         (:top-level
                          ,(mapcar (lambda (scope)
                                     `(,(first scope) (,(second scope))))
                                   environment)
                          ,result))
                       result)))))

(defun ignore-top-level (fully-debugged-result)
  (if (eq :top-level (first (second fully-debugged-result)))
    (third (second fully-debugged-result))
    fully-debugged-result))