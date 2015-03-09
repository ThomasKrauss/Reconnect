(in-package :my-utilities)

(defmacro let-a (value-form &body body)
  `(let ((it ,value-form)) ,@body))

(defmacro aif (test then-clause &optional else-clause)
  "Maybe overkill for some but it so leads to clearer code in
the specific situation that benefits from it!"
  `(let ((it ,test))
     (if it ,then-clause ,else-clause)))

(defmacro awhen (test &body body)
  "Same overkill as aif. But can be so clear!"
  `(let ((it ,test))
               (when it ,@body)))

(defmacro alet (letargs &body body)
  "Classic anaphoric macro alet.
Assume the last instruction in body build a function.
Capture this functional result and provide an indirection to it
with injection of the variable this.
Return a closure on this that just invokes it."
  `(let ((this) ,@letargs)
     (setq this ,@(last body))
     ,@(butlast body)
     (lambda (&rest params) (apply this params))))

(defmacro alet-fsm (&body states)
  "Special syntax to define a finite state machine.
Assume a variable this to hold the function to use on a given state.
Provide a syntax (state name-of-state) to bind a new function to the this variable.
Wait a list of functions defined as in labels.
By default, this is initially bound to the first function of this list."
  `(macrolet ((state (s)
                `(setq this #',s)))
     (labels (,@states)
       #',(caar states))))

(defmacro acond (&rest clauses)
  "Instead of nesting aif..."
  (if (null clauses)
      nil
    (let ((cl1 (car clauses)) (sym (gensym)))
      `(let ((,sym ,(car cl1)))
         (if ,sym
             (let ((it ,sym))
               (declare (ignorable it))
               ,@(cdr cl1))
           (acond ,@(cdr clauses)))))))

