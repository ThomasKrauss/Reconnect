(in-package :my-utilities)

(defun cl-action? (symbol)
  "Test if a symbol is bound to an action (i.e a function, a special operator or a macro)"
  (when (fboundp symbol)
	symbol))

(defun cl-macro? (symbol)
  (when (macro-function symbol)
    symbol))

(defun cl-function? (symbol)
  (when (and (cl-action? symbol)
             (not (cl-macro? symbol))
             (not (special-operator-p symbol)))
    symbol))

(defun cl-special-action? (symbol)
  "Test if a symbol is bound to a special action, i.e. a macro or a special operator.
With some exceptions. Like declare."
  (when (and (cl-action? symbol)
             (not (null (or (macro-function symbol)
                            (special-operator-p symbol)))))
    symbol))

(defun cl-self-evaluating? (sexpr)
  "Return true if the sexpr is self-evaluating."
  (or (null sexpr)
      (eq t sexpr)
      (keywordp sexpr)
      (and (atom sexpr)
           (not (symbolp sexpr)))))
		   
(defun get-action-lambda-list (sexpr)
  (cond
    ((member (first sexpr) '(defun defmacro))
     (third sexpr))
    ((equal (first sexpr) 'lambda)
     (second sexpr))
    ((symbolp (first sexpr))
     (second sexpr))
    (t
     (error "Can't retrieve the lambda list from the given perimeter ~a" sexpr))))

(defun get-action-body (sexpr)
  "Get the body of the given sexpr, specially handling it for definition actions like, as example, ignoring the documentation string and declarations, if any or doing an even more fine grain parsing.
If the action of the given sexpr is not specially handled, get-action-body returns the action of the sexpr, as a keyword and the sepxr itself, wrapped in a list, meaning that it is the whole body itself.
- The following CL definition actions are taken into account:
defun, defmacro, defvar, defparameter, defconstant, define-condition, defsetf, define-setf-expander.
- The others are not at all handled and may cause problems, including all CLOS actions such as defclass, defgeneric and defmethod.
- The following custom definition actions are specially handled:
define-hierarchical-cache
- The following actions are ignored:
in-package"
  (flet ((fetch-body (start &optional (form sexpr))
           (when (and (< (1+ start) (length form))
                      (stringp (elt form start)))
             (incf start))
           (loop for expr in (subseq form start)
                 while (and (not (atom expr))
                            (equal 'declare (first expr)))
                 do (incf start))
           (subseq form start)))
    (when (listp sexpr)
      (case (first sexpr)
        ((defun defmacro define-setf-expander)
         (values (fetch-body 3)
                 (second sexpr)))
        ((lambda let let* flet labels)
         (values (fetch-body 2)
                 (first sexpr)))
        ((defvar defconstant defparameter)
         nil)
        #|(values (when (< 2 (length sexpr)) (list (third sexpr)))
                 (second sexpr)))|#
        ((defsetf)
         (if (let ((size (length sexpr)))
               (or (= 3 size) (= 4 size)))
           (values (list (append (list 'function) (third sexpr)))
                   (second sexpr))
           (values (fetch-body 4)
                   (second sexpr))))
        ((define-condition)
         (values (let (result)
                   (dolist (slot-spec (fourth sexpr))
                     (let ((item (getf (rest slot-spec) :init-form)))
                       (when item
                         (push item result))))
                   (dolist (option (subseq sexpr 5))
                     (when (eq :report (first option))
                       (push (second option) result)))
                   result)
                 (second sexpr)))
        ((in-package)
         nil)
        (t
         (let ((name (string-downcase (symbol-name (first sexpr)))))
           (cond
            ((string= name "def-chainable-setf")
             (values (fetch-body 3 (macroexpand-1 sexpr))
                     (second sexpr)))
            ((string= name "def-access")
             (values (let ((expansion (macroexpand-1 sexpr)))
                       (append (fetch-body 3 (second expansion))
                               (fetch-body 3 (macroexpand-1 (third expansion)))))
                     (second sexpr)))
            ((string= name "defun-to-external-program")
             (values (fetch-body 4)
                     (second sexpr)))
            ((string= name "define-hierarchical-cache")
             (values (list sexpr)
                     (list (second sexpr) (list :end))))
            ((string= name "define-web-result")
             (values (append (list sexpr)
                             (mapcar (lambda (x)
                                       (list (first x)))
                                     (plist-values (rest (rest sexpr)))))
                     (list (second (second sexpr)) (list :end))))
            ((string= name "define-web-editor")
             (values (mapcar (lambda (spec)
                               (list (if (atom spec)
                                       spec
                                       (first spec))))
                             (nthcdr 2 sexpr))
                     (list (second (second sexpr)) (list :end))))
            (t
             (values (list sexpr)
                     (first sexpr))))))))))