(in-package :to-web)

(defvar *prologue* "<!DOCTYPE html>"
  "The doctype for HTML5.")

(defparameter *empty-elements*
  '(:area :base :br :col :hr :img :input :link :meta :param))

(defun empty-element-p (tag)
  (find tag *empty-elements*))

; String emission

(defvar *html-output* *standard-output*)
(defvar *css-output* nil)
(defvar *js-output* nil)

(defun raw-string (string)
  (write-string string *html-output*))

(defun embed-raw-string (string)
  (push-op `(:raw-string ,string)))

(defvar *html-string-emitter* #'raw-string)

(defvar *ops* nil)

; Test html-sexp
(defparameter *html-special-operators* '(:print :format :progn :noescape :attribute))

(defun self-evaluating-p (form)
  (and (atom form)
       (if (symbolp form)
         (keywordp form)
         t)))

(defun cons-form-p (form &optional (test #'keywordp))
  (and (consp form)
       (funcall test (car form))))

(defun html-sexp-p (form)
  (or (self-evaluating-p form)
      (cons-form-p form)))

(defun html-special-form-p (form)
  (and (consp form)
       (symbolp (car form))
       (member (car form) *html-special-operators*)))

(defun html-macro-form-p (form)
  (cons-form-p form
               (lambda (x)
                 (and (symbolp x)
                      (get x 'html-macro)))))

; Escaping

(defun escape-char (char)
  (case char
    (#\& "&amp;")
    (#\< "&lt;")
    (#\> "&gt;")
    (#\' "&apos;")
    (#\" "&quot;")
    (t
     (format nil "&#~d;" (char-code char)))))

(defun escape (in to-escape)
  (flet ((needs-escape-p (char)
           (find char to-escape)))
    (with-output-to-string (out)
      (loop for start = 0 then (1+ pos)
            for pos = (position-if #'needs-escape-p in :start start)
            do (write-sequence in out :start start :end pos)
            when pos do (write-sequence (escape-char (char in pos)) out)
            while pos))))

(defparameter *element-escapes* "<>&")
(defparameter *attribute-escapes* "<>&\"'")

(defvar *escapes* *element-escapes*)

  
; Parse tag, attributes and body

(defun parse-cons-form (sexp)
  (loop with tag = (first sexp)
        for rest on (rest sexp) by #'cddr
        while (and (keywordp (first rest))
                   (second rest))
        when (second rest)
        collect (first rest) into attributes and
        collect (second rest) into attributes
        end
        finally (return (values tag attributes rest))))

; Inner process

(defun emit-attributes (attributes)
  (loop for (k v) on attributes by #'cddr do
        (funcall *html-string-emitter* (format nil " ~(~a~)='" k))
        (let ((*escapes* *attribute-escapes*))
          (process (cond
                    ((eql v t)
                     (string-downcase k))
                    ((and (listp v)
                          (not (keywordp (first v))))
                     `(:print ,v))
                    (t
                     v))))
        (funcall *html-string-emitter* "'")))

(defun emit-open-tag (tag body-p attributes)
  (funcall *html-string-emitter* (format nil "<~(~a~)" tag))
  (emit-attributes attributes)
  (funcall *html-string-emitter*
           (if (and (empty-element-p tag)
                    (not body-p))
             " />"
             ">")))

(defun emit-element-body (body)
  (dolist (item body)
    (process item)))

(defun emit-close-tag (tag body-p)
  (unless (and (empty-element-p tag)
               (not body-p))
    (funcall *html-string-emitter* (format nil "</~(~a~)>" tag))))

(defun process-cons-html-sexp (form)
  (when (string= *escapes* *attribute-escapes*)
    (error "Can't use cons forms in attributes: ~a" form))
  (multiple-value-bind
      (tag attributes body)
      (parse-cons-form form)
    (emit-open-tag tag body attributes)
    (emit-element-body body)
    (emit-close-tag tag body)))

(defun process-html-sexp (form)
  (if (self-evaluating-p form)
    (if (keywordp form)
      (case form
        (:br (funcall *html-string-emitter* "<br />")))
      (funcall *html-string-emitter* (escape (princ-to-string form) *escapes*)))
    (process-cons-html-sexp form)))

(defun process-special-form (form)
  (case (first form)
    (:noescape
     (let ((*escapes* nil))
       (loop for form in (rest form)
             do (process form))))
    (:attribute
     (let ((*escapes* *attribute-escapes*))
       (loop for form in (rest form)
             do (process form))))
    (:print
     (let ((form (second form)))
       (cond
        ((self-evaluating-p form)
         (warn "Redundant :print of self-evaluating form ~s" form)
         (process-html-sexp form))
        (t
         (embed-value form)))))
    (:format
     (if (every #'self-evaluating-p (rest form))
       (process-html-sexp (apply #'format nil (rest form)))
       (embed-value `(format nil ,@(rest form)))))
    (:progn
      (loop for form in (rest form)
            do (process form)))))

(defun process (form)
  (cond
   ((html-special-form-p form)
    (process-special-form form))
   ((html-macro-form-p form)
    (process (expand-macro-form form)))
   ((html-sexp-p form)
    (process-html-sexp form))
   ((consp form)
    (if *ops*
      (embed-code form)
      (error "Cannot embed code ~a when directly emitting HTML." form)))
   (t
    (if *ops*
      (embed-value form)
      (error "Cannot embed value ~a when directly emitting HTML." form)))))

; Interpreter
(defun emit-html (sexp)
  (let ((*html-string-emitter* #'raw-string)
        (*ops* nil))
    (process sexp)))


; Compiler
(defun make-op-buffer ()
  (make-array 10 :adjustable t :fill-pointer 0))

(defun push-op (op &optional (ops *ops*))
  (vector-push-extend op ops))

(defun embed-value (value)
  (push-op `(:embed-value ,value ,*escapes*)))

(defun embed-code (code)
  (push-op `(:embed-code ,code)))

(defun ops<-html-sexp (body)
  (let ((*ops* (make-op-buffer))
        (*html-string-emitter* #'embed-raw-string))
    (loop for form in body
          do (process form))
    *ops*))

(defun optimize-static-output (ops)
  (let ((new-ops (make-op-buffer)))
    (with-output-to-string (buf)
      (flet ((add-op (op) 
               (compile-buffer buf new-ops)
               (push-op op new-ops)))
        (loop for op across ops
              do (ecase (first op)
                   (:raw-string
                    (write-sequence (second op) buf))
                   ((:newline :embed-value :embed-code)
                    (add-op op))))
        (compile-buffer buf new-ops)))
    new-ops))

(defun compile-buffer (buf ops)
  "Compile a string possibly containing newlines into a sequence of
:raw-string and :newline ops."
  (loop with str = (get-output-stream-string buf)
        for start = 0 then (1+ pos)
        for pos = (position #\Newline str :start start)
        when (< start (length str))
        do (push-op `(:raw-string ,(subseq str start pos)) ops)
        when pos do (push-op '(:newline) ops)
        while pos))

(defun code<-op (op &rest operands)
  (ecase op
    (:raw-string
     (destructuring-bind (string) operands
       `(write-sequence ,string *html-output*)))
    (:newline
     `(write-char #\Newline *html-output*))
    (:embed-value
     (destructuring-bind (value escapes) operands
       (if escapes
         `(write-sequence (escape (princ-to-string ,value) ,escapes) *html-output*)
         `(princ ,value *html-output*))))
    (:embed-code
     (first operands))))

(defmacro html% (&body body)
  `(progn
     ,@(loop for op across (optimize-static-output
                            (ops<-html-sexp body))
             collect (apply #'code<-op op))
     nil))

(defmacro html (&whole whole &body body)
  (declare (ignore body))
  `(macrolet ((html (&body body)
                `(progn
                   ,@(loop for op across (optimize-static-output
                                          (ops<-html-sexp body))
                           collect (apply #'code<-op op))
                   nil)))
     ,whole))

; Macros
(defun parse-html-macro-lambda-list (args)
  (let ((attr-cons (member '&attributes args)))
    (values
     (cadr attr-cons)
     (nconc (ldiff args attr-cons)
            (cddr attr-cons)))))

(defmacro define-html-macro (name (&rest args) &body body)
  (multiple-value-bind
      (attribute-var args)
      (parse-html-macro-lambda-list args)
    (aif attribute-var
         (generate-macro-with-attributes name it args body)
         (generate-macro-no-attributes name args body))))

(defun generate-macro-with-attributes (name attribute-args args body)
  (with-gensyms (attributes form-body)
    (when (symbolp attribute-args)
      (setf attribute-args `(&rest ,attribute-args)))
    `(setf (get ',name 'html-macro-wants-attributes) t
           (get ',name 'html-macro)
           (lambda (,attributes ,form-body)
             (destructuring-bind (,@attribute-args) ,attributes
               (destructuring-bind (,@args) ,form-body
                 ,@body))))))

(defun generate-macro-no-attributes (name args body)
  (with-gensyms (form-body)
    `(setf (get ',name 'html-macro-wants-attributes) nil
           (get ',name 'html-macro)
           (lambda (,form-body)
             (destructuring-bind (,@args) ,form-body
               ,@body)))))

(defun expand-macro-form (form)
  (if (or (consp (first form))
          (get (first form) 'html-macro-wants-attributes))
    (multiple-value-bind
        (tag attributes body)
        (parse-cons-form form)
      (funcall (get tag 'html-macro) attributes body))
    (destructuring-bind
        (tag &body body)
        form
      (funcall (get tag 'html-macro) body))))

; API
(defmacro with-html-output ((stream &key prologue) &body body)
  "Emit html to the given stream.
Return as values the CSS and JS files included in the html stream."
  `(let ((*html-output* ,stream)
         *css-output*
         *js-output*)
     ,@(append
        (when prologue `((write-string ,(if (stringp prologue) prologue *prologue*) ,stream)))
        body)
     (values *css-output*
             *js-output*)))

(defmacro with-html-output-to-string ((&key prologue) &body body)
  (with-gensyms (out html-output css-output js-output css js)
    `(let* (,css-output
            ,js-output
            (,html-output (with-output-to-string (,out)
                            (multiple-value-bind
                                (,css ,js)
                                (with-html-output (,out :prologue ,prologue)
                                  ,@body)
                              (setf ,css-output ,css
                                    ,js-output ,js)))))
       (values ,html-output
               ,css-output
               ,js-output))))
       
(defmacro with-html-output-to-file ((file &key (prologue t)) &body body)
  (with-gensyms (stream string-stream css-output js-output css js)
    (once-only (file)
      `(let (,css-output ,js-output)
         (ensure-directories-exist ,file)
         (with-open-file (,stream ,file
                                  :direction :output
                                  :if-exists :supersede
                                  :element-type 'unsigned-byte)
           (trivial-utf-8:write-utf-8-bytes
            (with-output-to-string (,string-stream)
              (multiple-value-bind
                  (,css ,js)
                  (with-html-output (,string-stream :prologue ,prologue)
                    ,@body)
                (setf ,css-output ,css
                      ,js-output ,js)))
            ,stream))
         (values ,file
                 ,css-output
                 ,js-output)))))

; Test

(defmacro test-html (code result)
  `(equal (with-html-output-to-string ()
            ,code)
          ,result))