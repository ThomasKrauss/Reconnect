(in-package :my-utilities)

(defun json-escape (str)
  "JSON requires that no newlines, carriage-returns, linefeed or tabulations are in the string.
Additionnaly, double-quotes and backslashes must of course be escaped too
since printing them verbatim in a JSON stream will invalidate it all.
Examples:
- \"test\" must not be printed as is like => \"\"test\"\"
This is <empty string><symbol test><empty string>!
- blabla\\ must not be printed as is like => \"blablabla\\\"
The ending double-quote gets escaped!"
  (with-output-to-string (out)
    (flet ((escaped-write (char)
             (write-char #\\ out)
             (write-char char out)))
      (with-input-from-string (s str)
        (loop for char = (read-char s nil nil)
              while char
              do (case char
                   ((#\Newline #\Linefeed) (escaped-write #\n))
                   (#\Tab (escaped-write #\t))
                   (#\Return (escaped-write #\r))
                   (#\" (escaped-write #\"))
                   (#\\ (escaped-write #\\))
                   (t (write-char char out))))))))

(defun nil-is-json-null (value)
  (if (or (eq :false value)
          (null value))
    :null
    value))

(defun lisp<-json (json)
  (labels ((rec (result)
             (cond
              ((eq 'st-json:jso (type-of result))
               (let (plist)
                 (st-json:mapjso (lambda (key value)
                                   (setf (getf plist (as-keyword key))
                                         (rec value)))
                                 result)
                 plist))
              ((atom result)
               (if (keywordp result)
                 (case result
                   (:true t)
                   ((:null :false) nil)
                   (t result))
                 result))
              (t
               (mapcar #'rec result)))))
    (when json
      (rec (st-json:read-json json)))))

(defun json<-lisp (sexpr)
  (labels ((rec (item result)
             (cond
              ((atom item) (cond
                            ((eq t item) (write-string "true" result))
                            ((eq nil item) (write-string "null" result))
                            ((stringp item) (format result "\"~a\"" (json-escape item)))
                            ((numberp item) (if (and (rationalp item)
                                                     (not (integerp item)))
                                              (format result "~a" (float item))
                                              (format result "~a" item)))
                            ((symbolp item) (format result "\"~(~s~)\"" item))
                            ((pathnamep item) (format result "\"~a\"" item))
                            (t (error "Unable to handle atom object ~a of type ~a" item (type-of item)))))
              ((plistp item)
               (write-string "{" result)
               (loop for entry in (group item 2)
                     for i from 0
                     unless (= i 0)
                     do (write-string "," result)
                     do (format result "\"~a\":" (string-downcase (symbol-name (first entry))))
                     do (rec (second entry) result))
               (write-string "}" result))
              (t
               (write-string "[" result)
               (loop for entry in item
                     for i from 0
                     unless (= i 0)
                     do (write-string "," result)
                     do (rec entry result))
               (write-string "]" result)))))
    (with-output-to-string (result)
      (rec sexpr result))))