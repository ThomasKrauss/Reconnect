(in-package :to-web)

; perimeter count but avoiding counting parens in strings!
; read as code on the last parens
; commit inside header or paragraph or standalone
; escaping parens

(defun get-inner-text-list (characters)
  "inserting a :br directly in the characters list. Or other stuff."
  (let (result
        (stream (make-string-output-stream)))
    (dolist (char characters)
      (if (characterp char)
       (princ char stream)
       (progn
         (push (get-output-stream-string stream) result)
         (push char result)
         (setf stream (make-string-output-stream)))))
    (let-a (get-output-stream-string stream)
      (when (< 0 (length it))
        (push it result)))
    (nreverse result)))

(defun lisp-read (characters)
  (with-input-from-string (in (with-output-to-string (out)
                                (dolist (char characters)
                                  (princ char out))))
    (read in)))

(defun html<-text (stream)
  (let (result line-characters side-characters)
    (labels ((commit (tag)
               (prog1
                   (push `(,tag (:noescape
                                 ,@(get-inner-text-list (nreverse line-characters))))
                         result)
                 (setf line-characters nil
                       side-characters nil)))
             (commit-paragraph ()
               (commit :p))
             (commit-header1 ()
               (pop line-characters)
               (commit :h1))
             (commit-header2 ()
               (pop line-characters)
               (commit :h2))
             (commit-raw (html)
               (push html result)))
      (macrolet ((push-string (str)
                   `(progn
                      ,@(loop for char across str
                              collect `(push ,char line-characters))))
                 (push-char (&optional char)
                   `(push ,(if char char 'char) line-characters))
                 (char-aside ()
                   `(push char side-characters))
                 (no-new-and (&rest setf-clauses)
                   `(setf ,@setf-clauses
                          new-word nil
                          new-line nil))
                 (no-new ()
                   `(setf new-word nil
                          new-line nil))
                 (markdown-cond (&rest clauses)
                   `(cond
                     ,@(loop for clause in clauses
                             collect `((equal side-characters
                                              ',(loop for char across (reverse (first clause))
                                                      collect char))
                                       ,@(rest clause))))))
        (let ((fsm (alet ((new-word t)
                          (new-line t)
                          double-quote-opened
                          possible-underline possible-double-underline
                          possible-escape
                          spacing force-non-breaking-space
                          possible-markup in-markup-string markup-escape (perimeter-count 0))
                     (alet-fsm
                       (parse (char)
                              (unless (or (assess-markup char)
                                          (handle-whitespaces char)
                                          (assess-underline char)
                                          (markdown-escape char)
                                          (special-syntax char))
                                (empty-side-characters)
                                (unless (or (html-escape char)
                                            (assess-double-quote char))
                                  (case char
                                    (#\:
                                     (char-aside)
                                     (no-new-and possible-escape t))
                                    (#\'
                                     (if new-word (char-aside) (push-string "&rsquo;"))
                                     (no-new))
                                    (#\(
                                     (char-aside)
                                     (setf possible-markup t))
                                    (t
                                     (no-new)
                                     (push-char))))))
                       (assess-double-quote (char)
                                            (when (char= char #\")
                                              (if new-word
                                                (push-string "&ldquo;")
                                                (if double-quote-opened
                                                  (push-string "&rdquo;")
                                                  (push-char)))
                                              (no-new-and double-quote-opened new-word)
                                              t))
                       (assess-markup (char)
                                      (when possible-markup
                                        (if (char= char #\:)
                                          (progn
                                            (char-aside)
                                            (setf possible-markup nil
                                                  perimeter-count 1)
                                            (state parse-markup))
                                          (progn
                                            (no-new)
                                            (empty-side-characters)
                                            (push-char)
                                            nil))))
                       (html-escape (char)
                                    (case char
                                      (#\& (push-string "&amp;"))
                                      (#\< (push-string "&lt;"))
                                      (#\> (push-string "&gt;"))))
                       (empty-side-characters (&key no-commit keep-spacing read-as-lisp)
                                              (unless (or no-commit
                                                          (and (not read-as-lisp)
                                                               (special-syntax)))
                                                (if read-as-lisp
                                                  (let-a (lisp-read (nreverse side-characters))
                                                    (if line-characters
                                                      (push it line-characters)
                                                      (commit-raw it)))
                                                  (dolist (char (nreverse side-characters))
                                                    (unless (html-escape char)
                                                      (push-char)))))
                                              (setf side-characters nil
                                                    possible-underline nil
                                                    possible-double-underline nil
                                                    possible-escape nil
                                                    spacing (when keep-spacing spacing)
                                                    force-non-breaking-space (when keep-spacing force-non-breaking-space)
                                                    possible-markup nil))
                       (handle-whitespaces (char)
                                           (when (member char '(#\Space #\Newline #\Tab))
                                             (setf new-word t)
                                             (case char
                                               (#\Newline
                                                (setf new-line t)
                                                (cond
                                                 (possible-underline
                                                  (empty-side-characters :no-commit t)
                                                  (commit-header2))
                                                 (possible-double-underline
                                                  (empty-side-characters :no-commit t)
                                                  (commit-header1))
                                                 (t
                                                  (empty-side-characters)
                                                  (state maybe-paragraph-end))))
                                               (#\Space
                                                (empty-side-characters :keep-spacing t)
                                                (unless spacing
                                                  (if force-non-breaking-space
                                                    (push-string "&nbsp;")
                                                    (push-char))
                                                  (setf spacing t))
                                                t)
                                               (#\Tab
                                                (empty-side-characters :keep-spacing t)
                                                (unless (or spacing new-line)
                                                  (if force-non-breaking-space
                                                    (push-string "&nbsp;")
                                                    (push-char #\Space))
                                                  (setf spacing t))
                                                t))))
                       (maybe-paragraph-end (char)
                                            (case char
                                              (#\Newline
                                               (commit-paragraph)
                                               (state parse))
                                              (t
                                               (push-char #\Newline)
                                               (parse char)
                                               (state parse))))
                       (assess-underline (char)
                                         (unless possible-escape
                                           (case char
                                             (#\-
                                              (no-new-and possible-underline (or possible-underline new-line))
                                              (char-aside))
                                             (#\=
                                              (no-new-and possible-double-underline (or possible-double-underline new-line))
                                              (if possible-double-underline (char-aside) (push-char))))))
                       (markdown-escape (char)
                                        (when possible-escape
                                          (char-aside)
                                          (when (markdown-cond
                                                 (":'"
                                                  (push-char #\'))
                                                 (":\""
                                                  (push-char #\"))
                                                 (":-"
                                                  (push-char #\-))
                                                 (":."
                                                  (push-char #\.))
                                                 (":("
                                                  (push-char #\())
                                                 (":para"
                                                  (setf force-non-breaking-space t)
                                                  (push-string "&para;"))
                                                 (":sect"
                                                  (setf force-non-breaking-space t)
                                                  (push-string "&sect;"))
                                                 (":trade"
                                                  (push-string "&trade;"))
                                                 (":reg"
                                                  (push-string "&reg;"))
                                                 (":copy"
                                                  (setf force-non-breaking-space t)
                                                  (push-string "&copy;"))
                                                 (":nbsp"
                                                  (setf spacing t)
                                                  (push-string "&nbsp;"))
                                                 (":br"
                                                  (setf spacing t)
                                                  (push :br line-characters)))
                                            (empty-side-characters :no-commit t :keep-spacing t))
                                          char))
                       (special-syntax (&optional char)
                                       (if (and char
                                                (char= char #\.)
                                                (every (lambda (char)
                                                         (char= char #\.))
                                                       side-characters)
                                                (< (length side-characters) 3))
                                         (progn
                                           (no-new)
                                           (char-aside))
                                         (progn
                                           (when (markdown-cond
                                                  ("'"
                                                   (if (and char (char= #\' char))
                                                     (push-string "&rsquo;")
                                                     (push-string "&lsquo;")))
                                                  ("-"
                                                   (unless (or possible-underline
                                                               (and char (char= #\- char)))
                                                     (push-string "&#8209;")))
                                                  ("--"
                                                   (unless (or possible-underline
                                                               (and char (char= #\- char)))
                                                     (push-string "&ndash;")))
                                                  ("---"
                                                   (unless possible-underline
                                                     (push-string "&mdash;")))
                                                  ("..."
                                                   (push-string "&hellip;")))
                                             (when (and char
                                                        (not (member char (list #\' #\-)))
                                                        (not (assess-double-quote char)))
                                               (push-char))
                                             (empty-side-characters :no-commit t)
                                             t))))
                       (parse-markup (char)
                                     (char-aside)
                                     (case char
                                       (#\(
                                        (unless in-markup-string
                                          (incf perimeter-count)))
                                       (#\)
                                        (unless in-markup-string
                                          (decf perimeter-count)))
                                       (#\"
                                        (if markup-escape
                                          (setf markup-escape nil)
                                          (setf in-markup-string (not in-markup-string))))
                                       (#\\
                                        (when in-markup-string
                                          (setf markup-escape t))))
                                     (when (= 0 perimeter-count)
                                       (empty-side-characters :read-as-lisp t)
                                       (state parse)))
                       ))))
          (loop for char = (read-char stream nil)
                while char
                do (funcall fsm char))
          (when line-characters
            (funcall fsm #\Newline))
          (when line-characters
            (funcall fsm #\Newline))
          (nreverse result))))))

(defun html<-text-string (str)
  (with-input-from-string (in str)
    (html<-text in)))

(defun html<-text-file (filename)
  (with-open-file (in filename)
    (html<-text in)))

(defun text-string-test (text-string str)
  (equal (html<-text-string text-string)
         `((:p (:noescape ,str)))))