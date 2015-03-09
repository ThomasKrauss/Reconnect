(in-package :formatter)

; Parenscript macros
(defpsmacro alet (letargs &body body)
  "using cl-this instead of this"
  `(let ((cl-this) ,@letargs)
     (setq cl-this ,@(last body))
     ,@(butlast body)
     (lambda (&rest params)
       (apply cl-this params))))

(defpsmacro alet-fsm (&body states)
  "Redefining alet-fsm for Parenscript."
  `(macrolet ((state (s)
                `(setq cl-this #',s)))
     (labels (,@states)
       #',(caar states))))

(defpsmacro vector-push-extend (value str)
  `(setf ,str (concatenate 'string ,str ,value)))

(defpsmacro nreverse (lst)
  `(chain ,lst (reverse)))

(defpsmacro char= (a b)
  `(= ,a ,b))

(defpsmacro pop (value)
  `(chain ,value (shift)))

(defpsmacro listp (value)
  `(= (chain *object prototype to-string (apply ,value))
      "[object Array]"))

(defpsmacro caar (value)
  `(elt (elt ,value 0) 0))

(defpsmacro cadar (value)
  `(elt (elt ,value 0) 1))

(defpsmacro caaar (value)
  `(elt (elt (elt ,value 0) 0) 0))

(defpsmacro first (value)
  `(elt ,value 0))

(defpsmacro second (value)
  `(elt ,value 1))

(defpsmacro let-a (value &body body)
  `(let ((it ,value))
     ,@body))

(defpsmacro aif (value then-clause else-clause)
  `(let ((it ,value))
     (if (not (null it))
       ,then-clause
       ,else-clause)))

(defpsmacro member (value place)
  `(/= -1 (chain ,place (index-of ,value))))

(defpsmacro find (value place &rest keys)
  (declare (ignore keys))
  `(/= -1 (chain ,place (index-of ,value))))

(defpsmacro push (value place)
  `(chain ,place (unshift ,value)))

(defpsmacro make-array (dimensions &rest keys)
  (declare (ignore keys) (ignore dimensions))
  "")

(defpsmacro rest (lst)
  `(chain ,lst (slice 1)))

; Utilities


; Common Lisp format
(defun cl-format (str &optional (indent-size 4))
  (let ((result (make-array 0 :fill-pointer 0 :adjustable t :element-type 'character))
        (token-characters '()))
    (flet ((insert-n-spaces (n)
             (loop for i from 1 to n
                   do (vector-push-extend #\Space result))
             n)
           (empty-token-characters ()
             (let ((token (make-array 0 :fill-pointer 0 :adjustable t :element-type 'character)))
               (loop for c in (nreverse token-characters)
                     do (vector-push-extend c result)
                     do (vector-push-extend c token))
               (setf token-characters '())
               token))
           (last-char ()
             (elt result (1- (length result)))))
      (let ((fsm (alet (; the current length of the line will help to know to what position to indent
                        (line-length 0)
        
                        ; the position_s_ to indent
                        ; it's a list because of nested lists: we have to follow for each perimeter the
                        ; indent position
                        ;
                        ; It's a list of couples because there are two indent-positions: a regular one and
                        ; an exceptional one.
                        ; -> (regular-position exceptional-position)
                        ;
                        ; When a new perimeter is open, indentation is as follows:
                        ; - (R) regular list: the first element directly follows the left parenthesis
                        ;   Two possibilities then:
                        ;   - (R1) the regular^2 list: the second element is on the same line
                        ;     => the indentation is at the position where this second element begin
                        ;     ex: (10 20
                        ;             30 40
                        ;             50 60)
                        ;   - (R2) the exception: the second element is on the next line
                        ;     => the indentation is at the position where the first element begin
                        ;     ex: (10
                        ;          20
                        ;          30)
                        ; - (E) exception list: the first element does not directly follow the left
                        ;   parenthesis
                        ;   This does not matter: spaces and tabs are ignored and included manually
                        ;   A newline following a left parenthesis is ignored too
                        (indent-position '((0)))
                        
                        ; the count of tokens for each perimeter
                        (token-count '(0))
                        
                        ; for string parsing only
                        escaped
                        quoted
                        commented)
                   (alet-fsm
                     (through (c)
                              (case c
                                ; SPACE TAB
                                ; to ignore
                                ((#\Space #\Tab)
                                 (state through))
                                
                                ; NEWLINE
                                ; A newline does not count as a new character but resets the position
                                ; A newline is ignored if we are waiting for the first token
                                ; It is also ignored at the very beginning of an expression
                                ; A newline cancel commented mode if it was a single line comment
                                (#\Newline
                                 (when (equal #\Newline commented)
                                   (setf commented nil))
                                 (unless (or (= 1 (first token-count))
                                             (= 0 (length result)))
                                   (collect #\Newline)
                                   (setf line-length 0)
                                   (state through)))
                                
                                ; OPENING PARENTHESIS
                                (#\(
                                 ; Since a perimeter can be a token of another perimeter, we add
                                 ; spaces of indentation
                                 (collect #\( :count (1+ (insert-spaces-for-token)))
                                 ; we can't yet count the perimeter as a token because it is not closed
                                 ; in fact, we must initiate a new token count as well as a new indent-position
                                 (push (list 2 line-length) indent-position)
                                 (push 1 token-count)
                                 (state through))
                                
                                ; CLOSING PARENTHESIS
                                (#\)
                                 (collect #\))
                                 ; a perimeter is closed so we can stop counting the tokens it holds and
                                 ; also stop following the number of spaces needed to indent it correctly
                                 (unless (= 1 (length token-count))
                                   (pop token-count))
                                 (unless (= 1 (length indent-position))
                                   (pop indent-position))
                                 ; now that the perimeter is closed, we can count it as a token
                                 (count-token)
                                 (state through))

                                ; QUOTE & BACKQUOTE
                                ((#\' #\`) (setf quoted c))
                                
                                 ; DOUBLE-QUOTE
                                (#\"
                                 ; a string is a token so we add spaces accordingly...
                                 (collect #\" :count (1+ (insert-spaces-for-token)))
                                 ; ... and we count it
                                 (incf (first token-count))
                                 (state through-string))
                                
                                ; SEMI-COLON
                                (#\;
                                 ; switch to commented mode before doing anything
                                 (setf commented #\Newline)
                                 ; a comment is indented like a token but does not count as such
                                 (collect #\; :count (1+ (insert-spaces-for-token)))
                                 (state through-single-line-comment))
                                
                                ; OTHERWISE
                                ; we are parsing a regular token
                                (t
                                 (through-token c)
                                 (state through-token)))
                              c)
                     
                     (collect (c &key (count 1))
                              (incf line-length count)
                              (vector-push-extend c result)
                              c)

                     (special-indent (nb)
                                     (+ (* nb indent-size)
                                        ; the special indent begins at the position of the opening parenthesis
                                        (1- (second (first indent-position)))))

                     (count-token (&optional token)
                                  ; If it was the first token of the perimeter then we have
                                  ; a new regular indent position to compute.
                                  ; If no token was given or if the given token is not recognized as special,
                                  ; the indentation is equal to the current line-length + 1 for the space
                                  ; coming after this token
                                  (when (= 1 (first token-count))
                                    (if token
                                      (cond
                                       ((char= #\: (elt token 0)) ; keyword have special indent -> we assume a plist
                                        (setf (first indent-position)
                                              (let ((form-beginning (second (first indent-position))))
                                                (list (list form-beginning form-beginning)))))
                                       ((find token '("defun" "with-open-file" "let" "progn" "lambda" "defmacro")
                                              :test #'string=)
                                        (setf (first indent-position)
                                              ; only one special-indent should work but it's not the case in JS
                                              (list (list (special-indent 1)
                                                          (special-indent 1)))))
                                       ((find token '("unwind-protect")
                                              :test #'string=)
                                        (setf (first indent-position)
                                              (list (list (special-indent 2)
                                                          (special-indent 1)))))
                                       ((find token '("multiple-value-bind" "register-groups-bind")
                                              :test #'string=)
                                        (setf (first indent-position)
                                              (list (list (special-indent 2)
                                                          (special-indent 2)
                                                          (special-indent 1)))))
                                       (t (setf (caar indent-position) (1+ line-length))))
                                      (setf (caar indent-position) (1+ line-length))))
                                  ; we can increment the count of tokens unless there was no perimeter
                                  (unless (= 1 (length token-count))
                                    (incf (first token-count))))
                     
                     (get-space-of-indentation ()
                                               ; Space of indentation is one by default since we usually
                                               ; separate tokens with one space.
                                               ; There is only one exception: if we are on a new line.
                                               ; Then we are truly indenting and not only separating tokens.
                                               (if (/= 0 line-length)
                                                 1
                                                 ; Is indent-position a singleton? If so, its only element
                                                 ; is the indentation to use or a list of indentations for
                                                 ; special tokens
                                                 (if (= 1 (length (first indent-position)))
                                                   (if (listp (caar indent-position))
                                                     (prog1
                                                         (caaar indent-position)
                                                       ; a comment is indented like all token yet it does not count as
                                                       ; such and does not consume special indentation
                                                       (unless commented
                                                         (setf (caar indent-position)
                                                               (aif (rest (caar indent-position))
                                                                    (if (= 1 (length it))
                                                                      (first it)
                                                                      it)
                                                                    (caaar indent-position)))))
                                                     (caar indent-position))
                                                   ; New line and second element? This is the (R2) case
                                                   ; so we use the exceptional position
                                                   ; In any case, the indent-position becomes a singleton
                                                   ; holding the appropriate value
                                                   (let-a (if (= 2 (first token-count))
                                                            (cadar indent-position)
                                                            (caar indent-position))
                                                     (setf (first indent-position) (list it))
                                                     it))))
                     
                     (insert-spaces-for-token ()
                                              ; only one thing is important for a token
                                              ; -> is it the first one of a perimeter?
                                              ; If so, no space must be inserted
                                              ; If not, one must be inserted
                                              ; Another exception is if result is empty
                                              ; Yet another one is the use of quote
                                              (+ (insert-n-spaces (if (or (= 0 (length result))
                                                                          (= 1 (first token-count)))
                                                                    0
                                                                    (get-space-of-indentation)))
                                                 (if quoted
                                                   (progn
                                                     (vector-push-extend quoted result)
                                                     (setf quoted nil)
                                                     1)
                                                   0)))
                     
                     (through-token (c)
                                     ; a token ends on a space, a tab, a newline or a right parenthesis
                                     (if (member c (list #\Space #\) #\Newline #\Tab))
                                       (progn
                                         ; 1  - Fill in spaces (--> _before_ adding the token)
                                         ; 1' - increment the line-length
                                         ;      + the number of inserted spaces
                                         ;      + the length of the token (we also saved it in the same time)
                                         (incf line-length
                                               (+ (insert-spaces-for-token)
                                                  (length token-characters)))
                                         ; 2 & 3 Add the token to the result then count the token
                                         (count-token (empty-token-characters))
                                         ; THE END: give back the hand to the main process
                                         (through c))
                                       (push c token-characters))
                                     c)
                     
                     (through-string (c)
                                     ; 'escaped' is for this function alone
                                     ; it allows to not go back to the main function, through, upon
                                     ; detecting a " -> when it is escaped, the string is not finished!
                                     (case c
                                       (#\\ (setf escaped (not escaped)))
                                       (#\" (if escaped (setf escaped nil) (state through)))
                                       (t (setf escaped nil)))
                                     (incf line-length)
                                     (vector-push-extend c result))

                     (through-single-line-comment (c)
                                                  (case c
                                                    (#\Newline (through c))
                                                    (t
                                                     (incf line-length)
                                                     (vector-push-extend c result))))

                                                  ))))
        (when str
          (loop for c across str
                do (funcall fsm c)))))
    result))

#|
(defun generate-js (fun-id module-name system-name path)
  (with-open-file (out path
                       :direction :output
                       :if-exists :supersede)
    (write-string (ps* (get-source-code fun-id module-name system-name)) out)))
|#

(defun install-cl-formatter-in-js (path)
  (declare (ignore path))
  nil)
  ;(generate-js "cl-format" "cl-formatter" "formatter" path))

(defun install-cl-format ()
  (with-open-file (out (ensure-directories-exist
                        (my-systems:system-resource "cl-format.js" "formatter"))
                       :direction :output
                       :if-exists :supersede)
    (write-string (ps* (my-systems:load-action-definition 'cl-format)) out)))