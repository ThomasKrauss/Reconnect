(in-package :my-backquote)

(defun print-backquote (str)
  "Transform backquote related action perimeters into syntax:
- ([my-backquote:]backquote expr)     => `expr
- (#:comma expr)                      => ,expr
- (#:comma-atsign expr)               => ,@expr
- (#:comma-dot expr)                  => ,.expr"
  (let (result action-name)
    (flet ((special-action-name? ()
             (let ((name (concatenate 'string (reverse action-name))))
               (cond
                ((or (string= name "backquote")
                     (string= name "my-backquote:backquote"))
                 (list #\`))
                ((string= name "#:comma") (list #\,))
                ((string= name "#:comma-atsign") (list #\, #\@))
                ((string= name "#:comma-dot") (list #\, #\.)))))
           (push-action-name-to-result (&optional char-list)
             (let ((name (or char-list (nreverse action-name))))
               (dolist (char name)
                 (push char result)))
             (setf action-name nil))
           (get-result-string ()
             (concatenate 'string (reverse result))))
      (let ((fsm (let (state-fn
                       (perimeter-level 0)
                       special-perimeter-levels)
                   (setq state-fn
                         (macrolet ((state (s) `(setq state-fn #',s)))
                           (labels ((parse (char)
                                      (case char
                                        (#\(
                                         (incf perimeter-level)
                                         (push char result)
                                         (state parse-action-name))
                                        (#\)
                                         (if (and special-perimeter-levels
                                                  (= perimeter-level (first special-perimeter-levels)))
                                           (progn
                                             (pop special-perimeter-levels)
                                             (decf perimeter-level))
                                           (progn
                                             (decf perimeter-level)
                                             (push char result))))
                                        (#\"
                                         (push char result)
                                         (state parse-string))
                                        (t
                                         (push char result))))
                                    (parse-action-name (char)
                                      (case char
                                        ((#\Space #\Tab #\Newline #\( #\) #\')
                                         (let ((name (special-action-name?)))
                                           (if name
                                             (progn
                                               (pop result)
                                               (push perimeter-level special-perimeter-levels)
                                               (push-action-name-to-result name)
                                               (special-parse char))
                                             (progn
                                               (push-action-name-to-result)
                                               (parse char)))))
                                        (t (push char action-name))))
                                    (special-parse (char)
                                      (case char
                                        ((#\Space #\Tab #\Newline)
                                         (state special-parse))
                                        (#\'
                                         (push char result)
                                         (state special-parse))
                                        (#\(
                                         (push char result)
                                         (incf perimeter-level)
                                         (state parse-action-name))
                                        (#\)
                                         (error "A special action must have exactly one argument. None were found and parsing was thus stopped at ~s" (get-result-string)))
                                        (t
                                         (push char result)
                                         (state parse))))
                                    (parse-string (char)
                                      (push char result)
                                      (when (char= #\" char)
                                        (state parse))))
                             #'parse)))
                   (lambda (&rest params)
                     (apply state-fn params)))))
        (when str
          (loop for char across str
                do (funcall fsm char)))
        (get-result-string)))))