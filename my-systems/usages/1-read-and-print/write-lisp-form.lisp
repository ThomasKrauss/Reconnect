("Write the given form followed by 2 newlines."
 ("Example"
  (let ((form '(list 1 2 3)))
    (string= (with-output-to-string (s)
               (write-lisp-form form "cl-user" s))
             (format nil "~(~a~)~%~%" form)))))

