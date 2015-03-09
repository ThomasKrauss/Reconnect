("Example"
 (let ((file
        (merge-pathnames "test-with-lisp-file.lisp"
                         (system-work-directory "my-systems"))))
   (unwind-protect
       (let ((forms '((list 1 2 3) (list 4 5 6) (list 7 8 9))))
         (with-lisp-file file "my-systems"
           (first forms)
           (second forms)
           (third forms))
         (and (file-exists-p file)
              (string= (with-output-to-string (str)
                         (loop for form in forms
                               do (format str "~(~a~)~%~%" form)))
                       (load-file-content file))))
     (if (file-exists-p file) (delete-file file)))))

("An existing lisp file is not superseded. Forms are appended to it."
 (let ((filename
        (merge-pathnames "test-with-lisp-file.lisp"
                         (system-work-directory "my-systems"))))
   (unwind-protect
       (let ((forms '((list 1 2 3) (list 4 5 6) (list 7 8 9))))
         (with-lisp-file filename "my-systems" (first forms))
         (with-lisp-file filename "my-systems" (second forms))
         (with-lisp-file filename "my-systems" (third forms))
         (and (file-exists-p filename)
              (string= (with-output-to-string (str)
                         (loop for form in forms
                               do (format str "~(~a~)~%~%" form)))
                       (load-file-content filename))))
     (if (file-exists-p filename) (delete-file filename)))))

