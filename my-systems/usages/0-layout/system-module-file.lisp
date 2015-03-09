("Example"
 (my-equal (system-module-file "0-layout" "my-systems")
           (merge-pathnames "0-layout.lisp"
                            (system-source-directory "my-systems"))))

("The module file may not exist"
 (let ((file
        (merge-pathnames "no-such-module.lisp"
                         (system-source-directory "my-systems"))))
   (and (null (file-exists-p file))
        (my-equal (system-module-file "no-such-module" "my-systems")
                  file))))

