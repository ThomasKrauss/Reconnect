("print-code-chunk ensures that the given form is printed as if written by a human."
 ("Symbols are printed in lower case"
  (string= (let ((form '(list 1 2 3)))
             (with-output-to-string (s) (write form :stream s)))
           "(LIST 1 2 3)")
  (string= (let ((form '(list 1 2 3)))
             (print-code-chunk form "my-systems"))
           "(list 1 2 3)"))
 ("Symbols are printed without the package if you pass the appropriate perspective."
  (string= (let ((*package* (find-package "CL"))
                 (form '(system-source-directory "my-systems")))
             (with-output-to-string (s) (write form :stream s)))
           "(MY-SYSTEMS:SYSTEM-SOURCE-DIRECTORY \"my-systems\")")
  (string= (let ((form '(system-source-directory "my-systems")))
             (print-code-chunk form "my-systems"))
           "(system-source-directory \"my-systems\")")
  (string= (let ((form '(system-source-directory "my-systems")))
             (print-code-chunk form "my-lisp-parsing"))
           "(system-source-directory \"my-systems\")")))

