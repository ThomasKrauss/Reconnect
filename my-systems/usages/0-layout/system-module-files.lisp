("Example"
 (equal (system-module-files "my-systems")
        (mapcar (lambda (x)
                  (merge-pathnames (concatenate 'string x ".lisp")
                                   (system-source-directory "my-systems")))
                '("0-layout"
                  "1-read-and-print"
                  "2-load-and-save"
                  "3-management"
                  "4-documentation"
                  "5-backup"
                  "6-external-programs"
                  "7-wrap-up"))))

