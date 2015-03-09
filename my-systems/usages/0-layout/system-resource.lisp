("Example"
 (let ((file
        (merge-pathnames "resource-example.txt"
                         (system-resources-directory "my-systems"))))
   (and (file-exists-p file))
   (my-equal (system-resource "resource-example.txt" "my-systems")
             file)))

