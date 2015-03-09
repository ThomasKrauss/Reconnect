("Get the file of a module, not much to say!"
 ("Example"
  (equal (let-a (get-module-file "0-layout" "my-systems")
                (append (pathname-directory it)
                        (list (file-namestring it))))
         '(:absolute "home" "thomas" "dev" "my-systems" "0-layout.lisp"))))