(""
 (equal '(system-name module-name)
        (key-name-hierarchy :module '(:system :module :action)))
 (equal '(system-name)
        (key-name-hierarchy :system '(:system :module :action))))