("Get the keys of the cache of given name up to the given key."
 (equal '(:system :module)
        (key-hierarchy :module '(:system :module :action)))
 (equal '(:system)
        (key-hierarchy :system '(:system :module :action))))