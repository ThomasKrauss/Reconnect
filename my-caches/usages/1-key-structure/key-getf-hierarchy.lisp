(""
 ("Empty query list on :root"
  (null (key-getf-hierarchy :root '(:system :module :action))))
 ("Completely named query when the key is singular"
  (equal (key-getf-hierarchy :module '(:system :module :action))
         '(:in system-name :in module-name))))