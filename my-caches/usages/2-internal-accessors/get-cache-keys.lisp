(""
 (with-cache-example
  ()
  (equal (get-cache-keys cache-name)
         '(:alpha :beta :gamma))))