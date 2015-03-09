(""
 ("Query out of plural keys"
  (equal (to-getf-keys '("foo" "bar" "baz") '(:systems :modules :actions))
         '(:in "foo" :in "bar" :in "baz")))
 ("Pluralize keys"
  (equal (to-getf-keys '("foo" "bar" "baz") '(:system :module :action))
         '(:in "foo" :in "bar" :in "baz"))))