(""
 ("Empty query" (equal (complete-query nil) '(:in)))
 ("Names only"
  (equal (complete-query '("foo" "bar")) '(:in "foo" :in "bar")))
 ("Keyword :in at the end"
  (equal (complete-query '("foo" "bar" :in))
         '(:in "foo" :in "bar" :in)))
 ("Other keyword at the end takes the place of :in"
  (equal (complete-query '("foo" "bar" :test))
         '(:in "foo" :in "bar" :test))))

