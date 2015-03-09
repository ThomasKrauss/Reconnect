(""
 ("Empty list"
  (null (plist<-list nil)))
 ("Ignore list if no keyword as first element"
  (null (plist<-list '(1 :foo 2 :bar 3))))
 ("Ignore list if no appropriate succession of keywords and elements"
  (null (plist<-list '(1 :foo 2 3 :bar 4))))
 ("No change on a plist"
  (equal '(:foo 1 :bar 2 :baz 3)
         (plist<-list '(:foo 1 :bar 2 :baz 3))))
 ("Infer nil in plist"
  (equal '(:foo 1 :bar nil :baz 3)
         (plist<-list '(:foo 1 :bar :baz 3))))
 ("Infer nil in plist even at the end"
  (equal '(:foo 1 :bar nil :baz nil)
         (plist<-list '(:foo 1 :bar :baz)))))