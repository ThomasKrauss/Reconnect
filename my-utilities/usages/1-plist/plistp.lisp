("plistp identifies a property list"
 ("nil is a plist"
  (plistp nil))
 ("Example of a plist"
  (plistp '(:test 1 :to 2 :see 3)))
 ("A keyword can't be without a value"
  (not (plistp '(:test 1 :to 2 :see)))
  (not (plistp '(:test 1 :to :see 3))))
 ("A value can't be without a keyword"
  (not (plistp '(1 :to 2 :see)))
  (not (plistp '(:test 1 2 :see 3))))
 ("Keywords must be keywords"
  (not (plistp '(:test 1 'to 2 :see 3))))
 ("Values can be keywords"
  (plistp '(:test 1 :to :2 :see 3))))