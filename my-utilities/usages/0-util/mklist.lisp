("mklist examples"
 ("mklist makes a list"
  (equal (mklist 1) '(1))
  (equal (mklist "test") '("test")))
 ("mklist does not make a list when it already have one"
  (equal (mklist '(1)) '(1))
  (equal (mklist '("test")) '("test"))))