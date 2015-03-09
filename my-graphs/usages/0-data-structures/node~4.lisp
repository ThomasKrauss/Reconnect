("Examples"
 (null (node? 3))
 (null (node? "this is a node?"))
 (null (node? '(1)))
 (null (node? '(1 2)))
 (equal (node? '(1 2 3)) '(1 2 3)))

