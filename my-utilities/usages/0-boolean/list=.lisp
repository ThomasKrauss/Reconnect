("For list=, the order of the element does not matter"
 ("Examples"
  (not (equal '(1 2 3) '(2 3 1)))
  (list= '(1 2 3) '(2 3 1))))
("For lists to be \"list=ed\", no element"
 ("should be missing"
  (not (list= '(1 2 3 4) '(2 3))))
 ("should be duplicated"
  (not (list= '(1 2 3) '(1 2 3 1)))))