("For list-equal, the order of the element does not matter"
 ("Examples"
  (not (equal '(1 2 3) '(2 3 1)))
  (list-equal '(1 2 3) '(2 3 1))))
("For lists to be \"list-equaled\", no element should be missing"
 ("Example"
  (not (list-equal '(1 2 3) '(2 3)))))
("For lists to be \"list-equaled\", any element can be duplicated"
 ("Examples"
  (list-equal '(1 2 3) '(1 2 3 1))
  (list-equal '(1 2 3) '(1 2 3 2 1))))