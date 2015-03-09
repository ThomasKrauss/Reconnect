("Basic usages"
 ("Grouping by 1, 2 and 3"
  (equal (group '(1 2 3 4 5 6) 1)
         '((1) (2) (3) (4) (5) (6)))
  (equal (group '(1 2 3 4 5 6) 2)
         '((1 2) (3 4) (5 6)))
  (equal (group '(1 2 3 4 5 6) 3)
         '((1 2 3) (4 5 6)))))