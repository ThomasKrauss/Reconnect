("Flatten examples"
 ("Flattening an already flat list"
  (equal (flatten '(1 2 3))
         '(1 2 3)))
 ("Flattening usage"
  (equal (flatten '(1 2 (3 4 (5 6) (7 8)) (((((9)))))))
         '(1 2 3 4 5 6 7 8 9))))