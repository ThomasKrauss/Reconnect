("reduce-conc reduces a list of lists by concatenating all of them"
 ("nil gives nil"
  (null (reduce-conc nil)))
 ("one sublist ends up being the result"
  (equal (reduce-conc '((1 2 3)))
         '(1 2 3)))
 ("example"
  (equal (reduce-conc '((1 2 3) (4) (5 6) (7 8 9)))
         '(1 2 3 4 5 6 7 8 9))))