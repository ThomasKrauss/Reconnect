("mac send a string containing the macro-expanded code"
 ("Example"
  (string= (mac (when (= 1 1)
                  (+ 1 2)
                  (+ 3 4)))
           "
(IF (= 1 1) (PROGN (+ 1 2) (+ 3 4)))")))