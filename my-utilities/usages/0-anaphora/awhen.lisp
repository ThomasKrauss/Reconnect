("awhen binds the result of the test to the variable it. Useful for generalized booleans as it easily lets the value falls through while still testing it's not null."
 ("Examples"
  (equal (let ((x 10))
           (awhen (position 2 '(1 2 3))
                  (incf x it)
                  (list x it)))
         '(11 1))
  (null (let ((x 10))
           (awhen (position 2 '(1 20 3))
                  (incf x it)
                  (list x it))))))
