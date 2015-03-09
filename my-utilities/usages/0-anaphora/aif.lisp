("aif binds the result of the test to the variable it. Useful for generalized booleans as it easily lets the value falls through while still testing it's not null."
 ("Examples"
  (string= (aif (position 2 '(1 2 3))
                (format nil "ok: ~a" it)
                "ko")
           "ok: 1")
  (string= (aif (position 2 '(1 20 3))
                (format nil "ok: ~a" it)
                "ko")
           "ko")))