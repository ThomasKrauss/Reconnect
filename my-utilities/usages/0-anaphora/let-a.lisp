("let-a helps to avoid creating ourselves a binding each time a value is needed more than once."
 ("Example"
  (equal (let-a (position 2 '(1 2 3))
                it)
         1)))