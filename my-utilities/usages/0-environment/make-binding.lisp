("Normal case" (equal (make-binding 'x 10) '(x 10)))

("Provided nil value" (equal (make-binding 'x nil) '(x nil)))

("No value provided" (equal (make-binding 'x nil) 'x))

