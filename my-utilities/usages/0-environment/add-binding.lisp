(""
 (null (add-binding '(x 1) nil))
 (equal (add-binding nil '((let ((y 2)))))
        '((let ((y 2)))))
 (equal (add-binding '(x 1) '((let ((y 2)))))
        '((let ((y 2) (x 1)))))
 (equal (add-binding '(x 1) '((flet test (x) (1+ x))))
        '((flet test (x) (1+ x)))))