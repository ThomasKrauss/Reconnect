("Add the given scope to the given environment as its last scope and, consequently, as the one of highest priority."
 (equal '((let ((x 1))))
        (add-scope '(let ((x 1))) nil))
 (equal '((let ((y 2) (z 3))) (let ((x 1))))
        (add-scope '(let ((y 2) (z 3))) '((let ((x 1)))))))