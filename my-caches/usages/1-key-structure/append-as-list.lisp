(""
 (equal '(lambda (x) (incf x) (* x 2))
        (append-as-list '(lambda (x))
                        '(incf x)
                        '(* x 2))))