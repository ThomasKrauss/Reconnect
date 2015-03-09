(""
 (null (make-weight-register nil))
 (list= (make-weight-register '((foo 2) (bar 1) (baz 3)))
        '((2 (foo)) (1 (bar)) (3 (baz))))
 (list= (make-weight-register '((foo 2) (bar 1) (baz 3) (foofoo 1) (barbar 2)))
        '((2 (barbar foo)) (1 (foofoo bar)) (3 (baz)))))