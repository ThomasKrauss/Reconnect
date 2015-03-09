(""
 (null (count-action-usage nil))
 (list= '((foo 2) (bar 1) (baz 3))
        (count-action-usage '(foo bar baz baz foo baz))))