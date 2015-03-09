("as-keyword build a keyword symbol of all the stuff we pass to it"
 ("Example"
  (eql (as-keyword "test" 1 #\Space '(1 2 3))
       :|TEST1 (1 2 3)|))
 ("Symbols are accepted too and they are uppercased"
  (eql (as-keyword 'test)
       :test)))