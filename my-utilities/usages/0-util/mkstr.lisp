("mkstr makes a string with all the stuff we pass in"
 ("A string is unchanged"
  (string= (mkstr "test") "test"))
 ("Numbers, characters, lists... everything is appended"
  (string= (mkstr "test" 1 #\Space '(1 2 3))
           "test1 (1 2 3)"))
 ("Symbols are gobbled up too but beware, they are uppercased!"
  (string= (mkstr 'test)
           "TEST")))