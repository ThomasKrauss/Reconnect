("symb makes a symbol in the current package of all the stuff we passed to it"
 ("Example"
  (eql (symb "test" 1 #\Space '(1 2 3))
       '|test1 (1 2 3)|))
 ("Symbols are accepted too and they are uppercased"
  (eql (symb 'test)
       'test)))