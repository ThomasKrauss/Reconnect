("Bind required arguments"
 (equal '((x 1) (y 2) (z 3)) (bindings<-lambda-list '(x y z) '(1 2 3))))

("Bind optional arguments"
 ("All of them"
  (equal '((x 1) (y 2) (z 3) (opt1 4) (opt2 5) (opt3 6))
         (bindings<-lambda-list '(x y z &optional opt1 opt2 opt3)
                                '(1 2 3 4 5 6))))
 ("Some of them, the rest to their default values, nil if unspecified"
  (equal '((x 1) (y 2) (z 3) (opt1 4) (opt2 nil) (opt3 60))
         (bindings<-lambda-list '(x
                                  y
                                  z
                                  &optional
                                  (opt1 40)
                                  opt2
                                  (opt3 60))
                                '(1 2 3 4))))
 ("When provided in the lambda list, supplied-p parameters are also bound in the result"
  (equal '((x 1)
           (y 2)
           (z 3)
           (opt1 4)
           (opt1-supplied? t)
           (opt2 50)
           (opt3 60)
           (opt3-supplied? nil))
         (bindings<-lambda-list '(x
                                  y
                                  z
                                  &optional
                                  (opt1 40 opt1-supplied?)
                                  (opt2 50)
                                  (opt3 60 opt3-supplied?))
                                '(1 2 3 4)))))

("Bind rest arguments"
 ("No value"
  (equal '((x 1) (y 2) (z 3) (lst nil))
         (bindings<-lambda-list '(x y z &rest lst) '(1 2 3))))
 ("One value"
  (equal '((x 1) (y 2) (z 3) (lst '(10)))
         (bindings<-lambda-list '(x y z &rest lst) '(1 2 3 10))))
 ("Several values"
  (equal '((x 1) (y 2) (z 3) (lst '(10 20 30)))
         (bindings<-lambda-list '(x y z &rest lst) '(1 2 3 10 20 30))))
 ("With some optional arguments beforehand"
  (equal '((x 1) (y 2) (z 3) (opt1 4) (opt2 5) (lst '(10 20 30)))
         (bindings<-lambda-list '(x y z &optional opt1 opt2 &rest lst)
                                '(1 2 3 4 5 10 20 30)))))

("Bind keyword arguments, order do not matter"
 ("All of them"
  (equal '((x 1) (y 2) (z 3) (key1 400) (key2 500) (key3 600))
         (bindings<-lambda-list '(x y z &key key1 key2 key3)
                                '(1
                                  2
                                  3
                                  :key1
                                  400
                                  :key2
                                  500
                                  :key3
                                  600)))
  (equal '((x 1) (y 2) (z 3) (key1 400) (key2 500) (key3 600))
         (bindings<-lambda-list '(x y z &key key1 key2 key3)
                                '(1
                                  2
                                  3
                                  :key2
                                  500
                                  :key3
                                  600
                                  :key1
                                  400))))
 ("Some of them, the rest to their default values, nil if unspecified"
  (equal '((x 1) (y 2) (z 3) (key1 400) (key2 nil) (key3 6000))
         (bindings<-lambda-list '(x
                                  y
                                  z
                                  &key
                                  (key1 4000)
                                  key2
                                  (key3 6000))
                                '(1 2 3 :key1 400))))
 ("When provided in the lambda list, supplied-p parameters are also bound in the result"
  (equal '((x 1)
           (y 2)
           (z 3)
           (key1 400)
           (key1-supplied? t)
           (key2 500)
           (key3 6000)
           (key3-supplied? nil))
         (bindings<-lambda-list '(x
                                  y
                                  z
                                  &key
                                  (key1 40 key1-supplied?)
                                  (key2 5000)
                                  (key3 6000 key3-supplied?))
                                '(1 2 3 :key2 500 :key1 400))))
 ("With some optional arguments beforehand"
  (equal '((x 1)
           (y 2)
           (z 3)
           (opt1 4)
           (opt2 5)
           (key1 400)
           (key2 500)
           (key3 600))
         (bindings<-lambda-list '(x
                                  y
                                  z
                                  &optional
                                  opt1
                                  opt2
                                  &key
                                  key1
                                  key2
                                  key3)
                                '(1
                                  2
                                  3
                                  4
                                  5
                                  :key2
                                  500
                                  :key3
                                  600
                                  :key1
                                  400))))
 ("With a rest argument beforehand"
  (equal '((x 1)
           (y 2)
           (z 3)
           (lst '(10 20 30 :key2 500 :key3 600 :key1 400))
           (key1 400)
           (key2 500)
           (key3 600))
         (bindings<-lambda-list '(x y z &rest lst &key key1 key2 key3)
                                '(1
                                  2
                                  3
                                  10
                                  20
                                  30
                                  :key2
                                  500
                                  :key3
                                  600
                                  :key1
                                  400))))
 ("With some optional arguments and a rest arguments beforehand"
  (equal '((x 1)
           (y 2)
           (z 3)
           (opt1 4)
           (opt2 5)
           (lst '(10 20 30 :key2 500 :key3 600 :key1 400))
           (key1 400)
           (key2 500)
           (key3 600))
         (bindings<-lambda-list '(x
                                  y
                                  z
                                  &optional
                                  opt1
                                  opt2
                                  &rest
                                  lst
                                  &key
                                  key1
                                  key2
                                  key3)
                                '(1
                                  2
                                  3
                                  4
                                  5
                                  10
                                  20
                                  30
                                  :key2
                                  500
                                  :key3
                                  600
                                  :key1
                                  400)))))

