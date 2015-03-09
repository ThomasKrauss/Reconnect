("Getting and setting values with get-named-plist"
 ("Getting a value"
  (equal '(:name "test" :value 1) (get-named-plist "test" (list '(:name "foo" :value 0)
                                                                '(:name "test" :value 1)
                                                                '(:name "bar" :value 2)))))
 ("Setting a value"
  ("Changing an existing value of an existing named plist"
   (let ((plist (list '(:name "foo" :value 0)
                      '(:name "test" :value 1)
                      '(:name "bar" :value 2))))
     (setf (get-named-plist "test" plist) (list :value 10))
     (equal plist (list '(:name "foo" :value 0)
                        '(:name "test" :value 10)
                        '(:name "bar" :value 2)))))
  ("Adding a new value to an existing named plist"
   (let ((plist (list '(:name "foo" :value 0)
                      '(:name "test" :value 1)
                      '(:name "bar" :value 2))))
     (setf (get-named-plist "test" plist) (list :other 10))
     (equal plist (list '(:name "foo" :value 0)
                        '(:other 10 :name "test" :value 1)
                        '(:name "bar" :value 2)))))
  ("Adding a new named plist entirely"
   (let ((plist (list '(:name "foo" :value 0)
                      '(:name "bar" :value 2))))
     (setf (get-named-plist "test" plist) (list :value 1))
     (equal plist (list '(:name "test" :value 1)
                        '(:name "foo" :value 0)
                        '(:name "bar" :value 2)))))))