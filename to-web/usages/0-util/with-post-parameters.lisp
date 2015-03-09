("Examples"
  (string= (flet ((post-parameter (name)
                    (format nil
                            "This is the value of the parameter named ~a in the request"
                            name)))
             (with-post-parameters (test-to-see) test-to-see))
           "This is the value of the parameter named test-to-see in the request")
  (string= (flet ((post-parameter (name) (format nil "It's ~a" name)))
             (with-post-parameters (test-to-see if-this-works)
               (concatenate 'string
                            test-to-see
                            " and "
                            if-this-works
                            "!")))
           "It's test-to-see and It's if-this-works!"))

("With-post-parameters also accept a couple (function symbol). The function is applied to the post-parameter value before being bound to the symbol"
  (= (flet ((post-parameter (name)
              (cond ((string= name "test-to-see") "1") (t 1000))))
       (with-post-parameters ((#'parse-integer test-to-see)
                              if-this-works)
         (+ test-to-see if-this-works)))
     1001))

("The couple (function symbol) can actually be a list (function symbol1 symbol2 ...). The function is applied to all post-parameter values before these get bound to the given symbols."
  (= (flet ((post-parameter (name)
              (cond ((string= name "test-to-see") "1") (t "1000"))))
       (with-post-parameters ((#'parse-integer
                               test-to-see
                               if-this-works))
         (+ test-to-see if-this-works)))
     1001))