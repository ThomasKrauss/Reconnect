("Very handy shortcut for emitting HTML in Lisp code.
Emit it to the *standard-output*"
 ("Example"
  (string= (with-output-to-string (s)
             (let ((*standard-output* s))
               (with-html
                (:div :id "test"
                 (:ul
                  (:li "item1")
                  (:li "item2")
                  (:li (:p "item3 description")))))))
           "<div id='test'><ul><li>item1</li><li>item2</li><li><p>item3 description</p></li></ul></div>")))