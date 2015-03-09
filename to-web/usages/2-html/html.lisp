(""
 ("No body"
  ("Element accepting a body"
   (test-html (html (:p))
              "<p></p>"))
  ("Elements which do not accepting a body"
   (test-html (html (:area))
              "<area />")
   (test-html (html (:base))
              "<base />")
   (test-html (html (:br))
              "<br />")
   (test-html (html (:col))
              "<col />")
   (test-html (html (:hr))
              "<hr />")
   (test-html (html (:img))
              "<img />")
   (test-html (html (:input))
              "<input />")
   (test-html (html (:link))
              "<link />")
   (test-html (html (:meta))
              "<meta />")
   (test-html (html (:param))
              "<param />")))
 ("With attributes only"
  (test-html (html (:p :class "test" :onclick "do();"))
             "<p class='test' onclick='do();'></p>"))
 ("With body only"
  ("One element as a body"
   (test-html (html (:p "foo"))
              "<p>foo</p>"))
  ("Several elements as a body"
   (test-html (html (:p "foo" (:em "bar") "baz"))
              "<p>foo<em>bar</em>baz</p>")))
 ("With attributes and body"
  (test-html (html (:p :class "test" :onclick "do();"
                    "foo" (:em "bar") "baz"))
             "<p class='test' onclick='do();'>foo<em>bar</em>baz</p>"))
 ("With external value"
  (test-html (let ((x 10))
               (html (:p x)))
             "<p>10</p>"))
 ("With code"
  (test-html (html (:ul (dolist (x '("foo" "bar" "baz")) (html (:li x)))))
             "<ul><li>foo</li><li>bar</li><li>baz</li></ul>"))
 ("Escaping text"
  (test-html (html (:p "Escape the following characters: < & >"))
             "<p>Escape the following characters: &lt; &amp; &gt;</p>"))
 ("Escaping attributes"
  (test-html (html (:p :class "Escape < & > \" '" "The text."))
             "<p class='Escape &lt; &amp; &gt; &quot; &apos;'>The text.</p>"))
 ("Special operators available"
  (":noescape"
   (test-html (html (:p (:noescape "Escape the following characters: < & >")))
             "<p>Escape the following characters: < & ></p>")
   (test-html (html (:p :class (:noescape "Escape < & > \" '") "The text."))
              "<p class='Escape < & > \" ''>The text.</p>"))
  (":attribute"
   (test-html (html (:p (:attribute "Escape \" '")))
              "<p>Escape &quot; &apos;</p>"))
  (":print"
   (test-html (html (:p (:print (+ 1 2))))
              "<p>3</p>"))
  (":format"
   (test-html (html (:p (:format "Value: ~a" "test")))
              "<p>Value: test</p>"))
  (":progn"
   (test-html (html (:p (:progn "foo" "bar" "baz")))
              "<p>foobarbaz</p>")))
 ("Automatic print of attributes, since there is no other meaningful option"
  (test-html (html (:p :class (mkstr "my-" "class") "test"))
             "<p class='my-class'>test</p>")))