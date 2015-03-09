(""
 ("No body"
  ("Example of an element accepting a body"
   (test-html (emit-html '(:p))
              "<p></p>"))
  ("Elements which do not accepting a body"
   (test-html (emit-html '(:area))
              "<area />")
   (test-html (emit-html '(:base))
              "<base />")
   (test-html (emit-html '(:br))
              "<br />")
   (test-html (emit-html '(:col))
              "<col />")
   (test-html (emit-html '(:hr))
              "<hr />")
   (test-html (emit-html '(:img))
              "<img />")
   (test-html (emit-html '(:input))
              "<input />")
   (test-html (emit-html '(:link))
              "<link />")
   (test-html (emit-html '(:meta))
              "<meta />")
   (test-html (emit-html '(:param))
              "<param />")))
 ("With attributes only"
  (test-html (emit-html '(:p :class "test" :onclick "do();"))
             "<p class='test' onclick='do();'></p>"))
 ("With body only"
  ("One element as a body"
   (test-html (emit-html '(:p "foo"))
              "<p>foo</p>"))
  ("Several elements as a body"
   (test-html (emit-html '(:p "foo" (:em "bar") "baz"))
              "<p>foo<em>bar</em>baz</p>")))
 ("With attributes and body"
  (test-html (emit-html '(:p :class "test" :onclick "do();"
                          "foo" (:em "bar") "baz"))
             "<p class='test' onclick='do();'>foo<em>bar</em>baz</p>"))
 ("Escaping text"
  (test-html (emit-html '(:p "Escape the following characters: < & >"))
             "<p>Escape the following characters: &lt; &amp; &gt;</p>"))
 ("Escaping attributes"
  (test-html (emit-html '(:p :class "Escape < & > \" '" "The text."))
             "<p class='Escape &lt; &amp; &gt; &quot; &apos;'>The text.</p>"))
 ("Special operators available"
  (":noescape"
   (test-html (emit-html '(:p (:noescape "Escape the following characters: < & >")))
             "<p>Escape the following characters: < & ></p>")
   (test-html (emit-html '(:p :class (:noescape "Escape < & > \" '") "The text."))
              "<p class='Escape < & > \" ''>The text.</p>"))
  (":attribute"
   (test-html (emit-html '(:p (:attribute "Escape \" '")))
              "<p>Escape &quot; &apos;</p>"))
  (":progn"
   (test-html (emit-html '(:p (:progn "foo" "bar" "baz")))
              "<p>foobarbaz</p>"))))