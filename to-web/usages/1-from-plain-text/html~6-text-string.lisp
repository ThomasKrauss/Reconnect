(""
 ("Headers"
  ("Header 1"
   (equal (html<-text-string (mkstr "Testing a header 1"
                                   #\Newline
                                   "=================="
                                   #\Newline))
          '((:h1 (:noescape "Testing a header 1"))))
   (equal (html<-text-string (mkstr "Testing a header 1"
                                   #\Newline
                                   "="
                                   #\Newline))
          '((:h1 (:noescape "Testing a header 1")))))
  ("Header 2"
   (equal (html<-text-string (mkstr "Testing a header 2"
                                   #\Newline
                                   "------------------"
                                   #\Newline))
          '((:h2 (:noescape "Testing a header 2"))))
   (equal (html<-text-string (mkstr "Testing a header 2"
                                   #\Newline
                                   "-"
                                   #\Newline))
          '((:h2 (:noescape "Testing a header 2"))))))
 ("Paragraphs"
  ("One of one line"
   (equal (html<-text-string (mkstr "Testing a simple paragraph"))
          '((:p (:noescape "Testing a simple paragraph")))))
  ("One of several lines"
   (equal (html<-text-string (mkstr "Testing a paragraph of three lines"
                                   #\Newline
                                   "There is the second one!"
                                   #\Newline
                                   "And that is the last!"))
          (list
           (list :p
                 (list :noescape
                       (mkstr "Testing a paragraph of three lines"
                              #\Newline
                              "There is the second one!"
                              #\Newline
                              "And that is the last!"))))))
  ("Several one of one line"
   (equal (html<-text-string (mkstr "This is the first paragraph."
                                   #\Newline
                                   #\Newline
                                   "The second paragraph is here!"
                                   #\Newline
                                   #\Newline
                                   "The third paragraph is there."))
          '((:p (:noescape "This is the first paragraph."))
            (:p (:noescape "The second paragraph is here!"))
            (:p (:noescape "The third paragraph is there.")))))
  ("Several one of several lines"
   (equal (html<-text-string (mkstr "A first paragraph with two lines:"
                                   #\Newline
                                   "There is the second line!"
                                   #\Newline
                                   #\Newline
                                   "A second paragraph of one line."
                                   #\Newline
                                   #\Newline
                                   "A third of three lines."
                                   #\Newline
                                   "Its second line."
                                   #\Newline
                                   "And the last!"))
          (list
           (list :p
                 (list :noescape
                       (mkstr "A first paragraph with two lines:"
                              #\Newline
                              "There is the second line!")))
           '(:p (:noescape "A second paragraph of one line."))
           (list :p
                 (list :noescape
                       (mkstr "A third of three lines."
                              #\Newline
                              "Its second line."
                              #\Newline
                              "And the last!")))))))
 ("Enforce one space between words and sentences."
  (text-string-test "Too  much spaces in  this    sentence.  And another one badly added."
                        "Too much spaces in this sentence. And another one badly added."))
 ("Type composition, as exposed in Butterick's Practical Typography http://practicaltypography.com"
  ("Curly quotes"
   ("Isolated single quote transformed to an apostrophe unless at the beginning of a word"
    (text-string-test "It's okay."
                          "It&rsquo;s okay.")
    (text-string-test "What is 'that?"
                          "What is &lsquo;that?"))
   ("Forced apostrophe sequence"
    (text-string-test "Apostrophe needed in the ''90s"
                          "Apostrophe needed in the &rsquo;90s"))
   ("Opening and closing single quotes detection"
    (text-string-test "This is 'magic'!"
                          "This is &lsquo;magic&rsquo;!")
    (text-string-test "This is 'magic'! Truly 'magic'!"
                          "This is &lsquo;magic&rsquo;! Truly &lsquo;magic&rsquo;!"))
   ("Isolated double quote left as is"
    (text-string-test "It\"s not okay."
                          "It\"s not okay."))
   ("Opening and closing double quotes detection"
    (text-string-test "He said \"Hello!\"."
                          "He said &ldquo;Hello!&rdquo;.")
    (text-string-test "He said \"Hello!\". Then \"How are you?\"."
                          "He said &ldquo;Hello!&rdquo;. Then &ldquo;How are you?&rdquo;."))
   ("Both detections working together"
    (text-string-test "He said \"This is 'magic'!\"."
                          "He said &ldquo;This is &lsquo;magic&rsquo;!&rdquo;.")
    (text-string-test "He said \"This is 'magic'!\". Then he said \"Truly 'magic'!\"."
                          "He said &ldquo;This is &lsquo;magic&rsquo;!&rdquo;. Then he said &ldquo;Truly &lsquo;magic&rsquo;!&rdquo;.")))
  ("Paragraph, section, trademark and copyright symbols. With automatic non-breaking spaces, except for trademarks."
   (text-string-test "See :para 39." "See &para;&nbsp;39.")
   (text-string-test "Described in :sect 2." "Described in &sect;&nbsp;2.")
   (text-string-test "Welcome to MegaCorp:trade!" "Welcome to MegaCorp&trade;!")
   (text-string-test "Try our new SuperStuff:reg!" "Try our new SuperStuff&reg;!")
   (text-string-test ":copy John Smith" "&copy;&nbsp;John Smith")
   ("Multiple paragraph or section symbols"
    (text-string-test "See :para:para 39, 40 and 41." "See &para;&para;&nbsp;39, 40 and 41.")
    (text-string-test "Described in :sect:sect 2, 3 and 4." "Described in &sect;&sect;&nbsp;2, 3 and 4.")))
  ("Nonbreaking hyphens, automatically inserted"
   (text-string-test "The format is UTF-8." "The format is UTF&#8209;8."))
  ("Dashes, en and em"
   (text-string-test "pages 330--39" "pages 330&ndash;39")
   (text-string-test "The em dash puts a nice pause in the text---and it is underused in professional writing."
                         "The em dash puts a nice pause in the text&mdash;and it is underused in professional writing."))
  ("Ellipses"
   (text-string-test "I am not really sure..." "I am not really sure&hellip;"))
  ("Nonbreaking space"
   (text-string-test "I do want this sentence to break here:nbsp since blabla."
                         "I do want this sentence to break here&nbsp;since blabla."))
  ("Hard line break"
   (equal (html<-text-string "I force a break here:br so that the rest goes on the next line.")
          '((:p (:noescape "I force a break here" :br "so that the rest goes on the next line."))))))
 ("Tabs"
  ("Replaced by a space in sentences"
   (text-string-test (mkstr "This is " #\Tab " a sentence" #\Tab #\Tab "with " #\Tab #\Tab #\Tab "tabs.")
                         "This is a sentence with tabs."))
  ("Ignored if at the beginning of a line"
   (text-string-test (mkstr #\Tab "This is a tabulated sentence.")
                         "This is a tabulated sentence.")
   (text-string-test (mkstr #\Tab #\Tab #\Tab "This is a tabulated sentence.")
                         "This is a tabulated sentence.")))
 ("Parenthesis followed by a keyword fall through. It is expected to be HTML."
  ("Part of the current paragraph if found in it"
   (equal (html<-text-string "See this (:a :href \"my-url.html\" \"link\")")
          '((:p (:noescape "See this " (:a :href "my-url.html" "link"))))))
  ("Direct element of the document if at the beginning of a paragraph"
   (equal (html<-text-string "(:a :href \"my-url.html\" \"Testing a link\")")
         '((:a :href "my-url.html" "Testing a link"))))
  ("No impact of parentheses or double-quotes inside strings"
   (equal (html<-text-string "(:a :href \"my-url.html\" \"a great (but incomplete) link\")")
         '((:a :href "my-url.html" "a great (but incomplete) link")))
   (equal (html<-text-string "(:a :href \"my-url.html\" \"a great (but incomplete link\")")
         '((:a :href "my-url.html" "a great (but incomplete link")))
   (equal (html<-text-string "(:a :href \"my-url.html\" \"a great but incomplete) link\")")
         '((:a :href "my-url.html" "a great but incomplete) link")))
   (equal (html<-text-string "(:a :href \"my-url.html\" \"a great \\\"but incomplete\\\" link\")")
         '((:a :href "my-url.html" "a great \"but incomplete\" link")))
   (equal (html<-text-string "(:a :href \"my-url.html\" \"a great \\\"but incomplete link\")")
         '((:a :href "my-url.html" "a great \"but incomplete link")))
   (equal (html<-text-string "(:a :href \"my-url.html\" \"a great \\\"but incomplete) link\")")
         '((:a :href "my-url.html" "a great \"but incomplete) link")))))
 ("Escaping my-markdown transformation"
  ("Single and double quotes, to prevent their transformation to curly quotes. For instance to have foot and inch marks as -118° 19' 43.5\""
   ("Isolated single quote transformed to an apostrophe"
    (text-string-test "It:'s okay."
                          "It's okay.")
    (text-string-test "What is :'that?"
                          "What is 'that?"))
   ("Forced apostrophe sequence"
    (text-string-test "Get the sequence :':' back one character at a time."
                          "Get the sequence '' back one character at a time."))
   ("Opening and closing single quotes detection"
    (text-string-test "This is :'magic:'!"
                          "This is 'magic'!"))
   ("Opening and closing double quotes detection"
    (text-string-test "He said :\"Hello!:\"."
                          "He said \"Hello!\"."))
   ("Both detections working together"
    (text-string-test "He said :\"This is :'magic:'!:\"."
                          "He said \"This is 'magic'!\".")))
  ("Hyphen mark"
   (text-string-test "Bypass the automatic conversion of the hyphen :- to a nonbreaking one"
                         "Bypass the automatic conversion of the hyphen - to a nonbreaking one"))
  ("Dashes, en and em"
   (text-string-test "Get the sequence :-:- back one character at a time."
                         "Get the sequence -- back one character at a time.")
   (text-string-test "Get the sequence :-:-:- back one character at a time."
                         "Get the sequence --- back one character at a time."))
  ("Ellipses"
   (text-string-test "I am not really sure:..." "I am not really sure..."))
  ("Left parenthesis"
   (text-string-test "This is :(I am sure you know it) blabla."
                         "This is (I am sure you know it) blabla."))
  ("No escape with a colon in a word or at the end of a word or a line"
   (text-string-test "That is: blabla" "That is: blabla")
   (text-string-test "Th:at is" "Th:at is")
   (text-string-test (mkstr "That is:" #\Newline) "That is:")
   (text-string-test "That is:" "That is:")))
 ("HTML escaping. No & < >"
  (text-string-test "< and > are part of unexpected characters & co."
                        "&lt; and &gt; are part of unexpected characters &amp; co."))
 ("Gotchas"
  ("Special syntax was short-circuiting typographic double quotes"
   (text-string-test "\"Test...\"" "&ldquo;Test&hellip;&rdquo;"))))