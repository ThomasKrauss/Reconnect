HTML production
===============

The base
--------

Long story short:

	> (with-html-output-to-string (:prologue nil)
		 (html
		   (:p :class "title" "Here's a list of things")
		   (:ul
			(dolist (item '("foo" "bar" "baz"))
			  (html
				(:li item))))))

	"<p class='title'>Here's a list of things</p><ul><li>foo</li><li>bar</li><li>baz</li></ul>"

While the basic inner codes are pretty straightforward, there are two things to understand here:

1. There's a dissociation between the code production wrapper *with-html-output-to-string* and the code production action itself *html*.
2. Every time you do not use a keyword as the first element of a perimeter, it is assume you use code. You then must use again the html action to produce HTML.

Which you can see when we produce the unordered list of items in the code above. As soon as we used dolist, we have returned to plain code, no special notation needed. As soon as we needed to produce again HTML code, we write html again. And then we can write <code>(:li item)</code>.

HTML code production wrappers
-----------------------------

The wrappers direct the HTML code produced to where you need. When you look at their signature, they are pretty much alike:

	with-html-output ((stream &key prologue) &body body)
	
	with-html-output-to-string ((&key prologue) &body body)
	
	with-html-output-to-file ((file &key (prologue t)) &body body)

The most general one is with-html-output and associates the stream you give to the HTML output. All the produced HTML code will be written to that stream.

The wrapper *with-html-output-to-string* is in fact mounted on top of *with-html-output* and passes an output-string-stream to it so that in the end you have the produced HTML code in a string.

The wrapper *with-html-output-to-file* is also mounted on top of *with-html-output* but the produced HTML code will be written to a file in UTF-8 encoding.

The *:prologue* argument is true by default only for this last wrapper because it is expected that when you write HTML code in a file, it will be a HTML file. The default value for the prologue is the doctype for HTML5:

	<!DOCTYPE html>

If *:prologue* is a string however, that value will be used instead.


HTML production functions
-------------------------

The dissociation between the wrapper and the actual action to produce HTML code allows to define HTML production functions that are not tied to a particular place (string, file, socket, ...). In other words, by switching wrappers, you redirect the whole produced HTML code to another place without any manipulation on the underlying HTML production functions.

Such functions have the following basic form:

	(defun my-html-helper-function (args)
		...
		(html
			...))

So, to produce the above HTML unordered list, we could have defined the following function:

	(defun my-unordered-list (items)
	  (html
		(:ul
		 (dolist (item items)
		   (html
			 (:li item))))))

And then:

	> (with-html-output-to-string (:prologue nil)
		 (html
		   (:p :class "title" "Here's a list of things")
		   (my-unordered-list '("foo" "bar" "baz"))))

	"<p class='title'>Here's a list of things</p><ul><li>foo</li><li>bar</li><li>baz</li></ul>"

Only the top-most functions need to use a wrapper. All the other helper functions just use the html action. In practice, it is common to define a template with a macro that will install the wrapper in your code and to only care then about the html action. Such a macro can be, for example:

	(defmacro basic-page ((title) &body body)
	  `(with-html-output-to-file (,file)
		 (html
		   (:html :xmlns "http://www.w3.org/1999/xhtml"  :xml\:lang "en" :lang "en"
			(:head 
			 (:meta :http-equiv "Content-Type" :content "text/html;charset=utf-8")
			 (:title ,title))
			(:body
			 ,@body)))))

And then you may code this function:

	(defun my-test-page (data)
	  (basic-page ("Test page")
		(:p "Let's test HTML production")
		(helper-function-to-process data)))


The automatic goodies
---------------------

Text, either for a given node or for an attribute, is automatically escaped, as seen in:

	> (... (:p "Escape the following characters: < & >"))

	"<p>Escape the following characters: &lt; &amp; &gt;</p>"

And in:

	> (... (:p :class "Escape < & > \" '" "The text."))

	"<p class='Escape &lt; &amp; &gt; &quot; &apos;'>The text.</p>"

The tags that can be closed quickly are known. These are: area, base, br, col, hr, img, input, link, meta and param. As in:

	> (... (:area))

	"<area />"


Some things to pay attention to
-------------------------------

The general form for emitting HTML is

	(tag attributes body)

where tag is a keyword, attributes is a plist and body is a collection of strings, atom, general form for emitting HTML or even codes.

An attribute name can't be computed on the fly. It needs to be known otherwise the html action will consider, upon parsing the code you have written, that the information about attributes have ended and that the rest of information is therefore the body.

So, for instance:

	> (... (:p (as-keyword "class") "title" "Here's a list of things"))

	"<p>titleHere's a list of things</p>"

The rule about switching back to code over producing HTML takes precedence for the perimeter *as-keyword*. As is expected, since this code does not produce HTML, nothing happens. It's like it wasn't even there.

Thus the following fact to keep in mind: if a code perimeter is used in the body of a node N, it is expected by default that this code will produce HTML code. And that code will be a child node of the node N. If you want to deal with the text node of the node N instead of a regular child node, you must use the special tag *:print*. As in:

	> (... (:p (:print (as-keyword "class")) "title" "Here's a list of things"))

	"<p>CLASStitleHere's a list of things</p>"

A simpler example:

	> (... (:p (:print (concatenate 'string "foo" "bar" "baz"))))

	"<p>foobarbaz</p>"

If you recall the first example, :print was not necessary to use for atoms. So, to sum up:
- when you use strings and atoms in the body of a node N, the text node of N is automatically the target. Even if the atom does not evaluate to a string.
- when you use another general form for emitting HTML or code in the body of a node, a child for N will be added, if that child is actually produced. And if your code produces a string and no HTML, it will simply have no effect.


And attributes?
---------------

Attributes work fine, as in:

	> (... (:p :class (concatenate 'string "foo" "bar" "baz") "test"))

	"<p class='foobarbaz'>test</p>

The reason is that it is easy to identify what is information about attributes since they form a plist. And then there is only one option for an attribute value: it is necessarily a string in the end.


A tour of the special operators
-------------------------------

Special operators allow you to twist the basic rules of HTML production presented above. For instance, *:print* is a special operator. Here are some examples:

**Operator** :noescape

	> (... (:p (:noescape "Don't escape the following characters: < & >")))
	
	"<p>Escape the following characters: < & ></p>"

	> (... (:p :class (:noescape "Don't escape < & > \" '") "The text."))
	
	"<p class='Escape < & > \" ''>The text.</p>"

Of course, using *:noescape* when escaping would have help you to emit valid HTML is asking for trouble...


**Operator** :attribute

	> (... (:p (:attribute "Escape \" '")))
	
	"<p>Escape &quot; &apos;</p>"

This example is stupid because *:attribute* have no real use in the interpreter per se, since escaping is enabled by default. The need filled by *:attribute* is when you want to define a function to produce a string intended to be an attribute value. With *:attribute*, you can ask for the proper escaping.


**Operator** :print

	> (... (:p (:print (+ 1 2))))
	
	"<p>3</p>"

With *:print*, you indicates you want to target the text node of the node you currently writing content for.


**Operator** :format

	> (... (:p (:format "Value: ~(~a~)" :test)))

	"<p>Value: test</p>"

With *:format*, you also target the text node of the node you are currently writing content for. But *:print* just... well, prints the value according to basic rule of formatting of Common Lisp. Since it may not be what you want (for instance, symbol names are in uppercase), you can always do the following:

	> (... (:p (:print (format "Value: ~(~a~)" :test))))

	"<p>Value: test</p>"

The operator *:format* is just a shortcut for that pattern.


**Operator** :progn

	> (... (:p (:progn "foo" "bar" "baz")))

	"<p>foobarbaz</p>"

Yes, you could have written:

	> (... (:p "foo" "bar" "baz"))

	"<p>foobarbaz</p>"

This special operator is of no real use in hand-written HTML production code. But if you are writing HTML macros, it may be handy.

HTML macros
-----------

There are 5 HTML macros defined in the code system to-web:

	:css
	:js
	:com
	:insert
	:a!

The first two, *:css* and *:js*, are shortcuts to declare the use of CSS and JS files in a HTML document. By using these macros, the names of the required files are known and can then be automatically made available when building web-results and web-editors.

The macro *:com* is a shortcut to declare the use of the JS Ajax communication file that contains the generated Ajax calls that allows the web application to have out-of-the-box the appropriate JS function that will call the associated CL function.

The macro *:insert* is intended to insert a string containing HTML production code so that HTML is really produced upon reading that string instead of just treating it as a pure value to write verbatim.

The macro *:a!* is just a shortcut allowing to avoid writing all these target="_blank" in hyper-links to ensure a click on them open a new tab rather than change the current page.