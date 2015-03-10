from-plain-text
===============

Markdown... no, not anymore
---------------------------

In one s-expression:

	> (html<-text-string
		(mkstr "Header 1"
			   #\Newline
			   "========"
			   #\Newline
			   #\Newline
			   "Header 2"
			   #\Newline
			   "--------"
			   #\Newline
			   "One small paragraph here."
			   #\Newline
			   #\Newline
			   "Another small paragraph there."))

	((:h1 (:noescape "Header 1"))
	 (:h2 (:noescape "Header 2"))
	 (:p (:noescape "One small paragraph here."))
	 (:p (:noescape "Another small paragraph there.")))

And that's all there is. Of course, you do not use it like that. The text would be written verbatim as:

	Header 1
	========

	Header 2
	--------
	One small paragraph here.

	Another small paragraph there.


Nice typography on characters
-----------------------------

Some characters make texts nicer to read but they are quite a pain to use in pure HTML. The action html<-text-string insert them without mandating to memorize tedious things like &rsquo; or &ndash;. And in the very spirit of Markdown, you do not fill your text with such lengthy and inelegant tokens.

Here are the character or sequence of characters that are special:

'   
A single quote will yield a proper opening simple quote. If a word is written in quotes, the second will be replaced by a closing simple quote.

"    
Same thing here as with single quotes.

\-   
A hyphen will be replaced by a non-breaking hyphen.

\-\-   
Two hyphens will be replaced by a small dash, as needed when writing ranges like from page 330 to page 339 that you may want to shorten to 330--39.

\-\-\-   
Three hyphens will be replaced by a long dash, as needed to break visually a sentence with a remark.

...   
Three dots in a row will be replaced by an ellipsis.

(   
An opening parenthesis will trigger HTML insertion. You do not write classical, verbose HTML but Lisp-like HTML as in:

	(:p :class "title" "Here's a list of things")


How to escape
-------------

The escape character is the colon :. So if you really want a single quote or two hyphens or an opening parenthesis, you would write:

:'   
:\-:\-   
:(

Note that since the colon character is never used at the beginning of a word, you can just type a colon character when you actually need one (contrary to the backslash character commonly used in programming languages that you need to write twice to get one).

Special symbols and characters
------------------------------

You write special HTML characters as :char rather than writing them with the syntax &char;. It is visually less invasive and trigger on reading the same reaction as with escaping: "ah! There's something special here."

Here they are:

*:para*   
The symbol for a paragraph. A non-breaking is always included after it.

*:sect*   
The symbol for a section. A non-breaking is always included after it.

*:trade*   
The trademark symbol.

*:reg*   
The registered symbol.

*:copy*   
The copyright symbol. A non-breaking is always included after it.

*:nbsp*   
Insert a non-breaking space manually.

*:br*   
Insert a hard line break manually.