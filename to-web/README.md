to-web
======

This system is my-utilities sibling: a bin for all kind of utilities except they really have to do with web display.

1-from-plain-text
-----------------

I like <a href="http://daringfireball.net/projects/markdown/syntax">Markdown</a> but in fact I don't use it very much. I always forget the syntax for links and images and find it a bit unclear on many cases. At one point, I read about some <a href="http://practicaltypography.com/">practical typography advices</a> and it hardened a perspective whose germ was already there.

*1-from-plain-text* is my way of having some HTML code from plain text. I ditch almost everything from Markdown. In fact, there's only left the identification of headers and of paragraphs. Nothing else. Because with Markdown, I found it way too easy to italicize words and too tempting to make a list (<a href="http://www.edwardtufte.com/tufte/books_pp">PowerPoint lurking!</a>). And too hard to do anything else.

I got back to the fundamental which is just about capturing the flow of a text with its proper separation in sections. And I made it easier to have nice typography. To keep the ReadMe short, I have put the documentation in the *doc/* folder.

A note: *1-from-plain-text* is actually producing s-expressions that directly fit my HTML production code but there's little need for that. I plan to dissociate these two things so that I can head toward writing code files in which I just pour text and code together. My primary targets are usages and the documentation that follows.

2-html-engine
-------------

This engine has been purely coded from the one Peter Seibel <a href="http://www.gigamonkeys.com/book/practical-an-html-generation-library-the-interpreter.html">presented</a> in his book Practical Common Lisp.

I just coded it without the pretty printer and without any CLOS related code. Again, to keep this file short, please refer to the documentation in the *doc/* folder.

3-html-macros
-------------

Here's why I have choose to have my own HTML production engine over the great library <a href="http://weitz.de/cl-who/">cl-who</a>: I wanted custom tags. Especially to have the hand on CSS and JS code introduction.

Please refer to the HTML documentation in the *doc/* folder.

5-watcher-server
----------------

Each system can build a NodeJS webapps separated from the Common Lisp image. I'm currently using [Inertia](https://npmjs.org/package/inertia) to serve the static resources, [watch](https://npmjs.org/package/watch) to be notified of any file changes and [Socket.IO](http://socket.io/) to forward these notifications on the webapp.

It's not real time but close enough for the low amount of code it requires.

This module has all the Parenscript code needed to generate the watcher server and the various utilities needed for it.

6-web-results & 7-web-editors
-----------------------------

These two modules helps to define web products. The awkward name of 'results' denotes the watcher web applications which are totally reactive and not at all interactive. There's supposed to be at most one per code system. Interactive web applications are handled by the 7-web-editors module.

Here's an example of usage, from the code system my-site. Basically, it details how my site is built:

	(define-web-result ("my-site" :static-site)
	  :font (copy-file "Anonymous-Pro.ttf"
					   "Charter-Regular.ttf"
					   "Charter-Bold.ttf"
					   "Charter-Italic.ttf"
					   "fontawesome-webfont.ttf")
	  :icon (copy-file "disk.png"
					   "external-link.png"
					   "mail-send.png")
	  :image (copy-file "configuration-comparison.png"
						"dashboard-delivery.png"
						"system-graph.png"
						"module-graph.png"
						"action-graph.png"
						"code-editor.png"
						"maintenance-messages.png"
						"maintenance-messages2.png"
						"supervision-view.png"
						"supervision-system-view.png"
						"results-view.png"
						"close.png"
						"loading.gif"
						"prev.png"
						"next.png")
	  :file (copy-file "full-view-debug.html"
					   "documentation.html"
					   "résumé-thomas-krauss.pdf"
					   "cv-thomas-krauss.pdf")
	  :html (make-index-page :map-wrapped (list-article-languages))
	  :html (make-about-page :map-wrapped (list-article-languages))
	  :image (generate-photo-from-original :map-wrapped (list-photo-names))
	  :html (make-article-page
			 :map-wrapped (let ((*article-base* *article-directory-name*))
							(list-article-names (list-article-languages))))
	  :html (make-reconnect-page
			 :map-wrapped (let ((*article-base* *reconnect-directory-name*))
							(list-article-names (list-article-languages)))))

The *define-web-result* and *define-web-editors* also allow the keywords :css and :js. You don't see any of them in the definition of how my website is built because the CSS and JS files are, of course, declared in the various HTML pages and since the HTML production goes through my HTML engine, the needed files are automatically included.

I intend to further this capability of my HTML engine to images and links, whether they are declared in HTML or CSS code. At that point, the above definition would then be:

	(define-web-result ("my-site" :static-site)
	  :html (make-index-page :map-wrapped (list-article-languages))
	  :html (make-about-page :map-wrapped (list-article-languages))
	  :image (generate-photo-from-original :map-wrapped (list-photo-names))
	  :html (make-article-page
			 :map-wrapped (let ((*article-base* *article-directory-name*))
							(list-article-names (list-article-languages))))
	  :html (make-reconnect-page
			 :map-wrapped (let ((*article-base* *reconnect-directory-name*))
							(list-article-names (list-article-languages)))))

8-html-templates
----------------

The *base-page* macro defines a basic template. What a shock. The html, head and body are handled there. As per the prescriptions of <a href="http://yslow.org/">YSlow</a>, the CSS files are in the head tag and the JS files are at the bottom, in the body tag.

The *base-page* also includes the needed JS files for watcher pages. There are two of them: socket.io.js and refresh.js. The first to open a WebSocket to the server, the second to automatically refresh the page and scroll it to its last position in case the URL did not change.