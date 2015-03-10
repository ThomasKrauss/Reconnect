Run cheat sheet
===============

URLs
----

The code editor is available through the Lisp web server at: 

	http://localhost:9999/code-editor/editor


The core watcher that display the dashboard is at:

	http://localhost:8078/index.html

	
The regular watcher, associated to the editor in use through the Lisp web server is at:

	http://localhost:8079/index.html


Watcher commands
----------------

To launch the regular watcher:

	(to-web:launch-watcher)
	
	
To launch the core watcher:

	(to-web:launch-watcher :watcher-type :core)
	
	
If you experience difficulties making them work, be sure they are properly installed with:

	(to-web:install-watcher :install-node-modules t)


Documentation sample generation
-------------------------------

To have a sample of documentation generated from a module, have the regular watcher running <code>(to-web:launch-watcher)</code> and connect a browser to it at <a target="_blank" href="http://localhost:8079/index.html">http://localhost:8079/index.html</a>. Then type in the REPL:

	(librarian:refresh)

Full view debugging
-------------------

To test some full view debugging, have the regular watcher running <code>(to-web:launch-watcher)</code> and connect a browser to it at <a target="_blank" href="http://localhost:8079/index.html">http://localhost:8079/index.html</a>. Then either connect another browser/tab to <a target="_blank" href="http://localhost:9999/code-editor/editor">http://localhost:9999/code-editor/editor</a> or type in the REPL <code>(to-web:watch "code-editor")</code>.

You may then debug some code and the result will be automatically printed out in the browser connected to the watcher.

You can debug a plain perimeter like:

	(debug-some-code '(+ (* 2 3) 6) "code-editor")

Or you can debug a call to an action whose code source is available. The full view debugger will "enter" that function and play out what is happening in it.

	(debug-some-code '(no-backslashes "test\\pour\\voir") "my-utilities")

Using the code editor
---------------------

There is a bit to read so refer to the *code-editor/README.md* file. But here are the commands:

**Ctrl-1** put the focus on the input to open any module. First comes the name of the system then the name of the module. Both don't need to be complete. A pop-up will appear to list the matching options.

**Ctrl-Q** saves the work of both editors if need be. But in case there is nothing to save, it will open the first module in problem, displayed in the list in the command bar. Doing so enter maintenance mode.

**Alt+Up/Down** moves the cursor to the previous/next form.

**Alt+Right** opens the usage editor. The cursor must be on a defmacro/defun form and the usages will be about the action being defined by such a form.

**Alt+Left** closes the usage editor, wherever the cursor is.