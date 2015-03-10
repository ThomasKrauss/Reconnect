full-view-debug
===============

Full view debugging is computing automatically all intermediate results of a given code so that no interaction is required on the programmer's part. It can also locate where errors happened and place emphasis on them for you to find the location in a glance and move on from there to solve the problem.

An interesting use of full view debugging is to apply it to usages that succeed. Because that actually generates a proper and up-to-date documentation of how to use the code.

Testing it
----------

To test full view debugging, use the *debug-some-code* function of the **code-editor** system.

Be sure to have a tab opened at the web-editor associated to the code-editor at this URL <a target="_blank" href="http://localhost:9999/code-editor/editor">http://localhost:9999/code-editor/editor</a> so that the associated watcher is activated. That way, you will see the properly laid out debugged result be automatically pushed to the browser connected to the watcher (URL: <a target="_blank" href="http://localhost:8079/index.html">http://localhost:8079/index.html</a>).

As an alternative, have a browser connected to the watcher and type in the REPL:

	(to-web:watch "code-editor").

You can debug a plain perimeter like:

	(debug-some-code '(+ (* 2 3) 6) "code-editor")

Or you can debug a call to an action whose code source is available. The full view debugger will "enter" that function and play out what is happening in it.

	(debug-some-code '(no-backslashes "test\\pour\\voir") "my-utilities")