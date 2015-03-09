In one word
-----------

This my web-based Common Lisp code editor.


Display
-------

There can be one or two editors. The first one is for editing the code of modules. The second appear to the right to edit the usages associated to the action the cursor was on in the first code editor.

The command bar has an input to enter the name of the module you want to write code for. If there are problems (compile or usage problems), the modules in which these problems are located in will be listed.


Auto-complete
-------------

Both editors have auto-complete on each term composing a hyphenated symbol. For instance, here's a hyphenated symbol that is quite long to type:

	define-web-result

You may type:

	def

And then press Tab to auto-complete but that would give a lengthy list. As usual, you may want to pursue with:

	defin

Or more. But you can also pursue with:

	def-web

Or even:

	def-w-res


Commands
--------

Ctrl-1 put the focus on the input to open any module. First comes the name of the system then the name of the module. Both don't need to be complete. A pop-up will appear to list the matching options.

Ctrl-Q saves the work of both editors if need be. But in case there is nothing to save, it will open the first module in problem, displayed in the list in the command bar. Doing so enter maintenance mode.

Alt+Up/Down moves the cursor to the previous/next form.

Alt+Right opens the usage editor. The cursor must be on a defmacro/defun form and the usages will be about the action being defined by such a form.

Alt+Left closes the usage editor, wherever the cursor is.
 

Maintenance mode
----------------

First, you must solve compilation warnings or errors. Then you solve usage problems.

In both cases, the modules are listed from the most fundamental system to the most peripheral ones and all modules of a given system are also listed that way.

Once every problem has been solved, the editor will come back to its default state, on the module you were currently editing.


On save
-------

When saving some code, it gets compiled (if it is a function) or evaluated (if it is a macro). If no problem occurred at this point, associated usages are replayed.

In any case, all symbols occurring in the definition of the action are made available in the associated package for auto-complete.

When saving some usages, they get evaluated. And if both some code and some usages need to be saved, code is saved first.

I plan to refresh depending compilations and tests along these three simple recursive rules:

- an action being re-compiled/re-evaluated should provoke the recompilation/re-evaluation of all depending actions
- an action being re-compiled/re-evaluated should have its usages being re-evaluated


The associated watcher
----------------------

The watcher defined by the code systems code-editor will display the documentation generated from the edited module. And it will display the full view debugged usages of the edited usages.