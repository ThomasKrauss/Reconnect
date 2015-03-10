my-utilities
============

I store there many of the functions and macros I use or suspect to use in all my projects.

Some are pretty classic like symb, mkstr, once-only, with-gensyms, mklist, flatten, group.

Here's a tour of the modules.

0-anaphora
----------

Basically, when the result of an action perimeter is of interest enough to come up in several forthcoming perimeters, you manually create a variable to store that result.

Anaphoric macros creates for you the variable *it* to refer to that result.

Here's a definition of anaphora, for the big picture: the use of a word as a regular grammatical substitute for a preceding word or group of words, as the use of *it* and *do* in *I know it* and *he does too*.
(WordReference Random House Unabridged Dictionary of American English © 2015)

So, for a *if*, you would manually write:

	(let ((intermediate-result (compute this with that)))
		(if intermediate-result
			(further-compute intermediate-result on that-other-thing)
			(do-default-thing)))

With the anaphoric if *aif*:

	(aif (compute this with that)
		(further-compute it on that-other-thing)
		(do-default-thing))

And that only needs one more letter on the term *if* to ripe this benefit of brevity and readability!

Of course, anaphora do not blend well together, just as in normal speech: *"Put it on the table and serve it."*. And you're like ù"Wait, what? You are talking about the salt, right?"ù Strange? Yes. Because nobody talks like that with the first "it" being the salt and the second being something else. Don't expect anaphora to work otherwise in your own code. If there's anaphoric conflicts, there's a need of clarity.

The anaphoric macro I'm using and haven't seen somewhere else is *let-a*. Most anaphoric macros are tied down to an action that is different from *let*: *aif* is a wrapper over *if*, *awhen* over *when*, etc.

*let-a* is not a wrapper, it is a pure replacement. Instead of:

	(let ((at-lost-for-meaningful-variable-name (compute ...)))
		; use at-lost-for-meaningful-variable-name everywhere
		)

You do:

	(let-a (compute ...)
		; use it everywhere
		)

It felt silly when I defined *let-a* but being relieved from having to come up with new names or use repetitive names proved itself to be great. Really. It is great. Seriously. It is AWESOME!

1-util
------

Ouch. That's the bin of all modules of my-utilities which is the bin of all code systems. I need to sort things there.

2-environment
-------------

The utilities in there are about modeling an environment with lists. They are heavily used in the full view debugger but are also helpful for parsing dependencies and evaluate usages.

Looking at the dashboard, you will notice they are almost all used by the my-lisp-parsing system so the 2-environment module ought to be there but one action is used by the librarian code system.

It's probably a beginning so I have preferred to outsource the 2-environment to my-utilities to avoid duplicated code.

3-cl-complement
---------------

I know, that name is gross. I'm sorry. It's just that I prefer the Scheme convention of suffixing predicates by a '?' rather than a 'p', because '?' is not a letter and thus it stands out.

I have just normalize thing a bit here. I mean, you have to know that:

- fboundp
	*=> pure boolean, identify an action*
- macro-function
	*=> nil or function, identify a macro (in fact)*
- special-operator-p
	*=> nil or a list of all succeeding special-operators, in alphabetical order, starting with the special operator you have passed*

For my code, it is easy: if the symbol you passed is indeed what it is you want, it falls through. Like:

	> (cl-action? '+)
	+

And you then only need to remember the action names:
- cl-action?
- cl-macro?
- cl-function?
- cl-special-action?

This module also have the following self-explaining actions:
- cl-self-evaluating? (sexpr)
- get-action-lambda-list (sexpr)
- get-action-body (sexpr)

4-boolean
---------

A lot of utilities on operations about/on booleans. Most of them are used in the definition of usages. There are a lot of diff operations defined here but I haven't started using them. I intend them to fit in full view debugging to better explain what precisely went wrong when an obtained value isn't the expected value.

5-fad
-----

The abbreviation *fad* stands for Files And Directories. The utilities in there complement what is provided by the great cl-fad library.

The dashboard may tell you they are unused but I have some end code systems that do use these utilities. It's just that they are not part of the Reconnect project per se.

And that tell you the dependency parser should be improved on that point to take into account systems that are not loaded in the image (because they are not available or because they are not needed per se).

*Call for help*: action in Lisp may contain nearly any character possible and that includes some that are not accepted by outside programs. Namely Windows for me. But usage files are named after an action name. So I have used a simple but awful strategy.

I have a list of forbidden characters and when I spot them in the name of an action, I replaced them by a tilde followed by the index of the offending character in my list.

Using a tilde is safe as long as it isn't used in an action name. Indexes are safe as long as the new elements are only appended to the list. And because I coded like a pig, the indexes cannot go over 9.

6-plist
-------

I work a lot with plists, never with alists. I thus have a need for quite some utilities of such a data structure.

7-queue
-------

A queue implementation from Paradigms of Artificial Intelligence Programming by Peter Norvig. Currently used only for graphs, in depth or breadth traversal.

8-json
------

Utilities to handle JSON on top of the reading and parsing function provided by ST-JSON. Basically, since I used plists a lot, I must have a way to translate them to JSON. And build them back from reading JSON.