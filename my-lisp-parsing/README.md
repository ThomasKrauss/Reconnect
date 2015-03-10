my-lisp-parsing
===============

0-parse-forms
-------------

Parsing Common Lisp code for dependencies.

Functions are run-time actions, macros are install-time actions. So the parser outputs two results: the used install-time actions and the used run-time actions.

There's still a bit of work to be done.

Namely, parsing functions body is okay because it's simple once you see things as described above. Allow me to elaborate. I initially tried to reason about what is at the crossing of *install / run*-time and actions *relied on /provided by*.

In the case of functions using only functions, it is simple and straightforward. The install-time is empty, whether you are looking at what is being relied on or what is being provided by. Because everything happens at run-time for functions. What's more, any function only provides itself. Therefore, a function using only functions relies on exactly the actions one reads in its body. And there's only one whole feature being provided: the one that was read.

In other words, this is **a model of pure delegation**.

The nice trait is that it is transitive. At depth zero, a function relies exactly on the functions written in its body. At depth one, it is the union of all the depth-zero dependency sets of every function it uses. And so on.

Macros felt a bit more complicated at first glance but it was obvious they only provides themselves at install-time and rely on nothing at runtime since they are not even there. But when studying more precisely functions using macros, macros using functions, or macros using macros, two serious problems rose up.

*Macros are way more complicated than I thought.*

The smallest problem of the two was to try to get what a macro relied on at install-time (and only then, since a macro is not present at run-time). The answer is that the parser needs to identify the actions that are being executed to compute the macro expansion. A function or a macro that gets used at install-time is to be thought as being relied on.

While this task did not seem easy (especially when backquote expansion is being performed at read-time: how to even get how the code was actually written?), it felt good to have an answer. And that particular answer led me to think that what a macro provides for the run-time is the actions that are not executed during macro expansion. Upon reading all the written actions of a macro, I thought I just needed to discriminate between the ones being used right away from the ones that are not.

In other words, I thought, as is commonly presented, that macros followed a model of replacement. And that I only needed to parse the particular arguments of a macro when it is used to say that what it provides for the run-time are:
- the functions I read in the arguments
- plus the functions I read in its body and that are not executed during macro expansion.

But every case (functions using macros, macros using functions and macros using macros) led me to the same conclusion: I need to parse the full expansion *every single time*. There is no other way around. Because macros do not follow a nice model of replacement, in the sense that you can determine what is what just by reading code and then obtain a nice scaling-up, transitive model.

**Macros follow a model of replacement wide opened on enrichment and filtering.**

A macro can expand into vastly different codes. It can filter and rework the argument being passed, even ignore the particular actions written in them. It can enrich them or enrich the written code in its body. Through for instance functions that outputs code. And even properly parsing these kind of functions isn't bringing anything to the table when it is time to decide what is being provided.

It has taken me time to conclude that, while it is interesting to view macros as install-time facilities, some properties of the model of pure delegation just do not fit them. But they fit in it once you compute their expansion. Which is what I did when parsing functions.

As for parsing macro bodies, the answer is that it makes sense to identify dependencies it relies on (which still needs to be done) but it doesn't make any sense in term of what a macro provides. From there, two possibilities. Either we try to fix macros, since it is an awful material that is off our formal views. Or we ditch our formal views, acknowledging it is another form of programming we just have in our hands.

I chose the latter because it fits nicely in my goal of understanding whole code systems and in dissociating how and with that they are built, from what is powering them.


1-count-lines-and-forms
-----------------------

Parsing Common Lisp code for counting lines and definitions.

In definitions, the documentation string, the comments and the blank lines are taken into account. But when these are outside definitions, they are ignored.

Defines the *:line-and-form-counts* hierarchical cache.


2-compile-forms
---------------

How to compile/evaluate a given form.

Defines the *:compile-problems* hierarchical cache.


3-dependency-graphs
-------------------

Defines the basic operations performed on dependency graphs.

A lot of code is very similar or even duplicated. This, in part, has triggered the refactoring of the code system **my-graphs**.


4-consolidate-dependencies
--------------------------

Once dependencies have been computed, a second pass is performed to tag the various nodes and to compute statistics about modules and systems.

See the documentation of *make-action-tagger* to have a description of tags associated to actions, of *make-module-tagger* to have a description of tags associated to modules and of *make-system-tagger* to have a description of tags associated to systems.


5-parse-sources
---------------

Defines the *:dependencies* hierarchical cache.


6-package-symbols
-----------------

Defines the *:package-symbols* hierarchical cache, which holds all of the symbols available in each system.


7-usages
--------

How to save, load, evaluate usages.

Defines the *:usages* hierarchical cache.