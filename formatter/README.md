formatter
=========

In one word, I needed to formatting nicely Lisp code for my website (but now I don't anymore... ahem!).

Hmm?
----

A Lisp function to format Lisp code? Well, I could have some use for that directly. But I have in fact try, out of curiosity, to generate a clFormat Javascript function, thanks to [Parenscript](http://common-lisp.net/project/parenscript/).

And, thanks again to Parenscript, the CL tests can also be compiled to Javascript and run with [Jasmine](http://pivotal.github.io/jasmine/). Although I haven't maintained that part anymore.

Features
--------

This formatter deals with:
- nested lists of any depth
- quoted and backquoted lists
- single-line comment indicated with a semi-colon
- strings
- property lists
- the special indent case where the second element of a list is not on the same line as the first

It does custom indent for a few special operators and macros:
- defun
- defmacro
- with-open-file
- let
- progn
- lambda
- unwind-protect
- multiple-value-bind
- register-groups-bind

It is well-commented and uses the [alet-fsm](http://letoverlambda.com/index.cl/guest/chap6.html#sec_3 "Let Over Lambda - Chapter 6") about which you may want to read.

It can be compiled to Javascript thanks to Parenscript. You can found the result, non-minified, in the resources of this code system.

It is not extensible to allow special indentation for user-defined macros.

It does not handle multiple lines comments enclosed in #| |#. 