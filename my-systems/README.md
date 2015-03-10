my-systems
==========

The my-systems code system define that is actually a code system. Basically, it is a folder with several source files.

So with my-systems, I have a way to locate the sources, locate the associated usages and read and print according to a system. Because a system is tied to exactly one package to provide its actions.

I rely on ASDF to load dependencies.

Vocabulary
----------

*Module* means *a source file*. It is expected that a module groups actions operating under a precise theme, which is generally of Lisp code systems are laid out.

*Usage* means *test*. Why a new word? Well, it is actually more precise than just "test". We often find the expressions *unit tests*, *integration tests* or *testing methodologies*. And so on. It is not that we are lacking examples and precise definitions; there are whole books on these subjects. But boy is it convoluted. Dip a toe, dip the entire leg. Why can't it be simple to test when the tested action is simple?

When I thought about associating tests to my source code, a very natural question arose: what's the first tests I could write?

I did answer the question. Interestingly, the answer had an unforeseen trait. I found out I could only write about how I intended the code to be used. Hence the term *usages*. From there, several things can be deduced.

Usages can form, with the documentation string, a pretty good documentation of the code. Because they are examples of how to use the code.

And at one point, the code is really used so it feels pretty natural to check if the use satisfies the need. Since any Lisp code can be read easily, I think such real uses can be automatically incorporated as usages and then checked. But I haven't done any work in that direction for now.

Last but not least, if you stumble on something that you have difficulty to provide usages of, it may be that it is not much usable per se. Then the world is wide open, from *Why bother providing usages?* to *Maybe I should think again my code?*. You are the judge.

Here is a summary of the various modules.

0-layout
--------

Defines the various directories that are expected to accompany a code system, either in it or in the basic directory of the Reconnect project.

1-read-and-print
----------------

How to read and print s-expressions from the perspective of a given code system.

The important notion here is the one of a code chunk. When you have a string, there is no way to determine in advance how many s-expressions there is in it. So reading a code chunk always returns a list of s-expressions, even if there was only one in the code chunk.

2-load-and-save
---------------

How to save and load code from the various modules of a system.

3-management
------------

Things like create-system, delete-system and so on. ABSOLUTELY NOT MATURE AT ALL.

The only working part is *create-system*.

I have done mostly things by hand. I only have 20 systems in total and most of them are under-developed end systems. Non-end systems are all part of the Reconnect project and have required various and precise actions on my part that felt not worth trying to generalize and program about since they were so narrow.

4-documentation
---------------

Only deal with the documentation associated to a symbol. I'm still on a very early phase on my work on system and module documentation (which is why I provided all manually written Readme files).

5-backup
--------

Well, pretty simple. I backup my data on the cloud and on USB keys. This module contains the code listing all the folders that needs backup.

6-external-programs
-------------------

This modules helps defining commands that call an external programs. It also contains Windows and LispWorks specific code to help update the PATH variable automatically.

Commands are called by their name only because I don't want the Reconnect project to have the exact knowledge of the installation path per external program.

The code of this module allows to configure the work environment without the need to restart of Windows or of LispWorks.

7-wrap-up
---------

Define the external program to Java. The code about external programs knows how to deal with JAR files and that requires a working installation of Java.

Note: I know about eval-when. So I could put the 7-wrap-up's code in 6-external-programs module. But I feel in such case it is just a installation hack. In the spirit of <a href="http://www.vagrant-coder.com/articles/english/Built-with,-powered-by.html">Built with, powered by</a> and in the absence of proper tool to build, maintain and organize code systems (Reconnect project included), I prefer to have a separate module.