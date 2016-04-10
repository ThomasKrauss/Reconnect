The Reconnect project
=====================

This is the Reconnect project, aiming to better support myself during programming as my hobby.

As of... a bit of time, I don't pursue this project anymore.
Some limitations clearly appeared to me and the most fundamental ones revolved around... the core language itself.
Thus my next project, Evolutionary, is about building a Lisp language  to adress those core issues.
It will probably diverge in some hopefully interesting direction.


The Reconnect project has the following things:
- a little library for defining code systems
- a dependency parser for Common Lisp code
- a full view debugger for Common Lisp code
- a HTML code generation library
- a very raw set of actions to specify how to build something for use through web browsers
- a library for HTML code generation from a flow of text (i.e. an extremely minimal Markdown)
- a text parser to output text with nice typography for HTML
- an in-browser maintenance-oriented code editor
- a watcher server pushing data to connected web browsers for automatic updates
- a very raw documentation generator
- a bunch of web components in need of cleaning and organization
- an overview dashboard and a result view, both automatically refreshed in real time
- a graph library
- a hierarchical cache library

For a quick overview of these features with some illustrative snapshots, please read <a href="http://www.vagrant-coder.com/reconnect/english/Results.html">this</a>.

It is still very young and is far from being either whole or usable. This is not surprising because the Reconnect project aims to connect again things that were connected.

We know all too well that computers can give us results they are not much able to help us explore. One way to fix the issue is obviously to make them more knowledgeable about the results themselves.

But since I am a programmer, I have been more interested in making them explore how we have arrived to such results. While this perspective may feel loony, it's actually distressing to notice how much code we have and how few ways to look at it we use. Despite code is a web of connections of all kinds: logical, structural, organized in a timely fashion, etc.

Since I am a programmer, I witness these connections on a daily basis. They exist as without them programs could not run. But somehow they get lost. And I know damn well what we are doing systematically then: we *manually* reconnect things. In our head, in documentation, in tutorials, in tests, in frameworks, in methodologies, you name it.

Every time, it's about asking more from us, not from the computers.

Therefore one of my basic moves was to not do that if I could have asked the computer to reconnect things. And I have found out that it goes to a so fundamental level that the Reconnect project still has deep shortcomings on basic code editing tasks.

However note that it is only about 8500 lines of Common Lisp code. And that's counting all the duplicates in the my-graphs system which is currently under heavy refactoring. The Javascript code is a not a monster either (if you ignore the various libraries used I didn't code), thanks to the use of <a href="http://facebook.github.io/react/index.html">React</a>. But it's quite a mess for now.

Setup
-----

This project has been developed on Windows, under <a href="http://www.lispworks.com/downloads/">the Personal Edition of LispWorks</a>. Even if only a tiny and peripheral bit of code is limited to work on this implementation, I would advice you to use it.

You may work on another OS without much difficulty (unchecked claim!). There's only one part of code related to Windows and it's the tiny and peripheral bit of code. What's more, that code only helps setting the PATH variable on Windows, something you can do by hand on the OS of your choice. To be more precise, it checks if the various external needed programs are available on the command line regardless of where they are installed.

The needed programs are:
- dot (from <a href="http://www.graphviz.org/">Graphviz</a>)
- <a href="http://nodejs.org/">NodeJS</a>
- Opera
- Chrome

Only the first two are absolutely required because I have delegated to Graphviz the layout of graphs and to NodeJS the task of watching changes in files and notify them to browsers.

A bit of a warning though.

The dependency parser has a list of packages it should not care about and these are all internal LispWorks packages. While the parser should work fine in other implementations (unchecked claim again), it will include extraneous information depending on how your implementation deals with various macros (like defun, defmacro, etc).

And they may be a lot of them which will result, in such a case, in really cluttered graphs.

To ignore these information, add the package names to ignore in *my-lisp-parsing/0-parse-forms.lisp*, in the default values of the *:ignore-packages* argument in functions *parse-module-actions* and *filter-symbols*.

Also note I work with three screens. Opera is opened on the left, Chrome on the right, Firefox on the middle. It is still experimental and not knowing where I go from there, I just keep it simple. So it is very easy to adapt to your own configuration.

Take a look at the *initialize-work-environment* in *overview/0-main.lisp*. Just setup where (and if) you want to open the dashboard view and the result view. For Firefox, as of now, I only have a bookmark to my code editor <a target="_blank" href="http://localhost:9999/code-editor/editor">http://localhost:9999/code-editor/editor</a>.

If I use three different browsers, it's because on Windows they are a bit of pain to configure on the command line. I wanted only one of them and open several windows where I wanted. But browsers are a bit deaf to command line arguments on Windows (it seems better on Mac OS and Linux but please do check by yourself what is available). So, long story short, I instead installed three of them and rely on the last position occupied since they reopen there.

More details
------------

For the detailed installation informations, check the file Install-instructions.md

For a summary of all things you may want to test, check the file Run-cheat-sheet.md

Vocabulary
----------

There's only a few terms, honest.

A *system of code* (or code system) is a folder containing source files. It defines one main package with the same as the folder name and has a ASDF system definition file.

A *module of code* is a source file.

A *usage* is a test. The term 'test' is pretty general and fuzzy. But it is safe to say it is all about verifications. And for the Reconnect project, the only verifications I code are how I intend the code to be used for a precise result.

To be honest
------------

I have only relied on my code editor to write tests, as it is more convenient to use than manually creating the test file in the appropriate folder for each action. Plus, it evaluates them automatically on save.

I have used a bit my full view debugger (see <a href="http://www.vagrant-coder.com/files/full-view-debug.html">here</a> for examples) and while it does lead quickly to where errors happened and to what resulted in what, it is still a bit raw (and buggy on code that works by side-effects).

I have used quite a bit my dependency graphs to identify unused parts of code. It is great to better understand the impacts of changes. But my code system about graphs is messy to use, which is why it is being refactored right now.

Since the code is still largely undocumented (but that's a part I'm working on now too!), if you want to know more about the ideas about the project, you may found interesting things in <a href="http://www.vagrant-coder.com/articles/english/Built-with,-powered-by.html">Built with, powered by</a> or in <a href="http://www.vagrant-coder.com/articles/english/The-steps-of-my-sabbatical.html">The steps of my sabbatical</a>.

The mission
-----------

You can find the detailed mission statement <a href="http://www.vagrant-coder.com/reconnect/english/The-mission.html">here</a>.

It is of course heavily geared towards my own idiosyncrasies. But it gives a sense of where this project is heading. Although I have not done much to make the various things I talk about a reality.

In particular, I just release the various code systems in a one whole package despite each one of them being an ASDF system. I did try a year ago to make a repository for each one of them but that was a lot of work. And the result felt disconnected and a burden of maintenance.

I know it is not the proper to share code systems but I prefer to deal with these issues while grounded in concretely sharing things rather than design in an ivory tower. I'm sorry for how inconvenient and demanding it is to install this project. I tried to lower it to the best of my ability.

What's going on now
-------------------

With a working full view debugger and a code editor that is minimal yet enough for basic needs, I will develop tests for the various code systems. Along the way I hope to develop diff methods (on lists, property lists and code itself) to further improve the explicative power of full view debugging.

I will also work on the documentation of code and a bit of refactoring may occur here and there (especially on the awkward definition of hierarchical caches). Along the way I hope to normalize and better structure the terms used in arguments of functions and macros so as to use them in the documentation as well as in studies of code pieces in relation to what they use. Which will probably yield graphical explanations of data flows and transformations.

And I will now switch from <a href="http://facebook.github.io/react/index.html">React</a> to <a href="http://lhorie.github.io/mithril/">Mithril</a>. Mithril is way tinier and one day, I will replace it. That will probably be easier to do than if I use React. In the process, I will also ditched JQuery and JQueryUI.

I'm also more pleased with the Mithril approach and it feels like that generating and parsing Javascript done for Mithril will yield less cluttered information that doing that on Javascript done for React. Along the way, I will strengthen the edition and analysis of code for the Web (html + css + js) and better handle what is now a repository of components.

That's it?
----------

No.

In each code system, I also have included a Readme to describe what the particular code system you're in is about, and how to tinker with it when possible. So go check them for more concrete documentation to help you explore the Reconnect project.

Finally... thank you very much for your time!

Acknowledgments
---------------

I feel like all Lispers past and present have helped this project even if the link may seem pretty tenuous.

In a more direct fashion and on Lisp matters, I am confident to say that, in order of discovery, the following people have helped a lot: Paul Graham, Adam Tornhill, Peter Seibel, Edmund Weitz, all of you behind the Parenscript project: Manuel Odendahl, Edward Marco Baringer, Vladimir Sedach and Travis Cross. Then there is also Xah Lee, Douglas Hoyte, Peter Norvig, Marijn Haverbeke and Guy Steele Jr.

Thanks to you all for making programming a better and more thoughtful experience for me!

It's almost certain I have forgotten a few people. I'm sorry for this.

Lastly, I have found out the name of the Reconnect project when, during yet another fruitless bit of thinking about that precise subject, I just looked at the name of <a href="https://grmnygrmny.bandcamp.com/album/reconnect">the music I was listening to</a>. Thank you very much, Germany Germany!