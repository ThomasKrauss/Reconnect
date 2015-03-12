The installation is in separated in four simple and straightforward steps. Then you may create a project of your own.

1. Set up
2. Install some libraries
3. Install some external programs
4. How to launch the Reconnect project after installation
5. Create a test project


1. Set up
---------

All files and directories will live in the basic directory *c:\home\lisp\*

- Create the *home\* directory.
	Then in it the *lisp\* directory.
	Then in that last one a *dev\* directory.
   
- Copy the Reconnect project in *c:\home\lisp\dev\*

- Download and install the <a target="_blank" href="http://www.lispworks.com/products/lispworks.html#personal">LispWorks Personal Edition</a> from the LispWorks site.
	(you will need to provide some information including email. Note that I never have been spammed or whatever by LispWorks.)

Note: on changing the basic directory
-------------------------------------

If *c:\home\lisp\* doesn't suit you, you can totally change it. However you must modify the following files located at the root of the Reconnect project:

- start.lisp, modify the <code>*basedir*</code> variable to your own basic directory
- install.lisp, same modification

Additionnally, you may want to change the script automating the launch of the Reconnect project too. Its name is *start-dev-environment.vbs* and it is located in *my-utilities\resources\commands\*

Modify the line n°19. The function *make-pathname* is used twice and you may modify the host argument as well as the directory argument in both cases.

The first pathname defines the file output-from-start-script.txt which is aimed to reside in the basic directory *c:\home\lisp\* and in which all the information of initialization is written to avoid cluttering the REPL. The second locates the start.lisp file.

2. Install some libraries
-------------------------

Quicklisp is a library manager that will tremendously simplify downloading and installing Common Lisp libraries and, as importantly, their dependencies!

- As of March 2014, Quicklisp is packed as one file available <a target="_blank" href="http://beta.quicklisp.org/quicklisp.lisp">here</a>.
	Save this file to the basic directory *c:\home\lisp\*

- Start LispWorks
	Do 'File > Load...'
	Choose the 'install.lisp' file in the *c:\home\lisp\dev\* directory

- Close LispWorks when the installation is done

As a result, all the external libraries required by the Reconnect project are properly installed. Here are a short description of them.

- trivial-utf-8
	To handle UTF-8 encoding.

- hunchentoot
	A great Common Lisp web-server.

- cl-fad
	"fad" for _F_ile _A_nd _D_irectories, a library of useful and portable utilities.

- cl-ppcre
	A very cool library to do regular expressions.

- parenscript
	A Javascript code generation library. Super cool.

- st-json
	To handle the JSON format, switching (pretty) seamlessly from Lisp data to JSON data and vice-versa.

- drakma
	To handle HTTP communications.


3. Install some external programs
---------------------------------

The following programs must be installed:
- one web browser
	Preferably Firefox or Opera or Google Chrome, because their command are already defined in the Reconnect project. But it does not take much to define one: feel free to use any other browser.
- Graphviz
- NodeJS

They all must be available directly from the command line. But some Reconnect code will help you update the PATH variable of both your computer and the LispWorks environment.

In *c:\home\lisp\dev\my-utilities\resources\commands\*, there is a script file called *start-dev-environment*. By executing it, you will automatically launch the Reconnect project.

Once things are loaded, many things will not work because some external programs cannot be found and because the various utilities have not been build.

To check external programs, type in the REPL:

	(in-package :my-systems)

Then:

	(check-external-program-installations)
   
The first of the two lists returned contains the programs whose paths are *maybe* not known. The second list contains the properly referenced programs and their version.

This function tries to find out the version of the external programs by calling them. Some, like Opera or Google Chrome, do not answer to such query on Windows. Thus I have defined the associated command to not check their version. You will have an empty string instead of the version number. But, if their installation path is known, you will see a new window be opened for them during the check. So you will know if they are correctly referenced or not.

You will definitely know that a command is not referenced if you see a message instead of the version number saying "This program is not recognized as an internal or external command, blabla".
   
If and when you have determined that some installation paths are not properly referenced, do not leave LispWorks to mend things by hand! You can still update from inside the PATH variable held by Windows as well as the ones held by LispWorks. And there will be no need to relaunch your LispWorks environment nor to reboot Windows.

Type:

	(interactive-path-update (check-external-program-installations))

The *interactive-path-update* function will look for the required programs in the *Program Files* folders and in common executables locations (like *bin\*) and upon finding them, it will ask you if you want to update the PATH variables with the found location.

Finally, to build all the various utilities, type:

	(overview::install-work-environment)

To verify everything went ok, just type:

	(to-web:launch-watcher)

and

	(to-web:launch-watcher :watcher-type :core)

You may then refresh the various browsers opened when you have launched LispWorks through the *start-dev-environment* script.

Note: on browsers
-----------------

Two more web browsers may be installed. Because I use three screens and the simplest way I came up to always have the same configurations was to install three web browsers. On the left, Opera. On the right, Chrome. On the center screen, Firefox.

These browsers all share two common traits: they are a little deaf to command line options and are very sensible on what screen they were closed (and additionally, if they were closed while maximized or not also impacts that!).

It may seem complex but only the **overview** project uses them and it only uses them once. And it's a project of one file... You may just change its *initialize-work-environment* to have only two browsers or even one with all the various display in differents tabs. Whatever suits you.

4. How to launch the Reconnect project after installation
---------------------------------------------------------

Once properly installed, the Reconnect project only needs to be launched with the *start-dev-environment* script in *c:\home\lisp\dev\my-utilities\resources\commands\*.

The corresponding manual way to launch the Reconnect project is:
- Start LispWorks
-  Do 'File > Load...'
- Choose the start.lisp file in the *dev\* directory
- Type into the listener/command line: <code>(overview:initialize-work-environment)</code>

As a result, every time you launch the Reconnect project:

- Opera should be opened on <a target="_blank" href="http://localhost:8078/index.html">http://localhost:8078/index.html</a> and you should see the overview dashboard.
- Google Chrome should be opened on <a target="_blank" href="http://localhost:8079/index.html">http://localhost:8079/index.html</a> and you should only see the message *Waiting to watch something*.
- Open Firefox and go to the following url: <a target="_blank" href="http://localhost:9999/code-editor/editor">http://localhost:9999/code-editor/editor</a>. Google Chrome should switch to another page but it will be empty as long as you do not select a module to edit in Firefox
- In the bottom left input box, type 'my-u' and select for instance the *0-anaphora* module of the *my-utilities* system. The documentation of the module should be automatically displayed on Google Chrome.

For detailed information on the initialization process, open the file *c:\home\lisp\dev\overview\0-main.lisp* to read the very short and self-explaining code of the *initialize-work-environment* function.

Note: on the automated launch process
-------------------------------------

Personally, I have a gaming mouse with additional buttons, a combination of which launches the *start-dev-environment* script. Another one launches the *backup* script and yet another one launches the *close-all-windows*. This way, I can very quickly close down everything and restart any time I want.

You should find a way to easily call the *start-dev-environment* and the *close-all-windows* scripts for a more convenient use.

5. Create a test system
-----------------------

You can open the various modules of code of the project or you may create your own system of code. To do so, use the command:

	(my-systems:create-system <system-name> :load t)

Then... it's a bit manual from there. Create a lisp file to hold your code in the directory *<system-name>\* created in *c:\home\lisp\dev\*. It should begin with:

	(in-package :<system-name>)

In order to be recognized as a module of the system you have created, it has to be listed in the components property of the system definition located in the file *<system-name>.asd*. I indeed rely entirely on <a target="_blank" href="https://www.common-lisp.net/project/asdf/">ASDF</a> to manage code systems and their dependencies.

Since the *create-system* command is so low level, none of the hierarchical cache has been updated. The easiest way to do that is to just restart. That's a good point to automate the calls to the *start-dev-environment* and the *close-all-windows* scripts: have you done it? It is super convenient to just have them at hand.

Once your environment has restarted, you may go to <a target="_blank" href="http://localhost:9999/code-editor/editor">http://localhost:9999/code-editor/editor</a>, use Ctrl+1 to focus the input for opening module of code and type the name of the system you have created (the auto-complete will now recognize that name).

In the editor, define some functions or macros. With the cursor on a *complete* definition, Alt+Right will open the usage editor. Say you have defined the function *foo*, you write usage that way:

("Simple example"
	(equal (foo "test")
			<the-expected-result)
	("With the optional argument informed"
		(equal (foo "test" 1)
			<the-expected-result)))
("Special cases"
	...)

In short, a usage is a list of lists beginning with a documentation string with then holds tests or other lists beginning with a documentation string.

To save modifications, use Ctrl+Q. The code will get recompiled and usages will be reverified. Try to code little problems like defined arguments that are not referenced in the body of the function or tests that will fail to see these issues being detected after having saved the modifications.

The same shortcut, Ctrl+Q, will move the focus to the modules with issues if there is not any modification to be saved. On the associated watcher, you will have the full view debugged usages so as to understand exactly where things went wrong in your code.