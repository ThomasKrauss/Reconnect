' adapted from http://bc.tech.coop/blog/040315.html by Bill Clementson

Dim Wsh
Set Wsh = Wscript.CreateObject("Wscript.Shell")

' Start up LispWorks
Wsh.Run("""C:\Program Files (x86)\LispWorks Personal\lispworks-personal-6-1-1-x86-win32""")

' Wait for a few seconds for it to start
WScript.Sleep(2500)

' Press the OK button on the splash screen, twice because sometimes the focus is not on the window
' Besides, if one is finally enough, the newline won't mess with the next command
Wsh.SendKeys "{ENTER}"
Wsh.SendKeys "{ENTER}"

' Load my start script
WScript.Sleep(100)
Wsh.SendKeys ("{(}with-open-file {(}s {(}make-pathname :host ""c"" :directory {(}list :absolute ""home"" ""lisp""{)} :name ""output-from-start-script.txt""{)} :direction :output :if-exists :supersede{)} {(}let {(}{(}*standard-output* s{)} {(}*error-output* s{)}{)} {(}load {(}make-pathname :host ""c"" :directory {(}list :absolute ""home"" ""lisp"" ""dev""{)} :name ""start.lisp""{)}{)}{)}{)}")

' Fully load my work environment
Wsh.SendKeys ("{(}overview::initialize-work-environment{)}")
Wsh.SendKeys "{ENTER}"