(in-package :my-systems)

(defconstant +used-external-programs-file+ (ensure-directories-exist
                                            (merge-pathnames "used-external-programs.lisp"
                                                             (system-work-directory "my-systems")))
  "The file storing the used external programs, in order to not check their installation every time the Lisp environment is launched. I ensure the directories exist because when launching the Reconnect project for the first, my-systems will be loaded before all system directories are created, work/my-systems included. Saving used external programs to this file will then fail.")

(defvar *registered-external-programs* nil
  "The list of external programs registered through the macro defun-to-external-program, along with the command and the version arguments. Stored as list: (program-name command version-arguments).")

(defvar *used-external-programs* (awhen (load-file-content +used-external-programs-file+)
                                   (first (read-code-chunk it "my-systems")))
  "The list of external programs, registered through the macro defun-to-external-program, but only if the installation is ok. Note that when the command to check the version of the external program was not given, the program is still registered in this variable.")

(defparameter *external-program-paths* '("Program Files" "Program Files (x86)") 
  "The paths to look programs in.")

(defparameter *external-program-locations* '("" "bin" "build")
  "The directories where a program might be stored in an installation.")

(defconstant +accepted-executable-extensions+ '("exe" "bat" "jar"))

(defmacro defun-to-external-program (name (program-name command version-arguments) &body body)
  (once-only (program-name command version-arguments)
    `(progn
       (unless (member ,program-name *registered-external-programs* :test #'string= :key #'first)
         (push (list ,program-name ,command ,version-arguments) *registered-external-programs*))
       (unless (member ,program-name *used-external-programs* :test #'string=)
         (if (or (null ,version-arguments)
                 (check-for-version-number (get-external-program-version ,command ,version-arguments)
                                           :check-all-version-numbers t))
             (progn
               (push ,program-name *used-external-programs*)
               (save-file-content +used-external-programs-file+
                                  (print-code-chunk *used-external-programs* "my-systems")))
           (warn "The external program ~a is not properly referenced in the PATH" ,program-name)))
       (defun ,name ,@body))))
  
(defun check-for-version-number (string &key check-all-version-numbers)
  "=> list
Parse version numbers in a multiline string.
It is expected the version number is: number.number.number
By default, the search stops at the very first version number encountered, a behavior that is disabled by setting check-all-version-numbers to true.
Implementation note: the code seems a little bit convoluted but it's just that I test several alternate regex. I couldn't make alternation work in only one regex with the pipe '|' character"
  (let (results)
    (flet ((scan-with (regex)
             (do-register-groups
                 (version-number)
                 (regex string)
               (if check-all-version-numbers
                 (push version-number results)
                 (return (setf results version-number))))))
      (loop for regex in (list "(\\d+\\.\\d+\\.\\d+)"
                               "version (\\d+\\.\\d+\\.\\d+)"
                               "version (\\d+)"
                               "Version: v([^\\s]+)")
            while (or (null results) check-all-version-numbers)
            do (scan-with regex)))
    results))

(defun get-external-program-version (command version-command)
  "=> string
Ask for the version of a program to the program itself.
Implementation note: I'm concatenating each part of the command instead of listing them because the former worked for ImageMagick while the second failed with an 'Invalid drive specification'. And since it works for every other programs..."
  (with-output-to-string (s)
    (call-command (join-str (aif version-command
                                 (append (list command) (mklist it))
                                 (list command))
                            #\Space)
                  :output-stream s)))

(defun check-external-program-installations (&key (external-programs *registered-external-programs*) check-all-version-numbers)
  "=> (ko-registered-external-programs ok-registered-external-programs)
Try to get the version of each external programs, by default all the ones listed in *registered-external-programs*.
A program will be listed as ko if calling it results in an error since that indicates a un-referenced program. But it will also be listed as ko if we can't find any version number."
  (let (ok ko)
    (loop for (name command version-command) in external-programs
          do (handler-case (get-external-program-version command version-command)
               (condition (c)
                 (let-a (if (stringp c)
                            c
                          (format nil "~A" c))
                   (push (list name it) ko)))
               (:no-error (result)
                 (aif (check-for-version-number result :check-all-version-numbers check-all-version-numbers)
                      (push (list name it) ok)
                      (push (list name result) ko)))))
    (values ko ok)))

(defun list-accessible-drives ()
  "=> list
List accessible drives.
Initially I have listed every letter thanks to the code: (loop for i from 97 to 122 collect (mkstr (code-char i))). Works since code-char was using ASCII codes.
The problem is that things get out of the hands of Lispworks when asking directory-exists-p and in such cases I get a dialog from Windows asking to plug a device for the given host. Very annoying! So I stopped at g:/"
  (loop for str in (list "a" "b" "c" "d" "e" "f" "g")
        if (directory-exists-p (make-pathname :host str :directory '(:absolute)))
        collect it))

(defun get-file-name-and-extension (path)
  "=> list (filename extension version-number)
Decompose a path in three parts: the name of the file, its extension and the version number found after the name, if any. The version number is expected to in the format number.number.number Each on of the extension and version number will be set to the empty string if they cannot be found."
  (let ((file (if (stringp path)
                (file-namestring (pathname path))
                (file-namestring path))))
    (aif (register-groups-bind
             (filename version-number extension)
             ("(.*)\\-(\\d+\\.\\d+.\\d+)\\.(.*)" file)
           (list filename extension version-number))
         it
         (aif (register-groups-bind
                  (filename extension)
                  ("(.*)\\.(.*)" file)
                (list filename extension ""))
              it
              (list file "" "")))))

(defun check-for-program-locations (external-programs &optional additional-program-paths)
  "=> locations
Check all potential locations for the given external-programs. A location is of the form: (program-name program-filename list-of-path). Note that a location contain the program filename and that may not be the command itself. It is for .exe files but it isn't for .jar files. In particular, it can contain a version number (in the case of .jar files for instance).
The work is done like this: check for every drive if a given path exists (like \"Program Files\") and if so, for each directories it contains, check if it has any commonly used installation folder (like itself or \"bin\"). For each existing installation folder, search if any of its files matches the command of the given external programs.
By default, this function uses the paths listed in *external-program-paths* but one can give <additional-program-paths>. This function will also look, for each external-program, if some special installation folders are mentionned in the specification and also take a look at them."
  (let (results)
    (labels ((get-executable-files (directory)
               "Note: insensitive case testing of extensions"
               (loop for file in (remove-if #'directory-pathname-p
                                            (list-directory directory))
                     when (member (second (get-file-name-and-extension file)) +accepted-executable-extensions+
                                  :test #'string-equal)
                     collect (file-namestring file)))
             (register (name command value)
               (aif (position name results :test #'string= :key #'first)
                    (setf (third (elt results it))
                          (cons value (third (elt results it))))
                    (push (list name command (list value)) results)))
             (check-command (external-program executable-files path)
               "Note: insensitive case testing of program name"
               (let ((file (find (second external-program) executable-files
                                 :test #'string-equal
                                 :key (lambda (file)
                                        (first (get-file-name-and-extension file))))))
                 (when file
                   (register (first external-program) file path))))
             (check-program-path (path)
               "First check common locations, then check specific program locations"
               (when (directory-exists-p path)
                 (dolist (install-path (remove-if-not #'directory-pathname-p
                                                      (list-directory path)))
                   (dolist (location *external-program-locations*)
                     (awhen (directory-exists-p (merge-pathnames location install-path))
                       (let ((executable-files (get-executable-files it)))
                         (dolist (external-program external-programs)
                           (check-command external-program executable-files it)))))
                   (dolist (external-program external-programs)
                     (dolist (location (mklist (fourth external-program)))
                       (awhen (directory-exists-p (merge-pathnames location install-path))
                         (check-command external-program (get-executable-files it) it))))))))
      (dolist (base-path (list-accessible-drives))
        (dolist (path *external-program-paths*)
          (check-program-path (merge-pathnames path base-path))))
      (dolist (path additional-program-paths)
        (check-program-path path))
      (check-program-path (system-external-resources-directory)))
    (append results
            (loop for external-program in external-programs
                  unless (member (first external-program) results :test #'string= :key #'first)
                  collect (list (first external-program) (second external-program) nil)))))

(defun pathname-string (path)
  "=> string
Output the path as a string with the drive letter in upper case."
  (concatenate 'string (string-upcase (pathname-host path)) ":" (directory-namestring path) (file-namestring path)))

(defun get-user-environment-path ()
  "=> string
Get the value in the User PATH variable.
We can't just ask for the PATH variable with 'echo %PATH%' because it is a merge of the System PATH variable and the User one. So we ask the register instead and, astonishingly, the output has to be parsed to really get just what we asked for..."
  (register-groups-bind (reg-type string)
      ("(REG_EXPAND_SZ|REG_SZ)\\s+\(.*\)" (with-output-to-string (s)
                                   (call-command "REG QUERY HKEY_CURRENT_USER\\Environment /v PATH"
                                                 :output-stream s)))
    (declare (ignore reg-type))
    string))

(defun update-path-variables (paths)
  "=> string
Update LispWorks PATH variable and the User PATH variable with the given path (whose duplicates will be removed).
Return the list of the paths that were added.
Implementation notes: Why doing it like that?
Because the PATH variable is a merge of the System PATH and the User PATH and 'setx path' will only update the User one! So I need to get the value of the User PATH to avoid saving in it the System PATH content as well.
Picture a the result of calling this function if implemented that way:
System PATH = X and User PATH = X + old User PATH
So what happens if I run this function again?
User PATH = X + X + old User PATH!
Lots of duplicated paths! Hence the call to get-user-environment-path.

Now, why using a Lisp function and an extra environment instead of scripting down all the things? Two reasons.
One, it is insanely sophisticated to get the result of a command in Windows scripting. Abashing but true.
Two, in addition I have to parse the result (of the command inside get-user-environment-path) because it actually holds more than what I ask for! Again, abashing.
Oh yeah, and I won't also use any option of 'setx' because they are indicated by a '/' on Win7 but on XP it's by a '-'. That's sheer brilliance for portability. So I prefer to do all this inside Lisp."
  (awhen (remove-duplicates paths :test #'pathname-equal)
    (setf (lispworks:environment-variable "PATH")
          (concatenate 'string (lispworks:environment-variable "PATH")
                       ";"
                       (join-str (mapcar #'pathname-string it) #\;)))
    (call-command "setx path \"%mypath%\""
                  :extra-environ (list (cons "mypath"
                                             (join-str (append (list (get-user-environment-path))
                                                               (mapcar (lambda (path)
                                                                         (let-a (pathname-string path)
                                                                           (subseq it 0 (1- (length it)))))
                                                                       it))
                                                       #\;))))
    it))

(defun make-proxy-bat-file (path command)
  "=> path
Make a proxy BAT file to call as a command functionnalites packed in a JAR file.
JAR files aren't naturally lookup by Java in the PATH variable but in the CLASSPATH variable. So I think the best way to execute JAR files is with a proxy BAT file.
Here's my reasoning.
The other options I have thought of was using the global CLASSPATH or using the command-line one in a call to \"java\". Both are not elegant. Messing with the global CLASSPATH obviously impact every call to Java which is very undesirable.
The command-line CLASSPATH seems a better choice at first sight but that means I must deal with the precise installation folder on a per-command basis. That defeats the whole purpose of not having to deal with installation folders in the Lisp code!
A compromise could have been to maintain a list of every JAR installation paths and give it blindly to each call to \"java\"
but it will undoubtedly results in a conflict of dependencies between the various JAR files since this option means all their dependencies will be merged in one CLASSPATH and they were designed to run that way... This is Jar Hell!
Implementation notes: I use '@echo off' because I only want the output of the command but Windows CMD output all lines by default. And ~dp0 means the current directory while %* means all the arguments."
  (let-a (merge-pathnames (concatenate 'string (first (get-file-name-and-extension command)) ".bat") path)
    (with-open-file (s it
                       :direction :output
                       :if-exists :supersede)
      (write-line "@echo off" s)
      (write-line (concatenate 'string "java -jar %~dp0" command " %*") s))
    it))

(defun interactive-path-update (ko-installations &optional additional-program-paths)
  "Given a list of un-referenced programs (as obtained with a call to check-external-program-installations), it will check for program locations and when some are found, it will ask interactively the one you want to register in the PATH variable for the targeted program. For JAR files, accepting a given path will create a proxy BAT file in the accepted path.
Notes:
- it will update the PATH variable LispWorks maintains so as to make a LispWorks reboot unneeded.
- it will update the User PATH variable, not the System one since it is likely LispWorks doesn't run -and probably
should not run- with administrator privileges. If you prefer to have all the paths in the System PATH variable, that's up to you. You can still run this function to painlessly update paths and then copy-paste all of them from the User PATH variable to the System one. You will need to reboot your computer after that so that LispWorks takes notice of these changes."
  (flet ((prompt-read (prompt)
           "From Practical Common Lisp. Prompt for some user input."
           (format *query-io* "~a " prompt)
           (force-output *query-io*)
           (read-line *query-io*)))
    (let* ((external-programs (loop for name in (mapcar #'first ko-installations)
                                    collect (find name *registered-external-programs* :test #'string= :key #'first)))
           (locations (check-for-program-locations external-programs additional-program-paths))
           (data "-1")
           paths)
      (dolist (location locations)
        (case (length (third location))
          (0
           (format t "No location found for program ~a~%~%"
                   (first location))
           (setf data ""))
          (1
           (format t "Only one location found for program ~a: ~a~%~%"
                   (first location)
                   (first (third location)))
           (setf data (if (y-or-n-p "Select it?") "1" "")))
          (t
           (format t "~a locations found for the program ~a:~%~{~2t~a~%~}~%"
                   (length (third location))
                   (first location)
                   (third location))
           (loop while (and (string/= "" data)
                            (let-a (parse-integer data :junk-allowed t)
                              (or (null it) (< it 1) (< (length (third location)) it))))
                 do (setf data (prompt-read "Select which one?")))))
        (when (string/= "" data)
          (let ((reference-path (elt (third location) (1- (parse-integer data :junk-allowed t)))))
            (push reference-path paths)
            (when (string= "jar" (second (get-file-name-and-extension (second location))))
              (make-proxy-bat-file reference-path (second location)))))
        (setf data "-1"))
      (update-path-variables paths))))

(defun cmd-escape (str &key (escape-levels 1))
  "=> string
Newlines are replaced by spaces and characters < > | are escaped (unless in a double-quoted area).
The escape character is the caret ^. Note that if you need to pass data to a command via stdin, you will need to echo it and then pipe it. Both operations need a level of escape!"
  (let (result in-double-quotes)
    (loop for char across str
          when (char= #\" char)
          do (setf in-double-quotes (not in-double-quotes))
          when (and (member char '(#\< #\| #\>))
                    (not in-double-quotes))
          do (dotimes (i (1+ (* 2 (1- escape-levels))))
               (push #\^ result))
          do (push (if (char= char #\Newline)
                     #\Space
                     char)
                   result))
    (concatenate 'string (nreverse result))))