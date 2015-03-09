(in-package :cl-user)

(defvar *basedir* #p"c:/home/lisp/")

#+:win32
;; to "fix" USER-HOMEDIR-PATHNAME
;; see <http://support.microsoft.com/default.aspx?scid=kb;en-us;101507>
(setf (environment-variable "HOMEPATH")
		(directory-namestring *basedir*)
		(environment-variable "HOMEDRIVE")
		(format nil "~:@(~A:~)" (pathname-host *basedir*)))

(load #p"~/quicklisp.lisp")

(quicklisp-quickstart:install)

;; Install needed libraries

; trivial-utf-8, to handle UTF-8 encoding
(ql:quickload "trivial-utf-8")

; hunchentoot, a great Common Lisp web-server
; note it also comes with the following libraries which are used by the Reconnect project:
; - cl-fad, "fad" for _F_ile _A_nd _D_irectories, a library of useful and portable utilities
; - cl-ppcre, a very cool library to do regular expressions
(ql:quickload "hunchentoot")

; A Javascript code generation library. Super cool too.
(ql:quickload "parenscript")

; to handle the JSON format, switching (pretty) seamlessly from Lisp data to JSON data and vice-versa.
(ql:quickload "st-json")

; to handle HTTP communications
(ql:quickload "drakma")