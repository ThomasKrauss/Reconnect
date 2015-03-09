;;; Copyright (c) 2006-2009, Dr. Edmund Weitz.  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; This file was derived from the start.lisp from the "Lisp Starter Pack"
;;; See <http://weitz.de/starter-pack/> for details.

;;; The following code is heavily changed and bear little similarities with
;;; the original process. In particular, it does not rely anymore on LW-ADD-ONS
;;; and LW-DOC.

;;; Note that Dr. Edmund Weitz does not maintain the Starter Pack since
;;; QuickLisp is now a good alternative to get librairies working in no
;;; time in one's environment.

(in-package :cl-user)

(defvar *basedir* #p"c:/home/lisp/")

(defvar *working-dir* (merge-pathnames #p"dev/" *basedir*)
  "The directory containing the projects and this file.")
  
(defvar *asdf-pathname* (merge-pathnames #p"quicklisp/asdf.lisp" *basedir*)
  "The ASDF version provided by QuickLisp.")

#+:win32
;; to "fix" USER-HOMEDIR-PATHNAME
;; see <http://support.microsoft.com/default.aspx?scid=kb;en-us;101507>
(setf (environment-variable "HOMEPATH")
			(directory-namestring *working-dir*)
			(environment-variable "HOMEDRIVE")
			(format nil "~:@(~A:~)" (pathname-host *working-dir*)))

; Setup quicklisp, which will load its own included version of ASDF
(load (merge-pathnames "quicklisp/setup.lisp" *basedir*))

; Update the ASDF system registry to include the working directory
; Code originally written by Dr. Edmund Weitz, as part of his starter pack
; See <http://weitz.de/starter-pack/>
(defvar *skip-if-no-asdf-file-found-p* t
  "If this variable has a true value, the process which searches for
ASDF system definitions won't recurse into directories which don't
contain system definitions themselves.")

(defun walk-directory-for-asdf (dir)
  "Looks into the directory DIR and all subdirectories and adds all
directories which contain files of type \"asd\" to
ASDF:*CENTRAL-REGISTRY*."
  (dolist (dir-candidate (directory (merge-pathnames "*" (lw:pathname-location dir))))
    (when (lw:file-directory-p dir-candidate)
      (let (found-some-p)
        (let ((asd-candidate (merge-pathnames "*.asd" dir-candidate)))
          (when (directory asd-candidate)
            (setq found-some-p t)
            (pushnew dir-candidate asdf:*central-registry* :test #'equal)))
        (when (or found-some-p
                  (not *skip-if-no-asdf-file-found-p*))
          (walk-directory-for-asdf dir-candidate))))))

(defun update-asdf-central-registry (base-directories)
  "Loops through base-directories recursively and adds all
directories containing system definitions to ASDF's central
registry."
  (dolist (base-dir base-directories)
    (walk-directory-for-asdf base-dir)))

(update-asdf-central-registry (list *working-dir*))

; Turn off warnings of redefinition of functions and macros
(setf dspec:*redefinition-action* nil)

; Change backup directory to *basedir* + /backup/
(defvar *backup-directory* (ensure-directories-exist (merge-pathnames "backup/" *basedir*)))

(defun make-backup-filename-using-backup-directory (pathname)
  "Creates and returns a backup pathname for PATHNAME. Substitute any backslash by a regular slash, does not check for illegal characters."
  (ensure-directories-exist
   (merge-pathnames (enough-namestring pathname *basedir*) *backup-directory*)))

(defadvice (editor::make-backup-filename alternative-location :around
                                         :documentation "Circumvents the original function.")
    (pathname)
	(make-backup-filename-using-backup-directory pathname))
		
; Set the current directory to be the working directory
(hcl:change-directory *working-dir*)

; Define some keybindings
(editor:bind-key "Compile Defun" #(#\control-\c #\control-\c) :mode "Lisp")
(editor:bind-key "Indent New Line" #\Return :mode "Lisp")
(editor:bind-key "Next Window" #(#\control-\x #\o) :mode "Lisp")

; Load librairies and systems
(asdf:oos 'asdf:load-op "hunchentoot")
(asdf:oos 'asdf:load-op "parenscript")
(asdf:oos 'asdf:load-op "code-editor")
(asdf:oos 'asdf:load-op "librarian")
(asdf:oos 'asdf:load-op "overview")