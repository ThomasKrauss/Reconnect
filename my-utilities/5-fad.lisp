(in-package :my-utilities)

(defun last-directory (path) (first (last (pathname-directory path))))

(defun file-directory (path)
  (if (stringp path)
    path
    (make-pathname :host (pathname-host path)
                   :directory (pathname-directory path))))

(defun copy-directory-and-files (from to &key overwrite)
  "Recursively copy the content of the from directory to the to directory.
If overwrite is true, actually replace all the content of the to directory."
  (when (and (directory-exists-p to) overwrite)
    (delete-directory-and-files to))
  (if (directory-exists-p to)
      (error "The destination already exists : ~a" to)
    (progn
      (walk-directory from
                      (lambda (x)
                        (let ((target
                               (merge-pathnames (enough-namestring x
                                                                   from)
                                                to)))
                          (ensure-directories-exist target)
                          (when (not (directory-pathname-p x))
                            (copy-file x target))))
                      :directories
                      t)
      (directory-exists-p to))))

(defun load-file-content (path)
  "Load the content of the file at the given path, reading line by line. Nil if the file does not exist"
  (when (and path (file-exists-p path))
    (with-output-to-string (str)
      (with-open-file (s path)
        (loop for char = (read-char s nil)
              while char
              do (write-char char str))))))

(defun save-file-content (path content)
  "Save the given content (string expected) to the file at given path."
  (with-open-file (s
                   path
                   :direction
                   :output
                   :if-exists
                   :supersede
                   :if-does-not-exist
                   :create)
    (write-string content s)))

(defun read-obvious-links-from-file (file)
  "The file is supposed to contain a list of lines
that are associated 2 by 2 in the order they are written.

Return a list composed of such couples."
  (let (link results)
    (flet ((commit-link ()
             (when (< 0 (length link))
               (push (nreverse link) results)
               (setf link nil))))
      (with-open-file (s file)
        (loop for line = (read-line s nil)
              while line
              if (string/= "" line)
              do (push line link)
              else
              do (commit-link)))
      (commit-link)
      (nreverse results))))

(defvar *escape-list* (list #\\ #\/ #\: #\* #\? #\" #\< #\> #\|))

(defun escape-filename (str)
  (with-output-to-string (out)
    (with-input-from-string (in str)
      (loop for char = (read-char in nil)
            while char
            do (let ((pos (position char *escape-list* :test #'char=)))
                 (if pos
                     (progn
                       (write-char #\~ out)
                       (write-char (digit-char pos) out))
                   (write-char char out)))))))

(defun parse-filename (str)
  (with-output-to-string (out)
    (with-input-from-string (in str)
      (loop for char = (read-char in nil nil)
            while char
            if (char= char #\~)
              do (let (next-chars)
                   (loop
                     (let ((next (peek-char nil in nil nil)))
                       (if (and next
                                (alphanumericp next)
                                (not (alpha-char-p next)))
                           (push (read-char in nil) next-chars)
                         (return))))
                   (if next-chars
                       (write-char (elt *escape-list*
                                        (parse-integer (concatenate 'string
                                                                    (nreverse next-chars))))
                                   out)
                     (write-char char out)))
            else
              do (write-char char out)))))

