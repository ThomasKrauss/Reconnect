(in-package :my-systems)

(defparameter *idrive-backup-file* (merge-pathnames "idrive-backup-data.txt"
                                                    (system-work-directory "my-systems")))

(defparameter *local-backup-file* (merge-pathnames "local-backup-data.txt"
                                                   (system-work-directory "my-systems")))

(defun make-backup-list ()
  "Build the list of folders and files to backup.
This list includes all the miscellaneous places registered in *backup-base* and all system dev, persist and resources directories."
  (append (loop for system-name in (system-list-all)
                collect (system-source-directory system-name))
          (remove-if #'directory-pathname-p
                     (list-directory (system-source-base-directory)))))

(defun print-idrive-backup-list (paths)
  "Output the backup list to *idrive-backup-file*.
Format is: c:/foo/bar.txt -> /C/foo/bar.txt"
  (with-open-file (s *idrive-backup-file*
                     :direction :output
                     :if-exists :supersede)
    (dolist (path paths)
      (format s "/~:@(~a~)~a~%"
              (pathname-host path)
              (regex-replace-all "\\" (if (directory-pathname-p path)
                                        (directory-namestring path)
                                        (concatenate 'string (directory-namestring path) (file-namestring path)))
                                 "/")))))

(defun print-local-backup-list (paths)
  "Output the backup list to *local-backup-file*.
Format is:"
  (with-open-file (s *local-backup-file*
                     :direction :output
                     :if-exists :supersede)
    (dolist (path paths)
      (format s "~a~%" path))))

(defun print-backup-files ()
  "Print all the backup files."
  (let ((backup-base (make-backup-list)))
    (print-idrive-backup-list backup-base)
    (print-local-backup-list backup-base)))