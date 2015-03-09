(in-package :my-utilities)

(defun not-null (a) (not (null a)))

(defun list= (a b &key (test #'equal))
  "Equality regardless of order but not repetition."
  (and (= (length a) (length b))
       (every (lambda (x) (find x b :test test)) a)
       (every (lambda (x) (find x a :test test)) b)))

(defun list-equal (a b &key (test #'equal))
  "Equality regardless of order and repetition."
  (and (every (lambda (x) (find x b :test test)) a)
       (every (lambda (x) (find x a :test test)) b)))

(defun pathname= (a b)
  "My equality for pathnames, not distinguishing nil and :unspecific for the various pathname components, also not distinguishing :newest from them for the version component.
The problem is that the :unspecific value and the nil value do not impact the printed representation of a pathname but with the equal function, they make pathnames not the same.
When I do not care about such difference, like when testing, I will rely on pathname=."
  (flet ((my-path-equal (x y &rest special-values)
           (let ((special-values
                  (append '(nil :unspecific) special-values)))
             (or (and (member x special-values)
                      (member y special-values))
                 (string-equal x y)))))
    (and (my-path-equal (pathname-host a) (pathname-host b))
         (my-path-equal (pathname-device a) (pathname-device b))
         (let* ((a (pathname-directory a))
                (b (pathname-directory b))
                (result (= (length a) (length b))))
           (loop for x in a
                 for y in b
                 while result
                 do (setf result
                          (cond ((and (symbolp x) (symbolp y))
                                 (eq x y))
                                ((and (stringp x) (stringp y))
                                 (string-equal x y))
                                (t (eq x y)))))
           result)
         (my-path-equal (pathname-name a) (pathname-name b))
         (my-path-equal (pathname-type a) (pathname-type b))
         (my-path-equal
          (pathname-version a)
          (pathname-version b)
          :newest)
         a)))

(defun my-equal (x y)
  "My version of equal, similar to equal in all respect except for pathnames where it uses pathname=.
Therefore, pathnames are considered equal even if they differ on some components which can be either nil or :unspecific (or :newest in the case of the version components."
  (cond ((or (and (numberp x) (numberp y))
             (and (characterp x) (characterp y)))
         (eql x y))
        ((and (consp x) (consp y))
         (and (my-equal (first x) (first y))
              (my-equal (rest x) (rest y))))
        ((or (and (bit-vector-p x) (bit-vector-p y))
             (and (stringp x) (stringp y)))
         (let ((result (= (length x) (length y))))
           (loop for a across x
                 for b across y
                 while result
                 do (setf result (eql a b)))
           result))
        ((and (pathnamep x) (pathnamep y)) (pathname= x y))
        (t (eq x y))))

(defun eq-diff (x y)
  (unless (eq x y)
    (if (not (eq (type-of x) (type-of y)))
        (format nil
                "Distinct types: a ~(~a and a ~a~)"
                (type-of x)
                (type-of y))
      "Non-eq values")))

(defun eql-diff (x y)
  (unless (eql x y)
    (cond ((and (numberp x) (numberp y))
           (if (not (eq (type-of x) (type-of y)))
               (format nil
                       "Distinct types of numbers: a ~(~a and a ~a~)"
                       (type-of x)
                       (type-of y))
             (format nil "Distinct ~(~a~)s" (type-of x))))
          ((and (characterp x) (characterp y)) "Distinct characters")
          (t (eq-diff x y)))))

(defun bit-vector-diff (x y)
  (unless (equal x y)
    (cond ((< (length x) (length y))
           "The first bit-vector is shorter than the second")
          ((> (length x) (length y))
           "The first bit-vector is longer than the second")
          (t
           (loop for a across x
                 for b across y
                 if (= a b)
                   collect a
                 else
                   collect (list :distinct a b))))))

(defun string=-diff (x y)
  (unless (string= x y)
    (cond ((< (length x) (length y))
           "The first string is shorter than the second")
          ((> (length x) (length y))
           "The first string is longer than the second")
          (t
           (loop for a across x
                 for b across y
                 if (char= a b)
                   collect a
                 else
                   collect (list :distinct a b))))))

(defun string-equal-diff (x y)
  (unless (string= x y)
    (cond ((< (length x) (length y))
           "The first string is shorter than the second")
          ((> (length x) (length y))
           "The first string is longer than the second")
          (t
           (loop for a across x
                 for b across y
                 if (char-equal a b)
                   collect a
                 else
                   collect (list :distinct a b))))))

(defun pathname-diff (x y)
  "Difference of two pathnames following the rules of equal: every pathname components of the two given pathnames must be the same.
This notion of being the same is implementation dependent regarding of the case of the strings.
Currently only tested for LispWorks on Windows where the case seems to be irrelevant for all components, although device or version components can probably only be nil, :unspecific or :newest (version only) on Windows."
  (unless (equal x y)
    (flet ((test (x y) (if (string-equal x y) x (list :distinct x y)))
           (diff-pathname-directory (a b)
             (let (result)
               (loop for x in a
                     for y in b
                     do (push (cond ((and (symbolp x) (symbolp y))
                                     (aif (eq-diff x y)
                                          (list :distinct x y it)
                                          x))
                                    ((and (stringp x) (stringp y))
                                     (if (string-equal x y)
                                         x
                                       (list :distinct
                                             x
                                             y
                                             "Distinct strings")))
                                    (t
                                     (aif (eq-diff x y)
                                          (list :distinct x y it)
                                          x)))
                              result))
               (nreverse result))))
      (list :host
            (test (pathname-host x) (pathname-host y))
            :device
            (test (pathname-device x) (pathname-device y))
            :directory
            (diff-pathname-directory
             (pathname-directory x)
             (pathname-directory y))
            :name
            (test (pathname-name x) (pathname-name y))
            :type
            (test (pathname-type x) (pathname-type y))
            :version
            (test (pathname-version x) (pathname-version y))))))

(defun pathname=-diff (x y)
  (unless (pathname= x y)
    (flet ((test (x y &rest special-values)
             (if (let ((special-values
                        (append '(nil :unspecific) special-values)))
                   (or (and (member x special-values)
                            (member y special-values))
                       (string-equal x y)))
                 x
               (list :distinct x y)))
           (diff-pathname-directory (a b)
             (let (result)
               (loop for x in a
                     for y in b
                     do (push (cond ((and (symbolp x) (symbolp y))
                                     (aif (eq-diff x y)
                                          (list :distinct x y it)
                                          x))
                                    ((and (stringp x) (stringp y))
                                     (if (string-equal x y)
                                         x
                                       (list :distinct
                                             x
                                             y
                                             "Distinct strings")))
                                    (t
                                     (aif (eq-diff x y)
                                          (list :distinct x y it)
                                          x)))
                              result))
               (nreverse result))))
      (list :host
            (test (pathname-host x) (pathname-host y))
            :device
            (test (pathname-device x) (pathname-device y))
            :directory
            (diff-pathname-directory
             (pathname-directory x)
             (pathname-directory y))
            :name
            (test (pathname-name x) (pathname-name y))
            :type
            (test (pathname-type x) (pathname-type y))
            :version
            (test (pathname-version x) (pathname-version y) :newest)))))

(defun paragraph-diff (str1 str2)
  (flet ((print-additional (lines nb)
           (format t
                   "~%Additional lines in string n°~a:~%~(~s~)"
                   nb
                   lines)))
    (let ((lines1 (split "\\n" str1)) (lines2 (split "\\n" str2)))
      (when (/= (length lines1) (length lines2))
        (format t
                "~a lines in first string against ~a lines in second string~%"
                (length lines1)
                (length lines2)))
      (loop for l1 in lines1
            for l2 in lines2
            if (string= l1 l2)
              do (format t "~s~%" l1)
            else
              do (format t "~%1>>> ~s~%2>>> ~s~%~%" l1 l2))
      (if (< (length lines1) (length lines2))
          (print-additional
           (subseq lines2 (- (length lines2) (length lines1)))
           2)
        (if (< (length lines2) (length lines1))
            (print-additional
             (subseq lines1 (- (length lines1) (length lines2)))
             1))))))

(defun equal-diff (x y)
  (unless (equal x y)
    (cond ((or (and (numberp x) (numberp y))
               (and (characterp x) (characterp y)))
           (eql-diff x y))
          ((and (consp x) (consp y))
           (cons (aif (equal-diff (first x) (first y))
                      (if (and (consp (first x)) (consp (first y)))
                          it
                        (list :distinct (first x) (first y) it))
                      (first x))
                 (aif (equal-diff (rest x) (rest y)) it (rest x))))
          ((and (bit-vector-p x) (bit-vector-p y))
           (bit-vector-diff x y))
          ((and (stringp x) (stringp y)) (string=-diff x y))
          ((and (pathnamep x) (pathnamep y)) (pathname-diff x y))
          (t (eq-diff x y)))))

(defun my-equal-diff (x y)
  (unless (equal x y)
    (cond ((or (and (numberp x) (numberp y))
               (and (characterp x) (characterp y)))
           (eql-diff x y))
          ((and (consp x) (consp y))
           (cons (aif (equal-diff (first x) (first y))
                      (if (and (consp (first x)) (consp (first y)))
                          it
                        (list :distinct (first x) (first y) it))
                      (first x))
                 (aif (equal-diff (rest x) (rest y)) it (rest x))))
          ((and (bit-vector-p x) (bit-vector-p y))
           (bit-vector-diff x y))
          ((and (stringp x) (stringp y)) (string=-diff x y))
          ((and (pathnamep x) (pathnamep y)) (pathname=-diff x y))
          (t (eq-diff x y)))))

(defun file-equal (a b)
  "Test if the contents of file a and of file b are equal, case sensitively.
Return true if both files are missing."
  (and (equal (not-null (file-exists-p a))
              (not-null (file-exists-p b)))
       (let ((ok t))
         (when (file-exists-p a)
           (with-open-file (in-a a)
             (with-open-file (in-b b)
               (loop for line-a = (read-line in-a nil)
                     for line-b = (read-line in-b nil)
                     while (and ok line-a line-b)
                     do (setf ok (string= line-a line-b))))))
         ok)))

(defun directory-equal (a b &key follow-symlinks)
  (and (equal (not-null (directory-exists-p a))
              (not-null (directory-exists-p b)))
       (let ((ok t))
         (when (directory-exists-p a)
           (let ((content-a
                  (list-directory a :follow-symlinks follow-symlinks))
                 (content-b
                  (list-directory b :follow-symlinks follow-symlinks)))
             (setf ok
                   (equal (mapcar (lambda (pathname)
                                    (enough-namestring pathname a))
                                  content-a)
                          (mapcar (lambda (pathname)
                                    (enough-namestring pathname b))
                                  content-b)))
             (loop for pathname-a in content-a
                   for pathname-b in content-b
                   while ok
                   do (setf ok
                            (if (directory-pathname-p pathname-a)
                                (directory-equal pathname-a pathname-b)
                              (file-equal pathname-a pathname-b))))))
         ok)))

(defmacro with-tested-files ((&rest bindings) &body body)
  "Just like a let except it assumes all the values in the bindings are file pathnames.
Because with-tested-files ensure that after the given body has been evaluated, each of these files are deleted."
  `(let ,bindings
               (unwind-protect
                   (progn ,@body)
                 ,@(mapcar (lambda (binding)
                            `(when (file-exists-p ,(first binding))
                                         (delete-file ,(first binding))))
                          bindings))))

(defmacro values-equal (values-perimeter &body val)
  "Test if each value of the given perimeter, expected to return multiple values, matches the given value.
These expected values must be listed in the same order the values-perimeter returns them.
Additionally, the exact number of values must be passed, even if nil. This macro will consider the global result to have failed when the number of returned values does not match the number of expected values, even if all individual comparison should work."
  (with-gensyms (returned-values)
    `(let ((,returned-values
                      (multiple-value-list ,values-perimeter)))
                 (and (= (length ,returned-values)
                         ,(length val))
                      (every #'equal
                             (list ,@val)
                             ,returned-values)))))

