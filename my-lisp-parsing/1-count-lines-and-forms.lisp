(in-package :my-lisp-parsing)

(defun cache-directory (system-name)
  (merge-pathnames "caches/" (system-work-directory system-name)))

(defun totalize-stats (&optional a b (fn #'+))
  (let (total)
    (dolist (item (list a b))
      (loop for key in (plist-keys item)
            do (setf (getf total key)
                     (funcall fn
                              (or (getf total key)
                                  (funcall fn))
                              (getf item key)))))
    total))

(defun totalize-sub-stats (&optional a b)
  (totalize-stats a b #'totalize-stats))

(defun totalize-encompassed-stats (lst &optional (fn #'totalize-stats))
  (reduce fn (mapcar (lambda (x)
                     (getf x :stats))
                   lst)))

(defun print-only (item &rest keys)
  (plist-overwrite
   (copy-tree item)
   (list :in
         (loop for item in (getf item :in)
               collect (copy-plist item :only keys)))))

(defun print-root-stats (items)
  (print-only (list :in items
                    :stats (totalize-encompassed-stats items))
              :name :stats))

(defun line-form-ratio (a b)
  (if (/= 0 b)
    (float (/ a b))
    0))

(defun count-lines-and-forms-from-stream (stream)
  "Count each line per form, including comment and empty lines in them.
The comment and empty lines at the top-level of the stream are ignored.
Return..."
  (let (result)
    (let ((fsm (let (state-fn
                     (perimeter-level 0)
                     (line-count 0)
                     previous-character
                     capture
                     token
                     id)
                 (setq state-fn
                       (macrolet ((state (s) `(setq state-fn #',s)))
                         (labels ((parse (char)
                                    (if (or (null previous-character)
                                            (char/= previous-character #\\))
                                      (case char
                                        (:out
                                         (values line-count perimeter-level token id))
                                        (#\(
                                         (when (= 0 perimeter-level)
                                           (incf line-count)
                                           (setf capture t))
                                         (incf perimeter-level))
                                        (#\)
                                         (decf perimeter-level)
                                         (when (= 0 perimeter-level)
                                           (when token
                                             (push (apply #'mkstr (nreverse token)) id)
                                             (setf token nil))
                                           (push (list (nreverse id) line-count) result)
                                           (setf line-count 0
                                                 id nil)))
                                        (#\"
                                         (state parse-string))
                                        (#\;
                                         (state parse-single-comment-line))
                                        (#\Newline
                                         (when (< 0 perimeter-level)
                                           (incf line-count)))
                                        (#\|
                                         (when (and previous-character
                                                    (char= previous-character #\#)
                                                    (char= char #\|))
                                           (state parse-multiple-comment-lines)))
                                        (t
                                         (capture? char)))
                                      (capture? char))
                                    (setf previous-character char))
                                  (capture? (char)
                                    (when capture
                                      (if (char= char #\Space)
                                        (progn
                                          (push (apply #'mkstr (nreverse token)) id)
                                          (setf token nil)
                                          (when (< 1 (length id))
                                            (setf capture nil)))
                                        (push char token))))
                                  (parse-string (char)
                                    (when (char= char #\Newline)
                                      (incf line-count))
                                    (when (and (char= #\" char)
                                               (char/= #\\ previous-character))
                                      (state parse))
                                    (setf previous-character char))
                                  (parse-single-comment-line (char)
                                    (when (and (< 0 perimeter-level)
                                               (char= char #\Newline))
                                      (incf line-count))
                                    (when (char= #\Newline char)
                                      (state parse)))
                                  (parse-multiple-comment-lines (char)
                                    (when (and (< 0 perimeter-level)
                                               (char= char #\Newline))
                                      (incf line-count))
                                    (when (and previous-character
                                             (char= previous-character #\|)
                                             (char= char #\#))
                                      (setf previous-character nil)
                                      (state parse))))
                           #'parse)))
                 (lambda (&rest params)
                   (apply state-fn params)))))
      (when stream
        (loop for char = (read-char stream nil)
              while char
              do (funcall fsm char)))
      (nreverse result))))

(defun count-lines-and-actions-of-module (system-name module-name)
  (let ((form-count 0)
        (line-count 0))
    (let-a (loop for (id count) in (with-open-file (in (system-module-file module-name system-name))
                                     (count-lines-and-forms-from-stream in))
                 when (or (string= (first id) "defun")
                          (string= (first id) "defmacro"))
                 collect (let ((action-name (second id)))
                           (incf form-count)
                           (incf line-count count)
                           (list :name (find-symbol (string-upcase action-name) (system-package system-name))
                                 :stats (list :form-count 1 :line-count count :line-form-ratio count))))
      (list :stats (as-plist line-count form-count (line-form-ratio line-count form-count))
            :in it))))

(defun print-root-stats-with-ratio (system-items)
  (let ((stats (totalize-encompassed-stats system-items)))
    (setf (getf stats :line-form-ratio)
          (line-form-ratio (getf stats :line-count)
                           (getf stats :form-count)))
    (print-only (list :in system-items
                      :stats stats)
                :name :stats)))

(define-hierarchical-cache :line-and-form-counts (((merge-pathnames "line-and-form-counts/"
                                                                   (cache-directory "my-lisp-parsing"))
                                                  (cache-directory "my-lisp-parsing"))
                                                 :root (nil
                                                        ("line-and-form-counts" print-root-stats-with-ratio))
                                                 :system ((lambda (item &rest names)
                                                            (declare (ignore names))
                                                            (print-only item :name :stats)))
                                                 :module ((lambda (item &rest names)
                                                            (declare (ignore names))
                                                            (print-only item :name :stats)))
                                                 :action ((lambda (action-item &rest names)
                                                            (declare (ignore names))
                                                            action-item)))
  :action nil
  :module (count-lines-and-actions-of-module system-name module-name)
  :system (awhen (totalize-encompassed-stats (get-from-hierarchical-cache 'line-and-form-counts system-name :in))
            (setf (getf it :line-form-ratio) (line-form-ratio (getf it :line-count)
                                                              (getf it :form-count)))
            (list :stats it))
  :root (system-list-all-loaded))

(defun refresh-line-and-form-counts ()
  (with-hierarchical-cache (:line-and-form-counts :write t)
    (refresh)))