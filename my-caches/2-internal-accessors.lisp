(in-package :my-caches)

(defvar *hierarchical-caches* nil)

(defmacro with-cache-example ((&key (initialize t) write) &body body)
  `(flet ((all-alpha ()
            (list "alpha1" "alpha2"))
          (alpha-beta-names (alpha-name)
            (list "beta1" "beta2"))
          (beta-gamma-names (alpha-name beta-name)
            (list "gamma1" "gamma2"))
          (key (name)
            (parse-integer (subseq name (1- (length name))))))
     (let* (my-caches::*hierarchical-caches*
            (cache-name :cache-example)
            (cache-name-string "cache-example")
            (cache-example-directory (merge-pathnames (concatenate 'string cache-name-string "/")
                                                      (usage-temporary-directory)))
            (cache-example-stats-directory (merge-pathnames (concatenate 'string cache-name-string "/stats/")
                                                            (usage-temporary-directory)))
            (cache-example-meta-directory (merge-pathnames (concatenate 'string cache-name-string "/meta/")
                                                           (usage-temporary-directory))))
       (define-hierarchical-cache :cache-example ((cache-example-directory
                                                    cache-example-stats-directory
                                                    cache-example-meta-directory)
                                                   :root (("root-file" (lambda (alpha-items)
                                                                         alpha-items))
                                                          nil
                                                          ("item-count"
                                                           (lambda (alpha-items)
                                                             (loop for alpha-item in alpha-items
                                                                   collect `(:name ,(getf alpha-item :name)
                                                                             :count ,(length (getf alpha-item :in)))))))
                                                   :alpha ((lambda (alpha-item &rest names)
                                                             (key (getf alpha-item :name)))
                                                           nil
                                                           (lambda (alpha-item &rest names)
                                                             (getf alpha-item :name)))
                                                   :beta (nil nil
                                                              (lambda (beta-item &rest names)
                                                                (key (getf beta-item :name))))
                                                   :gamma ((lambda (gamma-item &rest names)
                                                             (declare (ignore names))
                                                             gamma-item)
                                                           (lambda (gamma-item &rest names)
                                                             (list :name (getf gamma-item :name)
                                                                   :value (+ 100 (getf gamma-item :value))))))
         :gamma (list :value (apply #'+ (mapcar #'key (list alpha-name beta-name gamma-name))))
         :beta (list :constant 1)
         :alpha (list :custom (mkstr "custom" (key alpha-name)))
         :root (all-alpha))
       ,(when initialize
          `(progn
             (setf (getf+ my-caches::*hierarchical-caches* cache-name-string :root :in)
                   (list (list :custom "custom2" :name "alpha2"
                               :in (list (list :constant 1
                                               :name "beta2" :in (list (list :name "gamma2" :value 6)
                                                                       (list :name "gamma1" :value 5)))
                                         (list :constant 1
                                               :name "beta1" :in (list (list :name "gamma2" :value 5)
                                                                       (list :name "gamma1" :value 4)))))
                         (list :custom "custom1" :name "alpha1"
                               :in (list (list :constant 1
                                               :name "beta2" :in (list (list :name "gamma2" :value 5)
                                                                       (list :name "gamma1" :value 4)))
                                         (list :constant 1
                                               :name "beta1" :in (list (list :name "gamma2" :value 4)
                                                                       (list :name "gamma1" :value 3)))))))
             ,(when write
                `(flet ((write-alpha-files (alpha-name)
                          (with-open-file (out (ensure-directories-exist
                                                (merge-pathnames (format nil "~a.json" alpha-name)
                                                                 cache-example-directory))
                                               :direction :output
                                               :if-exists :supersede)
                            (write-string (json<-lisp (key alpha-name)) out))
                          (with-open-file (out (ensure-directories-exist
                                                (merge-pathnames (format nil "~a.json" alpha-name)
                                                                 cache-example-meta-directory))
                                               :direction :output
                                               :if-exists :supersede)
                            (write-string (json<-lisp alpha-name) out)))
                        (write-beta-file (alpha-name beta-name)
                          (with-open-file (out (ensure-directories-exist
                                                (merge-pathnames (format nil "~a/~a.json" alpha-name beta-name)
                                                                 cache-example-meta-directory))
                                               :direction :output
                                               :if-exists :supersede)
                            (write-string (json<-lisp (key beta-name)) out)))
                        (write-gamma-files (alpha-name beta-name gamma-name)
                          (with-open-file (out (ensure-directories-exist
                                                (merge-pathnames (format nil "~a/~a/~a.json"
                                                                         alpha-name beta-name gamma-name)
                                                                 cache-example-directory))
                                               :direction :output
                                               :if-exists :supersede)
                            (write-string (json<-lisp
                                           (list :name gamma-name
                                                 :value (apply #'+ (mapcar #'key (list alpha-name beta-name gamma-name)))))
                                          out))
                          (with-open-file (out (ensure-directories-exist
                                                (merge-pathnames (format nil "~a/~a/~a.json"
                                                                         alpha-name beta-name gamma-name)
                                                                 cache-example-stats-directory))
                                               :direction :output
                                               :if-exists :supersede)
                            (write-string
                             (json<-lisp
                              (list :name gamma-name
                                    :value (+ 100
                                              (apply #'+ (mapcar #'key (list alpha-name beta-name gamma-name))))))
                             out))))
                   (with-open-file (out (ensure-directories-exist
                                         (merge-pathnames "root-file.json" cache-example-directory))
                                        :direction :output
                                        :if-exists :supersede)
                     (write-string (json<-lisp
                                    '((:custom "custom2"
                                       :name "alpha2" :in ((:constant 1
                                                            :name "beta2" :in ((:name "gamma2" :value 6)
                                                                               (:name "gamma1" :value 5)))
                                                           (:constant 1
                                                            :name "beta1" :in ((:name "gamma2" :value 5)
                                                                               (:name "gamma1" :value 4)))))
                                      (:custom "custom1"
                                       :name "alpha1" :in ((:constant 1
                                                            :name "beta2" :in ((:name "gamma2" :value 5)
                                                                               (:name "gamma1" :value 4)))
                                                           (:constant 1
                                                            :name "beta1" :in ((:name "gamma2" :value 4)
                                                                               (:name "gamma1" :value 3)))))))
                                   out))
                   (with-open-file (out (ensure-directories-exist
                                         (merge-pathnames "item-count.json" cache-example-meta-directory))
                                        :direction :output
                                        :if-exists :supersede)
                     (write-string (json<-lisp '((:name "alpha2" :count 2) (:name "alpha1" :count 2)))
                                   out))
                   (mapcar #'write-alpha-files (list "alpha1" "alpha2"))
                   (mapcar (lambda (names)
                             (apply #'write-beta-file names))
                           (list (list "alpha1" "beta1")
                                 (list "alpha1" "beta2")
                                 (list "alpha2" "beta1")
                                 (list "alpha2" "beta2")))
                   (mapcar (lambda (names)
                             (apply #'write-gamma-files names))
                           (list (list "alpha1" "beta1" "gamma1")
                                 (list "alpha1" "beta1" "gamma2")
                                 (list "alpha1" "beta2" "gamma1")
                                 (list "alpha1" "beta2" "gamma2")
                                 (list "alpha2" "beta1" "gamma1")
                                 (list "alpha2" "beta1" "gamma2")
                                 (list "alpha2" "beta2" "gamma1")
                                 (list "alpha2" "beta2" "gamma2")))))))
       ,@body)))

(defun normalize-cache-name (cache-name)
  (if (symbolp cache-name)
    (string-downcase (symbol-name cache-name))
    cache-name))

(defun get-cache-keys (cache-name)
  "Get the key list of the cache of given name."
  (getf+ *hierarchical-caches* (normalize-cache-name cache-name) :root :keys))

(defun get-writers (cache-name key)
  "Get the writers of the cache of given name associated to the given key."
  (getf (getf+ *hierarchical-caches* (normalize-cache-name cache-name) :root :writers) key))

(defun get-all-writers (cache-name)
  "Get all writers of the cache of given name in one flat list."
  (flatten (plist-values (getf+ *hierarchical-caches* (normalize-cache-name cache-name) :root :writers))))

(defun get-updater (cache-name key)
  "Get the updater of the cache of given name associated to the given key."
  (getf (getf+ *hierarchical-caches* (normalize-cache-name cache-name) :root :updaters) key))

(defun get-refresher (cache-name key)
  "Get the refresher of the cache of given name associated to the given key."
  (getf (getf+ *hierarchical-caches* (normalize-cache-name cache-name) :root :refreshers) key))

(defun get-deleter (cache-name key)
  "Get the deleter of the cache of given name associated to the given key."
  (getf (getf+ *hierarchical-caches* (normalize-cache-name cache-name) :root :deleters) key))

(defun get-hierarchical-cache-file (directory names)
  "Get the path to the hierarchical cache file related to the given names, with the given directory as the base directory.
A getf-key query can even be passed, only the relevant names will be taken into account."
  (merge-pathnames (format nil "~{~a~^/~}.json" (mapcar (lambda (name)
                                                          (escape-filename (if (symbolp name)
                                                                             (string-downcase (symbol-name name))
                                                                             name)))
                                                        (remove-if #'keywordp names)))
                   directory))

(defun write-hierarchical-cache-file (cache-name preformatter directory names)
  "Write the content identified by the given names from the cache of given name to the appropriate cache file, overriding any previous content.
The given directory is used as the base directory for that file.
The preformatter function is applied to the retrieved data from the cache before being translated to JSON.
The performatter function is also passed the given names so its arguments must be of the form (cache-value &rest names)."
  (with-open-file (s (ensure-directories-exist
                      (get-hierarchical-cache-file directory names))
                     :direction :output
                     :if-exists :supersede)
    (write-string
     (json-preformatter ((first (plist-values names)) :with-system t)
       (apply preformatter
              (rec-getf *hierarchical-caches* (cons (normalize-cache-name cache-name) (cons :root names)))
              (plist-values names)))
     s)))

(defun write-hierarchical-cache-root-file (cache-name preformatter directory name)
  "A root file is identified with only one name but it is working just as a regular cache file, except that this name can be anything because it is not used to retrieve any data from the cache: the whole content is given to the preformatter. This particular formatter's arguments need only be of the form (cache-value)."
  (with-open-file (s (ensure-directories-exist
                      (get-hierarchical-cache-file directory (list name)))
                     :direction :output
                     :if-exists :supersede)
    (write-string
     (json<-lisp
      (funcall preformatter (rec-getf *hierarchical-caches* (list (normalize-cache-name cache-name) :root :in))))
     s)))

(defun delete-hierarchical-cache-file (directory names)
  "Delete the hierarchical cache file, if any, related to the given names, with the given directory the base directory."
  (delete-file (get-hierarchical-cache-file directory (mklist names)) nil))

(defun write-hierarchical-cache (cache-name events)
  "Call every available writer for the cache of given name, giving them the given events so that they write only what is necessary."
  (dolist (writer (get-all-writers cache-name))
    (funcall writer events)))

(defun hierarchise-events (cache-name events)
  (let (result)
    (dolist (event events)
      (let ((key (second event)))
        (setf (getf result key)
              (append (getf result key)
                      (list event)))))
    (loop for key in (reverse (get-cache-keys cache-name))
          collect (getf result key))))

(defun update-upper-levels (cache-name events)
  "Update the upper levels where events show it to be necessary."
  (let-a (hierarchise-events cache-name events)
    (dolist (events it)
      (let ((updater (get-updater cache-name (second (first events)))))
        (dolist (event events)
          (apply updater (third event)))))
    it))