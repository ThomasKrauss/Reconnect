(in-package :my-caches)

(defun make-writer (cache-name key-list directory key fn)
  (with-gensyms (event events name to-write-names to-delete-names)
    `(lambda (,events)
       (let (,to-write-names ,to-delete-names)
         (dolist (,event ,events)
           (when (equal ,key (second ,event))
             (case (first ,event)
               ((:add :update)
                (pushnew (to-getf-keys (third ,event) ',key-list) ,to-write-names
                         :test #'equal))
               (:delete
                (pushnew (to-getf-keys (third ,event) ',key-list) ,to-delete-names
                         :test #'equal)))))
         (dolist (,name ,to-write-names)
           (write-hierarchical-cache-file ',cache-name #',(if (eq t fn)
                                                            'identity-ignore-rest
                                                            fn)
                                          ,directory ,name))
         (dolist (,name ,to-delete-names)
           (delete-hierarchical-cache-file ,directory ,name))))))

(defun make-root-writer (cache-name name directory fn)
  (with-gensyms (events)
    `(lambda (,events)
       (when ,events
         (write-hierarchical-cache-root-file ',cache-name #',(if (eq t fn)
                                                               'identity
                                                               fn)
                                             ,directory ,name)))))

(defun make-updater (cache-name key-list updater)
  (let ((names (key-name-hierarchy (first updater) key-list)))
    `(lambda ,names
       (let ((*setf-named-plist-indexes* nil))
         (setf (getf+ *hierarchical-caches* ',cache-name :root
                      ,@(key-getf-hierarchy (first updater) key-list))
               (plist-overwrite
                (getf+ *hierarchical-caches* ',cache-name :root
                       ,@(key-getf-hierarchy (first updater) key-list))
                ,(second updater)))
         (loop for key in ',(key-hierarchy (first updater) key-list)
               for position in (reverse (butlast *setf-named-plist-indexes*))
               for count from 1
               collect (list (if position
                               :update
                               :add)
                             key
                             (subseq (list ,@names) 0 count)))))))

(defun make-refresher (cache-name key-list key &optional init-form)
  (let ((names (key-name-hierarchy key key-list)))
    (labels ((next-key (key)
               (aif (position key key-list)
                    (when (< (1+ it) (length key-list))
                      (elt key-list (1+ it)))
                    (first key-list)))
             (key-getter-name (key)
               (symb key '- (key-name (next-key key)) 's))
             (inner-iterate-over-keys (key &optional init-form)
               (let ((next-key (next-key key)))
                 (when next-key
                   `(let ((updater (get-updater ',cache-name ,next-key)))
                      ,(append-as-list
                        `(dolist (,(key-name next-key) ,(if init-form
                                                          init-form
                                                          `(,(key-getter-name key)
                                                            ,@(nreverse (mapcar #'key-name (key-hierarchy key key-list)))))))
                        (inner-iterate-over-keys next-key)
                        `(push-each-new (apply updater ,(cons 'list (key-name-hierarchy next-key key-list))) events)))))))
      (append-as-list
       `(lambda ,names)
       `(setf (getf+ *hierarchical-caches* ',cache-name :root
                     ,@(aif (next-key key)
                            (butlast (key-getf-hierarchy it key-list))
                            (key-getf-hierarchy key key-list)))
              nil)
       (append-as-list
        `(let (events))
        (inner-iterate-over-keys key init-form)
        (unless (equal :root key)
          `(push-each-new (apply (get-updater ',cache-name ,key) ,(cons 'list names)) events))
        'events)))))

(defun make-deleter (cache-name key-list key &optional init-form)
  (let ((names (key-name-hierarchy key key-list)))
    (labels ((next-key (key)
               (aif (position key key-list)
                    (when (< (1+ it) (length key-list))
                      (elt key-list (1+ it)))
                    (first key-list)))
             (key-getter-name (key)
               (symb key '- (key-name (next-key key)) 's))
             (inner-iterate-over-keys (key &optional init-form)
               (let ((next-key (next-key key)))
                 (when next-key
                   (append-as-list
                    `(dolist (,(key-name next-key) ,(if init-form
                                                      init-form
                                                      `(,(key-getter-name key)
                                                        ,@(nreverse (mapcar #'key-name (key-hierarchy key key-list)))))))
                    (inner-iterate-over-keys next-key)
                    `(push (list :delete
                                 ,next-key
                                 (list ,@(key-name-hierarchy next-key key-list)))
                           events))))))
      (append-as-list
       `(lambda ,names)
       (append-as-list
        `(let (events))
        (if (eq :root key)
          `(setf (getf+ *hierarchical-caches* ',cache-name :root :in) nil)
          `(setf (getf+ *hierarchical-caches* ',cache-name :root
                        ,@(butlast (key-getf-hierarchy key key-list)))
                 (remove-if (lambda (titled-plist)
                              (string= ,(first (last names)) (getf titled-plist :name)))
                            (getf+ *hierarchical-caches* ',cache-name :root
                                   ,@(butlast (key-getf-hierarchy key key-list))))))
        (unless (eq :root key)
          `(push (list :delete ,key (list ,@(key-name-hierarchy key key-list))) events))
        (inner-iterate-over-keys key init-form)
        'events)))))

(defun parse-preformatter-list (preformatter-list)
  (let (preformatters definitions)
    (loop for (key value) in (group preformatter-list 2)
          do (let-a (loop for fn in (if (eq :root key)
                                      (mapcar #'second value)
                                      value)
                          for index from 0
                          if (atom fn)
                          collect fn
                          else
                          if (eq (first fn) 'lambda)
                          collect (let ((symbol (gensym (mkstr key "-" index "-fn"))))
                                    (push `(,symbol ,@(rest fn)) definitions)
                                    symbol))
               (unless (every #'null it)
                 (push (if (eq :root key)
                         (list key (map 'list #'list (mapcar #'first value) it))
                         (list key it))
                       preformatters))))
    (values (nreverse preformatters)
            definitions)))

(defun parse-directory-list (directories)
  (let (symbols definitions)
    (dolist (directory directories)
      (push (if (atom directory)
              directory
              (let ((symbol (gensym)))
                (push `(,symbol ,directory) definitions)
                symbol))
            symbols))
    (values (nreverse symbols) definitions)))

(defun pre-check (directories preformatter-list)
  (let ((directory-list-length (length directories))
        (preformatter-list-length (aif (mapcar (lambda (item)
                                                 (length (second item)))
                                               preformatter-list)
                                       (apply #'max it)
                                       0)))
    (cond
     ((< directory-list-length preformatter-list-length)
      (if (= 0 directory-list-length)
        (warn "No output directory defined while as much as ~R preformatter~:*~[~;~:;s~] ~:*~[is~;is~:;are~] defined."
              preformatter-list-length)
        (warn "Only ~R output director~:*~[y~;y~:;ies~] defined while as much as ~R preformatters are defined."
              directory-list-length preformatter-list-length)))
     ((> directory-list-length preformatter-list-length)
      (if (= 0 preformatter-list-length)
        (warn "No preformatter defined while ~R director~:*~[y~;y~:;ies~] ~:*~[is~;is~:;are~] defined."
              directory-list-length)
        (warn "Only as much as ~R preformatter~:*~[~;~:;s~] defined while ~R directories are defined."
              preformatter-list-length directory-list-length))))))

(defmacro define-hierarchical-cache (cache-name (directories &rest preformatters) &body updaters)
  "name is a symbol refreshers is (key &rest fn)"
  (let* ((cache-name (normalize-cache-name cache-name))
         (preformatters (plist<-list preformatters))
         (key-list (remove-if (lambda (key)
                                (eq key :root))
                              (mapcar #'first (group preformatters 2))))
         (updaters (group updaters 2)))
    (multiple-value-bind
        (preformatter-list preformatter-definitions)
        (parse-preformatter-list preformatters)
      (multiple-value-bind
          (directories directory-definitions)
          (parse-directory-list directories)
        (pre-check directories preformatter-list)
        (flet ((at (key)
                 `(getf+ *hierarchical-caches* ,cache-name :root ,key)))
          `(flet ,preformatter-definitions
             (let ,directory-definitions
               (setf ,(at :keys) ',key-list
                     ,(at :writers) (list
                                     ,@(loop for (key preformatters) in preformatter-list
                                             collect key
                                             if (eq :root key)
                                             collect `(list
                                                       ,@(loop for directory in directories
                                                               for (name fn) in preformatters
                                                               when fn
                                                               collect (make-root-writer cache-name name directory fn)))
                                             else
                                             collect `(list
                                                       ,@(loop for directory in directories
                                                               for fn in preformatters
                                                               when fn
                                                               collect (make-writer cache-name key-list directory
                                                                                    key fn)))))
                     ,(at :refreshers) (list
                                        ,@(loop for updater in updaters
                                                if (equal :root (first updater))
                                                collect :root
                                                and
                                                collect (make-refresher cache-name key-list :root (second updater))
                                                else
                                                collect (first updater)
                                                and
                                                collect (make-refresher cache-name key-list (first updater))))
                     ,(at :updaters) (list
                                      ,@(loop for updater in updaters
                                              unless (equal :root (first updater))
                                              collect (first updater)
                                              and
                                              collect (make-updater cache-name key-list updater)))
                     ,(at :deleters) (list
                                      ,@(append `(:root ,(make-deleter cache-name key-list :root
                                                                       (second (find :root updaters :key #'first))))
                                                (loop for key in key-list
                                                      collect key
                                                      collect (make-deleter cache-name key-list key)))))
               ,cache-name)))))))
