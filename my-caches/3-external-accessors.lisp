(in-package :my-caches)

(defun get-from-hierarchical-cache (cache-name &rest query)
  "Get the data from the cache of given name, following the given query."
  (rec-getf (getf+ *hierarchical-caches* (normalize-cache-name cache-name) :root)
            (complete-query query)))

(defun update-hierarchical-cache (cache-name &rest query)
  "Update the place identified by the query in the cache of given name. A empty query have no effect."
  (awhen query
    (awhen (get-updater cache-name (elt (get-cache-keys cache-name) (1- (length it))))
      (apply it query))))

(defun refresh-hierarchical-cache (cache-name &rest query)
  "Refresh the cache of given name starting at the point identified by the given query."
  (aif query
       (awhen (get-refresher cache-name (elt (get-cache-keys cache-name) (1- (length it))))
         (apply it query))
       (awhen (get-refresher cache-name :root)
         (funcall it))))

(defun delete-in-hierarchical-cache (cache-name &rest query)
  "Delete the point, and all its related data, as identified by the given query from the cache of given name."
  (aif query
       (awhen (get-deleter cache-name (elt (get-cache-keys cache-name) (1- (length it))))
         (apply it query))
       (awhen (get-deleter cache-name :root)
         (funcall it))))

(defmacro with-hierarchical-cache ((cache-name &key write) &body body)
  "key is what indicates to operations what names to use:
root -> systems -> modules
:system => (system-name) is used in order to identify the system
:systems => nothing is used, all systems are targeted
:module => (system-name module-name) are used to identify the module
:modules => (system-name) is used in order to identify the system whose modules are all targeted"
  (with-gensyms (events)
    `(let (,events)
       (flet ((get-events ()
                ,events)
              (get (&rest query)
                (apply #'get-from-hierarchical-cache ',cache-name query))
              (update (&rest query)
                (push-each-new (apply #'update-hierarchical-cache ',cache-name query) ,events))
              (refresh (&rest query)
                (push-each-new (apply #'refresh-hierarchical-cache ',cache-name query) ,events))
              (delete (&rest query)
                (push-each-new (apply #'delete-in-hierarchical-cache ',cache-name query) ,events)))
         (prog1
             (progn
               ,@body)
           (update-upper-levels ',cache-name ,events)
           (when ,write
             (write-hierarchical-cache ',cache-name ,events)))))))

(defmacro with-hierarchical-caches ((cache-names &key write) &body body)
  "key is what indicates to operations what names to use:
root -> systems -> modules
:system => (system-name) is used in order to identify the system
:systems => nothing is used, all systems are targeted
:module => (system-name module-name) are used to identify the module
:modules => (system-name) is used in order to identify the system whose modules are all targeted"
  (with-gensyms (events)
    `(let (,events)
       (flet ((get-events ()
                ,events)
              (get (cache-name &rest query)
                (apply #'get-from-hierarchical-cache cache-name query))
              (update (cache-name &rest query)
                (put-each-new (apply #'update-hierarchical-cache cache-name query) ,events cache-name))
              (refresh (cache-name &rest query)
                (put-each-new (apply #'refresh-hierarchical-cache cache-name query) ,events cache-name))
              (delete (cache-name &rest query)
                (put-each-new (apply #'delete-in-hierarchical-cache cache-name query) ,events cache-name)))
         (prog1
             (progn
               ,@body)
           (dolist (cache-name ',cache-names)
             (awhen (rest (find cache-name ,events :key #'first))
               (update-upper-levels cache-name it)
               (when ,write
                 (write-hierarchical-cache cache-name it)))))))))