(""
 (with-cache-example
  ()
  (and *hierarchical-caches*
       (listp *hierarchical-caches*)
       (= 1 (length *hierarchical-caches*))
       (first *hierarchical-caches*)
       (plistp (first *hierarchical-caches*))
       (string= cache-name-string (getf (first *hierarchical-caches*) :name))
       (getf (first *hierarchical-caches*) :root)
       (equal '(:alpha :beta :gamma) (getf (getf (first *hierarchical-caches*) :root) :keys))
       (every (lambda (property)
                (not (null (getf (getf (first *hierarchical-caches*) :root) property))))
              '(:updaters :refreshers :deleters :writers)))))