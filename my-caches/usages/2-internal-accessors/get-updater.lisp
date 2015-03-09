("Get the updater of the cache of given name associated to the given key."
 ("Nil because..."
  ("... no such property as :updaters"
   (let ((my-caches::*hierarchical-caches* '((:name "cache-example" :root nil))))
     (null (get-updater "cache-example" :alpha))))
  ("... the given key is absent"
   (let ((my-caches::*hierarchical-caches* '((:name "cache-example" :root (:updaters nil)))))
     (null (get-updater "cache-example" :alpha))))
  ("... no associated writers to the given key"
   (let ((my-caches::*hierarchical-caches* '((:name "cache-example" :root (:updaters (:alpha nil :beta nil :gamma nil))))))
     (null (get-updater "cache-example" :alpha)))))
 (with-cache-example
  ()
  (let-a (get-updater cache-name :gamma)
         (and it
              (functionp it)))))