("Get the writers of the cache of given name associated to the given key."
 ("Nil because..."
  ("... no such property as :writers"
   (let ((my-caches::*hierarchical-caches* '((:name "cache-example" :root nil))))
     (null (get-writers "cache-example" :alpha))))
  ("... the given key is absent"
   (let ((my-caches::*hierarchical-caches* '((:name "cache-example" :root (:writers nil)))))
     (null (get-writers "cache-example" :alpha))))
  ("... no associated writers to the given key"
   (let ((my-caches::*hierarchical-caches* '((:name "cache-example" :root (:writers (:alpha nil :beta nil :gamma nil))))))
     (null (get-writers "cache-example" :alpha)))))
 (with-cache-example
  ()
  (let-a (get-writers cache-name :gamma)
         (and it
              (listp it)
              (every #'functionp it)))))