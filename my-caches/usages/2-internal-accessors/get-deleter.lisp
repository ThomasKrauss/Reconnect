("Get the deleter of the cache of given name associated to the given key."
 ("Nil because..."
  ("... no such property as :deleters"
   (let ((my-caches::*hierarchical-caches* '((:name "cache-example" :root nil))))
     (null (get-deleter "cache-example" :alpha))))
  ("... the given key is absent"
   (let ((my-caches::*hierarchical-caches* '((:name "cache-example" :root (:deleters nil)))))
     (null (get-deleter "cache-example" :alpha))))
  ("... no associated writers to the given key"
   (let ((my-caches::*hierarchical-caches* '((:name "cache-example" :root (:deleters (:alpha nil :beta nil :gamma nil))))))
     (null (get-deleter "cache-example" :alpha)))))
 (with-cache-example
  ()
  (let-a (get-deleter cache-name :gamma)
         (and it
              (functionp it)))))