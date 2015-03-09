("Get the refresher of the cache of given name associated to the given key."
 ("Nil because..."
  ("... no such property as :refreshers"
   (let ((my-caches::*hierarchical-caches* '((:name "cache-example" :root nil))))
     (null (get-refresher "cache-example" :alpha))))
  ("... the given key is absent"
   (let ((my-caches::*hierarchical-caches* '((:name "cache-example" :root (:refreshers nil)))))
     (null (get-refresher "cache-example" :alpha))))
  ("... no associated writers to the given key"
   (let ((my-caches::*hierarchical-caches* '((:name "cache-example" :root (:refreshers (:alpha nil :beta nil :gamma nil))))))
     (null (get-refresher "cache-example" :alpha)))))
 (with-cache-example
  ()
  (let-a (get-refresher cache-name :gamma)
         (and it
              (functionp it)))))