("Get all writers of the cache of given name in one flat list."
 ("Nil because..."
  ("... no such property as :writers"
   (let ((my-caches::*hierarchical-caches* '((:name "cache-example" :root nil))))
     (null (get-all-writers "cache-example"))))
  ("... no key are present"
   (let ((my-caches::*hierarchical-caches* '((:name "cache-example" :root (:writers nil)))))
     (null (get-all-writers "cache-example"))))
  ("... no associated writers to any key"
   (let ((my-caches::*hierarchical-caches* '((:name "cache-example" :root (:writers (:alpha nil :beta nil :gamma nil))))))
     (null (get-all-writers "cache-example")))))
 (with-cache-example
  ()
  (let-a (get-all-writers cache-name)
         (and it
              (listp it)
              (every #'functionp it)))))