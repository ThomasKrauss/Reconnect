("Call every available writer for the cache of given name, giving them the given events so that they write only what is necessary."
 (with-cache-example
  ()
  (write-hierarchical-cache cache-name (funcall (get-refresher cache-name :alpha) "alpha1"))
  (labels ((cache-path (key key-indexes directory-index)
             (let ((cache-directory (ecase directory-index
                                      (1 cache-example-directory)
                                      (2 cache-example-stats-directory)
                                      (3 cache-example-meta-directory))))
               (ecase key
                 (:alpha (merge-pathnames (format nil "alpha~a/" key-indexes) cache-directory))
                 (:alpha-file (merge-pathnames (format nil "alpha~a.json" key-indexes) cache-directory))
                 (:beta (merge-pathnames (format nil "alpha~a/beta~a/" (first key-indexes) (second key-indexes))
                                         cache-directory))
                 (:beta-file (merge-pathnames (format nil "alpha~a/beta~a.json" (first key-indexes) (second key-indexes))
                                              cache-directory))
                 (:gamma (merge-pathnames (format nil "alpha~a/beta~a/gamma~a.json"
                                                  (first key-indexes) (second key-indexes) (third key-indexes))
                                          cache-directory)))))
           (first-cache-path (key key-indexes)
             (cache-path key key-indexes 1))
           (second-cache-path (key key-indexes)
             (cache-path key key-indexes 2))
           (third-cache-path (key key-indexes)
             (cache-path key key-indexes 3))
           (gamma-value (index value)
             (format nil "{\"name\":\"gamma~a\",\"value\":~a}" index value)))
    (and (string= "[{\"custom\":\"custom2\",\"name\":\"alpha2\",\"in\":[{\"constant\":1,\"name\":\"beta2\",\"in\":[{\"name\":\"gamma2\",\"value\":6},{\"name\":\"gamma1\",\"value\":5}]},{\"constant\":1,\"name\":\"beta1\",\"in\":[{\"name\":\"gamma2\",\"value\":5},{\"name\":\"gamma1\",\"value\":4}]}]},{\"custom\":\"custom1\",\"name\":\"alpha1\",\"in\":[{\"constant\":1,\"name\":\"beta2\",\"in\":[{\"name\":\"gamma2\",\"value\":5},{\"name\":\"gamma1\",\"value\":4}]},{\"constant\":1,\"name\":\"beta1\",\"in\":[{\"name\":\"gamma2\",\"value\":4},{\"name\":\"gamma1\",\"value\":3}]}]}]"
                  (load-file-content (file-exists-p (merge-pathnames "root-file.json" cache-example-directory))))
         (string= "[{\"name\":\"alpha2\",\"count\":2},{\"name\":\"alpha1\",\"count\":2}]"
                  (load-file-content (file-exists-p (merge-pathnames "item-count.json" cache-example-meta-directory))))
         (directory-exists-p (first-cache-path :alpha 1))
         (string= "1"
                  (load-file-content (file-exists-p (first-cache-path :alpha-file 1))))
         (not (directory-exists-p (first-cache-path :alpha 2)))
         (not (file-exists-p (first-cache-path :alpha-file 2)))
         (not (directory-exists-p (first-cache-path :alpha 3)))
         (not (file-exists-p (first-cache-path :alpha-file 3)))
         (directory-exists-p (first-cache-path :beta '(1 1)))
         (not (file-exists-p (first-cache-path :beta-file '(1 1))))
         (directory-exists-p (first-cache-path :beta '(1 2)))
         (not (file-exists-p (first-cache-path :beta-file '(1 2))))
         (string= (gamma-value 1 3)
                  (load-file-content (file-exists-p (first-cache-path :gamma '(1 1 1)))))
         (string= (gamma-value 2 4)
                  (load-file-content (file-exists-p (first-cache-path :gamma '(1 1 2)))))
         (string= (gamma-value 1 4)
                  (load-file-content (file-exists-p (first-cache-path :gamma '(1 2 1)))))
         (string= (gamma-value 2 5)
                  (load-file-content (file-exists-p (first-cache-path :gamma '(1 2 2)))))
         (directory-exists-p (second-cache-path :alpha 1))
         (not (file-exists-p (second-cache-path :alpha-file 1)))
         (not (directory-exists-p (second-cache-path :alpha 2)))
         (not (file-exists-p (second-cache-path :alpha-file 2)))
         (not (directory-exists-p (second-cache-path :alpha 3)))
         (not (file-exists-p (second-cache-path :alpha-file 3)))
         (directory-exists-p (second-cache-path :beta '(1 1)))
         (not (file-exists-p (second-cache-path :beta-file '(1 1))))
         (directory-exists-p (second-cache-path :beta '(1 2)))
         (not (file-exists-p (second-cache-path :beta-file '(1 2))))
         (string= (gamma-value 1 103)
                  (load-file-content (file-exists-p (second-cache-path :gamma '(1 1 1)))))
         (string= (gamma-value 2 104)
                  (load-file-content (file-exists-p (second-cache-path :gamma '(1 1 2)))))
         (string= (gamma-value 1 104)
                  (load-file-content (file-exists-p (second-cache-path :gamma '(1 2 1)))))
         (string= (gamma-value 2 105)
                  (load-file-content (file-exists-p (second-cache-path :gamma '(1 2 2)))))
         (directory-exists-p (third-cache-path :alpha 1))
         (string= "\"alpha1\""
                  (load-file-content (file-exists-p (third-cache-path :alpha-file 1))))
         (not (directory-exists-p (third-cache-path :alpha 2)))
         (not (file-exists-p (third-cache-path :alpha-file 2)))
         (not (directory-exists-p (third-cache-path :alpha 3)))
         (not (file-exists-p (third-cache-path :alpha-file 3)))
         (not (directory-exists-p (third-cache-path :beta '(1 1))))
         (not (directory-exists-p (third-cache-path :beta '(1 2))))
         (string= "1"
                  (load-file-content (file-exists-p (third-cache-path :beta-file '(1 1)))))
         (string= "2"
                  (load-file-content (file-exists-p (third-cache-path :beta-file '(1 2)))))))))