(""
 (with-cache-example
  ()
  (let ((path (ensure-directories-exist (get-hierarchical-cache-file cache-example-directory '("alpha1" "beta1" "gamma1"))))
        cache-content
        keys)
    (with-open-file (out path :direction :output)
      (write-string "test" out))
    (and (file-exists-p path)
         (progn
           (write-hierarchical-cache-file cache-name
                                          (lambda (gamma-item &rest names)
                                            (setf cache-content gamma-item
                                                  keys names)
                                            (+ 10 (getf gamma-item :value) 0))
                                          cache-example-directory
                                          '(:in "alpha1" :in "beta1" :in "gamma1"))
           (and (equal cache-content '(:name "gamma1" :value 3))
                (equal keys '("alpha1" "beta1" "gamma1"))
                (file-exists-p path)
                (string= "13" (load-file-content path))))))))