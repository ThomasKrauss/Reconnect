(""
 (with-cache-example
  ()
  (let ((path (ensure-directories-exist
               (get-hierarchical-cache-file cache-example-directory '("test-root-file"))))
        cache-content)
    (with-open-file (out path :direction :output)
      (write-string "test" out))
    (and (file-exists-p path)
         (progn
           (write-hierarchical-cache-root-file cache-name
                                               (lambda (whole-cache)
                                                 (setf cache-content whole-cache)
                                                 (list (list :name "alpha1"
                                                             :value (length (getf+ whole-cache "alpha1" :in)))
                                                       (list :name "alpha2"
                                                             :value (length (getf+ whole-cache "alpha2" :in)))))
                                               cache-example-directory
                                               "test-root-file")
           (and (equal cache-content
                       '((:custom "custom2" :name "alpha2"
                          :in ((:constant 1 :name "beta2" :in ((:name "gamma2" :value 6) (:name "gamma1" :value 5)))
                               (:constant 1 :name "beta1" :in ((:name "gamma2" :value 5) (:name "gamma1" :value 4)))))
                         (:custom "custom1" :name "alpha1"
                          :in ((:constant 1 :name "beta2" :in ((:name "gamma2" :value 5) (:name "gamma1" :value 4)))
                               (:constant 1 :name "beta1" :in ((:name "gamma2" :value 4) (:name "gamma1" :value 3)))))))
                (file-exists-p path)
                (string= "[{\"name\":\"alpha1\",\"value\":2},{\"name\":\"alpha2\",\"value\":2}]"
                         (load-file-content path))))))))