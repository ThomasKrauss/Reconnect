(""
 ("Level 1"
  (with-cache-example
   (:initialize nil)
   (and (null (get-from-hierarchical-cache cache-name))
        (progn
          (update-hierarchical-cache cache-name "alpha1")
          (equal '((:name "alpha1" :custom "custom1"))
                 (get-from-hierarchical-cache cache-name)))
        (progn
          (update-hierarchical-cache cache-name)
          (equal '((:name "alpha1" :custom "custom1"))
                 (get-from-hierarchical-cache cache-name))))))
 ("Level 2"
  (with-cache-example
   (:initialize nil)
   (and (null (get-from-hierarchical-cache cache-name :in))
        (progn
          (update-hierarchical-cache cache-name "alpha1" "beta1")
          (equal '((:name "alpha1" :in ((:name "beta1" :constant 1))))
                 (get-from-hierarchical-cache cache-name)))
        (progn
          (update-hierarchical-cache cache-name "alpha1")
          (equal '((:custom "custom1" :name "alpha1" :in ((:name "beta1" :constant 1))))
                 (get-from-hierarchical-cache cache-name))))))
 ("Level 3"
  (with-cache-example
   (:initialize nil)
   (and (null (get-from-hierarchical-cache cache-name :in))
        (progn
          (update-hierarchical-cache cache-name "alpha1" "beta1" "gamma1")
          (equal '((:name "alpha1" :in ((:name "beta1" :in ((:name "gamma1" :value 3))))))
                 (get-from-hierarchical-cache cache-name)))
        (progn
          (update-hierarchical-cache cache-name "alpha1" "beta1")
          (equal '((:name "alpha1" :in ((:constant 1
                                         :name "beta1" :in ((:name "gamma1" :value 3))))))
                 (get-from-hierarchical-cache cache-name)))))))