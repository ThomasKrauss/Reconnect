(""
 ("Level 1"
  (with-cache-example
   (:initialize nil)
   (and (null (get-from-hierarchical-cache cache-name))
        (progn
          (refresh-hierarchical-cache cache-name "alpha1")
          (equal '((:custom "custom1"
                    :name "alpha1" :in ((:constant 1
                                         :name "beta2" :in ((:name "gamma2" :value 5) (:name "gamma1" :value 4)))
                                        (:constant 1
                                         :name "beta1" :in ((:name "gamma2" :value 4) (:name "gamma1" :value 3))))))
                 (get-from-hierarchical-cache cache-name)))
        (progn
          (refresh-hierarchical-cache cache-name)
          (equal '((:custom "custom2"
                    :name "alpha2" :in ((:constant 1
                                         :name "beta2" :in ((:name "gamma2" :value 6) (:name "gamma1" :value 5)))
                                        (:constant 1
                                         :name "beta1" :in ((:name "gamma2" :value 5) (:name "gamma1" :value 4)))))
                   (:custom "custom1"
                    :name "alpha1" :in ((:constant 1
                                         :name "beta2" :in ((:name "gamma2" :value 5) (:name "gamma1" :value 4)))
                                        (:constant 1
                                         :name "beta1" :in ((:name "gamma2" :value 4) (:name "gamma1" :value 3))))))
                 (get-from-hierarchical-cache cache-name))))))
 ("Level 2"
  (with-cache-example
   (:initialize nil)
   (and (null (get-from-hierarchical-cache cache-name :in))
        (progn
          (refresh-hierarchical-cache cache-name "alpha1" "beta1")
          (equal '((:name "alpha1" :in ((:constant 1
                                         :name "beta1" :in ((:name "gamma2" :value 4) (:name "gamma1" :value 3))))))
                 (get-from-hierarchical-cache cache-name)))
        (progn
          (refresh-hierarchical-cache cache-name "alpha1")
          (equal '((:custom "custom1"
                    :name "alpha1" :in ((:constant 1
                                         :name "beta2" :in ((:name "gamma2" :value 5) (:name "gamma1" :value 4)))
                                        (:constant 1
                                         :name "beta1" :in ((:name "gamma2" :value 4) (:name "gamma1" :value 3))))))
                 (get-from-hierarchical-cache cache-name))))))
 ("Level 3"
  (with-cache-example
   (:initialize nil)
   (and (null (get-from-hierarchical-cache cache-name :in))
        (progn
          (refresh-hierarchical-cache cache-name "alpha1" "beta1" "gamma1")
          (equal '((:name "alpha1" :in ((:name "beta1" :in ((:value 3 :name "gamma1"))))))
                 (get-from-hierarchical-cache cache-name)))
        (progn
          (refresh-hierarchical-cache cache-name "alpha1" "beta1")
          (equal '((:name "alpha1" :in ((:constant 1
                                         :name "beta1" :in ((:name "gamma2" :value 4) (:name "gamma1" :value 3))))))
                 (get-from-hierarchical-cache cache-name)))))))