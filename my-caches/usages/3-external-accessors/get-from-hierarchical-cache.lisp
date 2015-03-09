(""
 (with-cache-example
  ()
  (and (equal "gamma1"
              (get-from-hierarchical-cache cache-name "alpha1" "beta1" "gamma1" :name))
       (equal 3
              (get-from-hierarchical-cache cache-name "alpha1" "beta1" "gamma1" :value))
       (equal '(:name "gamma1" :value 3)
              (get-from-hierarchical-cache cache-name "alpha1" "beta1" "gamma1"))
       (equal '((:name "gamma2" :value 4) (:name "gamma1" :value 3))
              (get-from-hierarchical-cache cache-name "alpha1" "beta1" :in))
       (equal '(:constant 1 :name "beta1" :in ((:name "gamma2" :value 4) (:name "gamma1" :value 3)))
              (get-from-hierarchical-cache cache-name "alpha1" "beta1"))
       (equal '((:constant 1 :name "beta2" :in ((:name "gamma2" :value 5) (:name "gamma1" :value 4)))
                (:constant 1 :name "beta1" :in ((:name "gamma2" :value 4) (:name "gamma1" :value 3))))
              (get-from-hierarchical-cache cache-name "alpha1" :in))
       (equal '(:custom "custom1"
                :name "alpha1" :in ((:constant 1
                                     :name "beta2" :in ((:name "gamma2" :value 5) (:name "gamma1" :value 4)))
                                    (:constant 1
                                     :name "beta1" :in ((:name "gamma2" :value 4) (:name "gamma1" :value 3)))))
              (get-from-hierarchical-cache cache-name "alpha1"))
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
              (get-from-hierarchical-cache cache-name :in))
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
              (get-from-hierarchical-cache cache-name)))))