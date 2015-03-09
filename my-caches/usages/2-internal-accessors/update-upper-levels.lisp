("First we verify that upper levels are correctly updated with the cache being empty. Then we verify that these updates integrate with all the data already present"
 ("Events from an internal update, using a key that is not part of the known keys to verify new data gets saved and appended to any previous data"
  ("Level 3"
   (with-cache-example
    (:initialize nil)
    (let ((events (funcall (get-updater cache-name :gamma) "alpha1" "beta1" "gamma3")))
      (and (equal '(:name "alpha1" :in ((:name "beta1" :in ((:name "gamma3" :value 5)))))
                  (get-from-hierarchical-cache cache-name "alpha1"))
           (progn
             (update-upper-levels cache-name events)
             (equal '(:custom "custom1" :name "alpha1" :in ((:constant 1
                                                             :name "beta1" :in ((:name "gamma3" :value 5)))))
                    (get-from-hierarchical-cache cache-name "alpha1"))))))
   (with-cache-example
    (:initialize t)
    (let ((events (funcall (get-updater cache-name :gamma) "alpha1" "beta1" "gamma3"))
          (target-value '(:custom "custom1" :name "alpha1" :in ((:constant 1
                                                                 :name "beta2" :in ((:name "gamma2" :value 5)
                                                                                    (:name "gamma1" :value 4)))
                                                                (:constant 1
                                                                 :name "beta1" :in ((:name "gamma3" :value 5)
                                                                                    (:name "gamma2" :value 4)
                                                                                    (:name "gamma1" :value 3)))))))
      (and (equal target-value
                  (get-from-hierarchical-cache cache-name "alpha1"))
           (progn
             (update-upper-levels cache-name events)
             (equal target-value
                    (get-from-hierarchical-cache cache-name "alpha1")))))))
  ("Level 2"
   (with-cache-example
    (:initialize nil)
    (let ((events (funcall (get-updater cache-name :beta) "alpha1" "beta3")))
      (and (equal '(:name "alpha1" :in ((:name "beta3" :constant 1)))
                  (get-from-hierarchical-cache cache-name "alpha1"))
           (progn
             (update-upper-levels cache-name events)
             (equal '(:custom "custom1" :name "alpha1" :in ((:name "beta3" :constant 1)))
                    (get-from-hierarchical-cache cache-name "alpha1"))))))
   (with-cache-example
    (:initialize t)
    (let ((events (funcall (get-updater cache-name :beta) "alpha1" "beta3"))
          (target-value '(:custom "custom1" :name "alpha1" :in ((:name "beta3" :constant 1)
                                                                (:constant 1
                                                                 :name "beta2" :in ((:name "gamma2" :value 5)
                                                                                    (:name "gamma1" :value 4)))
                                                                (:constant 1
                                                                 :name "beta1" :in ((:name "gamma2" :value 4)
                                                                                    (:name "gamma1" :value 3)))))))
      (and (equal target-value
                  (get-from-hierarchical-cache cache-name "alpha1"))
           (progn
             (update-upper-levels cache-name events)
             (equal target-value
                    (get-from-hierarchical-cache cache-name "alpha1"))))))))
 ("Events from a refresh (API), using necessarily a known key"
  ("Level 3"
   (with-cache-example
    (:initialize nil)
    (let ((events (refresh-hierarchical-cache cache-name "alpha1" "beta1" "gamma1")))
      (and (equal '(:name "alpha1" :in ((:name "beta1" :in ((:value 3 :name "gamma1")))))
                  (get-from-hierarchical-cache cache-name "alpha1"))
           (progn
             (update-upper-levels cache-name events)
             (equal '(:custom "custom1" :name "alpha1" :in ((:constant 1
                                                             :name "beta1" :in ((:value 3 :name "gamma1")))))
                    (get-from-hierarchical-cache cache-name "alpha1"))))))
   (with-cache-example
    (:initialize t)
    (let ((events (refresh-hierarchical-cache cache-name "alpha1" "beta1" "gamma1"))
          (target-value '(:custom "custom1" :name "alpha1" :in ((:constant 1
                                                                 :name "beta2" :in ((:name "gamma2" :value 5)
                                                                                    (:name "gamma1" :value 4)))
                                                                (:constant 1
                                                                 :name "beta1" :in ((:name "gamma2" :value 4)
                                                                                    (:name "gamma1" :value 3)))))))
      (and (equal target-value
                  (get-from-hierarchical-cache cache-name "alpha1"))
           (progn
             (update-upper-levels cache-name events)
             (equal target-value
                    (get-from-hierarchical-cache cache-name "alpha1")))))))
  ("Level 2"
   (with-cache-example
    (:initialize nil)
    (let ((events (refresh-hierarchical-cache cache-name "alpha1" "beta1")))
      (and (equal '(:name "alpha1" :in ((:constant 1
                                         :name "beta1" :in ((:name "gamma2" :value 4) (:name "gamma1" :value 3)))))
                  (get-from-hierarchical-cache cache-name "alpha1"))
           (progn
             (update-upper-levels cache-name events)
             (equal '(:custom "custom1"
                      :name "alpha1" :in ((:constant 1
                                           :name "beta1" :in ((:name "gamma2" :value 4) (:name "gamma1" :value 3)))))
                    (get-from-hierarchical-cache cache-name "alpha1"))))))
   (with-cache-example
    (:initialize t)
    (let ((events (refresh-hierarchical-cache cache-name "alpha1" "beta1"))
          (target-value '(:custom "custom1" :name "alpha1" :in ((:constant 1
                                                                 :name "beta2" :in ((:name "gamma2" :value 5)
                                                                                    (:name "gamma1" :value 4)))
                                                                (:constant 1
                                                                 :name "beta1" :in ((:name "gamma2" :value 4)
                                                                                    (:name "gamma1" :value 3)))))))
      (and (equal target-value
                  (get-from-hierarchical-cache cache-name "alpha1"))
           (progn
             (update-upper-levels cache-name events)
             (equal target-value
                    (get-from-hierarchical-cache cache-name "alpha1")))))))))