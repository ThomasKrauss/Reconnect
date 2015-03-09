(""
 ("Get"
  (with-cache-example
   ()
   (with-hierarchical-cache
    ("cache-example")
    (and (equal "gamma1"
                (get "alpha1" "beta1" "gamma1" :name))
         (equal 3
                (get "alpha1" "beta1" "gamma1" :value))
         (equal '(:name "gamma1" :value 3)
                (get "alpha1" "beta1" "gamma1"))
         (equal '((:name "gamma2" :value 4) (:name "gamma1" :value 3))
                (get "alpha1" "beta1" :in))
         (equal '(:constant 1 :name "beta1" :in ((:name "gamma2" :value 4) (:name "gamma1" :value 3)))
                (get "alpha1" "beta1"))
         (equal '((:constant 1 :name "beta2" :in ((:name "gamma2" :value 5) (:name "gamma1" :value 4)))
                  (:constant 1 :name "beta1" :in ((:name "gamma2" :value 4) (:name "gamma1" :value 3))))
                (get "alpha1" :in))
         (equal '(:custom "custom1"
                  :name "alpha1" :in ((:constant 1
                                       :name "beta2" :in ((:name "gamma2" :value 5) (:name "gamma1" :value 4)))
                                      (:constant 1
                                       :name "beta1" :in ((:name "gamma2" :value 4) (:name "gamma1" :value 3)))))
                (get "alpha1"))
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
                (get :in))
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
                (get))))))
 ("Update"
  ("Query only, writing not requested"
   ("Level 1"
    (with-cache-example
     (:initialize nil)
     (with-hierarchical-cache
      ("cache-example")
      (and (null (get))
           (progn
             (update "alpha1")
             (equal '((:name "alpha1" :custom "custom1"))
                    (get)))))))
   ("Level 2"
    (with-cache-example
     (:initialize nil)
     (and (null (get-from-hierarchical-cache cache-name))
          (with-hierarchical-cache
           ("cache-example")
           (update "alpha1" "beta1")
           (equal '((:name "alpha1" :in ((:name "beta1" :constant 1))))
                  (get)))
          (equal '((:custom "custom1" :name "alpha1" :in ((:name "beta1" :constant 1))))
                 (get-from-hierarchical-cache cache-name)))))
   ("Level 3"
    (with-cache-example
     (:initialize nil)
     (and (null (get-from-hierarchical-cache cache-name))
          (with-hierarchical-cache
           ("cache-example")
           (update "alpha1" "beta1" "gamma1")
           (equal '((:name "alpha1" :in ((:name "beta1" :in ((:name "gamma1" :value 3))))))
                  (get)))
          (equal '((:custom "custom1" :name "alpha1" :in ((:constant 1
                                                           :name "beta1" :in ((:name "gamma1" :value 3))))))
                 (get-from-hierarchical-cache cache-name))))))
  ("Writing requested"
   (with-cache-example
    (:initialize nil)
    (flet ((cache-path (key key-indexes directory-index)
             (let ((cache-directory (ecase directory-index
                                      (1 cache-example-directory)
                                      (2 cache-example-stats-directory)
                                      (3 cache-example-meta-directory))))
               (ecase key
                 (:alpha-file (merge-pathnames (format nil "alpha~a.json" key-indexes) cache-directory))
                 (:beta-file (merge-pathnames (format nil "alpha~a/beta~a.json" (first key-indexes) (second key-indexes))
                                              cache-directory))
                 (:gamma (merge-pathnames (format nil "alpha~a/beta~a/gamma~a.json"
                                                  (first key-indexes) (second key-indexes) (third key-indexes))
                                          cache-directory))))))
      (and (not (file-exists-p (merge-pathnames "root-file.json" cache-example-directory)))
           (not (file-exists-p (merge-pathnames "item-count.json" cache-example-meta-directory)))
           (not (file-exists-p (cache-path :alpha-file 1 1)))
           (not (file-exists-p (cache-path :alpha-file 1 3)))
           (not (file-exists-p (cache-path :beta-file '(1 1) 3)))
           (not (file-exists-p (cache-path :beta-file '(1 2) 3)))
           (not (file-exists-p (cache-path :gamma '(1 1 1) 1)))
           (not (file-exists-p (cache-path :gamma '(1 1 1) 2)))
           (not (file-exists-p (cache-path :gamma '(1 1 2) 1)))
           (not (file-exists-p (cache-path :gamma '(1 1 2) 2)))
           (not (file-exists-p (cache-path :gamma '(1 2 1) 1)))
           (not (file-exists-p (cache-path :gamma '(1 2 1) 2)))
           (not (file-exists-p (cache-path :gamma '(1 2 2) 1)))
           (not (file-exists-p (cache-path :gamma '(1 2 2) 2)))
           (not (file-exists-p (cache-path :alpha-file 2 1)))
           (not (file-exists-p (cache-path :alpha-file 2 3)))
           (not (file-exists-p (cache-path :beta-file '(2 1) 3)))
           (not (file-exists-p (cache-path :beta-file '(2 2) 3)))
           (not (file-exists-p (cache-path :gamma '(2 1 1) 1)))
           (not (file-exists-p (cache-path :gamma '(2 1 1) 2)))
           (not (file-exists-p (cache-path :gamma '(2 1 2) 1)))
           (not (file-exists-p (cache-path :gamma '(2 1 2) 2)))
           (not (file-exists-p (cache-path :gamma '(2 2 1) 1)))
           (not (file-exists-p (cache-path :gamma '(2 2 1) 2)))
           (not (file-exists-p (cache-path :gamma '(2 2 2) 1)))
           (not (file-exists-p (cache-path :gamma '(2 2 2) 2)))
           (progn
             (with-hierarchical-cache
              ("cache-example" :write t)
              (update "alpha1" "beta1" "gamma1"))
             (and (file-exists-p (merge-pathnames "root-file.json" cache-example-directory))
                  (file-exists-p (merge-pathnames "item-count.json" cache-example-meta-directory))
                  (file-exists-p (cache-path :alpha-file 1 1))
                  (file-exists-p (cache-path :alpha-file 1 3))
                  (file-exists-p (cache-path :beta-file '(1 1) 3))
                  (not (file-exists-p (cache-path :beta-file '(1 2) 3)))
                  (file-exists-p (cache-path :gamma '(1 1 1) 1))
                  (file-exists-p (cache-path :gamma '(1 1 1) 2))
                  (not (file-exists-p (cache-path :gamma '(1 1 2) 1)))
                  (not (file-exists-p (cache-path :gamma '(1 1 2) 2)))
                  (not (file-exists-p (cache-path :gamma '(1 2 1) 1)))
                  (not (file-exists-p (cache-path :gamma '(1 2 1) 2)))
                  (not (file-exists-p (cache-path :gamma '(1 2 2) 1)))
                  (not (file-exists-p (cache-path :gamma '(1 2 2) 2)))
                  (not (file-exists-p (cache-path :alpha-file 2 1)))
                  (not (file-exists-p (cache-path :alpha-file 2 3)))
                  (not (file-exists-p (cache-path :beta-file '(2 1) 3)))
                  (not (file-exists-p (cache-path :beta-file '(2 2) 3)))
                  (not (file-exists-p (cache-path :gamma '(2 1 1) 1)))
                  (not (file-exists-p (cache-path :gamma '(2 1 1) 2)))
                  (not (file-exists-p (cache-path :gamma '(2 1 2) 1)))
                  (not (file-exists-p (cache-path :gamma '(2 1 2) 2)))
                  (not (file-exists-p (cache-path :gamma '(2 2 1) 1)))
                  (not (file-exists-p (cache-path :gamma '(2 2 1) 2)))
                  (not (file-exists-p (cache-path :gamma '(2 2 2) 1)))
                  (not (file-exists-p (cache-path :gamma '(2 2 2) 2)))))
           (progn
             (with-hierarchical-cache
              ("cache-example" :write t)
              (update "alpha1" "beta1"))
             (and (file-exists-p (merge-pathnames "root-file.json" cache-example-directory))
                  (file-exists-p (merge-pathnames "item-count.json" cache-example-meta-directory))
                  (file-exists-p (cache-path :alpha-file 1 1))
                  (file-exists-p (cache-path :alpha-file 1 3))
                  (file-exists-p (cache-path :beta-file '(1 1) 3))
                  (not (file-exists-p (cache-path :beta-file '(1 2) 3)))
                  (file-exists-p (cache-path :gamma '(1 1 1) 1))
                  (file-exists-p (cache-path :gamma '(1 1 1) 2))
                  (not (file-exists-p (cache-path :gamma '(1 1 2) 1)))
                  (not (file-exists-p (cache-path :gamma '(1 1 2) 2)))
                  (not (file-exists-p (cache-path :gamma '(1 2 1) 1)))
                  (not (file-exists-p (cache-path :gamma '(1 2 1) 2)))
                  (not (file-exists-p (cache-path :gamma '(1 2 2) 1)))
                  (not (file-exists-p (cache-path :gamma '(1 2 2) 2)))
                  (not (file-exists-p (cache-path :alpha-file 2 1)))
                  (not (file-exists-p (cache-path :alpha-file 2 3)))
                  (not (file-exists-p (cache-path :beta-file '(2 1) 3)))
                  (not (file-exists-p (cache-path :beta-file '(2 2) 3)))
                  (not (file-exists-p (cache-path :gamma '(2 1 1) 1)))
                  (not (file-exists-p (cache-path :gamma '(2 1 1) 2)))
                  (not (file-exists-p (cache-path :gamma '(2 1 2) 1)))
                  (not (file-exists-p (cache-path :gamma '(2 1 2) 2)))
                  (not (file-exists-p (cache-path :gamma '(2 2 1) 1)))
                  (not (file-exists-p (cache-path :gamma '(2 2 1) 2)))
                  (not (file-exists-p (cache-path :gamma '(2 2 2) 1)))
                  (not (file-exists-p (cache-path :gamma '(2 2 2) 2)))))
           (progn
             (with-hierarchical-cache
              ("cache-example" :write t)
              (update "alpha1"))
             (and (file-exists-p (merge-pathnames "root-file.json" cache-example-directory))
                  (file-exists-p (merge-pathnames "item-count.json" cache-example-meta-directory))
                  (file-exists-p (cache-path :alpha-file 1 1))
                  (file-exists-p (cache-path :alpha-file 1 3))
                  (file-exists-p (cache-path :beta-file '(1 1) 3))
                  (not (file-exists-p (cache-path :beta-file '(1 2) 3)))
                  (file-exists-p (cache-path :gamma '(1 1 1) 1))
                  (file-exists-p (cache-path :gamma '(1 1 1) 2))
                  (not (file-exists-p (cache-path :gamma '(1 1 2) 1)))
                  (not (file-exists-p (cache-path :gamma '(1 1 2) 2)))
                  (not (file-exists-p (cache-path :gamma '(1 2 1) 1)))
                  (not (file-exists-p (cache-path :gamma '(1 2 1) 2)))
                  (not (file-exists-p (cache-path :gamma '(1 2 2) 1)))
                  (not (file-exists-p (cache-path :gamma '(1 2 2) 2)))
                  (not (file-exists-p (cache-path :alpha-file 2 1)))
                  (not (file-exists-p (cache-path :alpha-file 2 3)))
                  (not (file-exists-p (cache-path :beta-file '(2 1) 3)))
                  (not (file-exists-p (cache-path :beta-file '(2 2) 3)))
                  (not (file-exists-p (cache-path :gamma '(2 1 1) 1)))
                  (not (file-exists-p (cache-path :gamma '(2 1 1) 2)))
                  (not (file-exists-p (cache-path :gamma '(2 1 2) 1)))
                  (not (file-exists-p (cache-path :gamma '(2 1 2) 2)))
                  (not (file-exists-p (cache-path :gamma '(2 2 1) 1)))
                  (not (file-exists-p (cache-path :gamma '(2 2 1) 2)))
                  (not (file-exists-p (cache-path :gamma '(2 2 2) 1)))
                  (not (file-exists-p (cache-path :gamma '(2 2 2) 2)))))
           (progn
             (with-hierarchical-cache
              ("cache-example" :write t)
              (update))
             (and (file-exists-p (merge-pathnames "root-file.json" cache-example-directory))
                  (file-exists-p (merge-pathnames "item-count.json" cache-example-meta-directory))
                  (file-exists-p (cache-path :alpha-file 1 1))
                  (file-exists-p (cache-path :alpha-file 1 3))
                  (file-exists-p (cache-path :beta-file '(1 1) 3))
                  (not (file-exists-p (cache-path :beta-file '(1 2) 3)))
                  (file-exists-p (cache-path :gamma '(1 1 1) 1))
                  (file-exists-p (cache-path :gamma '(1 1 1) 2))
                  (not (file-exists-p (cache-path :gamma '(1 1 2) 1)))
                  (not (file-exists-p (cache-path :gamma '(1 1 2) 2)))
                  (not (file-exists-p (cache-path :gamma '(1 2 1) 1)))
                  (not (file-exists-p (cache-path :gamma '(1 2 1) 2)))
                  (not (file-exists-p (cache-path :gamma '(1 2 2) 1)))
                  (not (file-exists-p (cache-path :gamma '(1 2 2) 2)))
                  (not (file-exists-p (cache-path :alpha-file 2 1)))
                  (not (file-exists-p (cache-path :alpha-file 2 3)))
                  (not (file-exists-p (cache-path :beta-file '(2 1) 3)))
                  (not (file-exists-p (cache-path :beta-file '(2 2) 3)))
                  (not (file-exists-p (cache-path :gamma '(2 1 1) 1)))
                  (not (file-exists-p (cache-path :gamma '(2 1 1) 2)))
                  (not (file-exists-p (cache-path :gamma '(2 1 2) 1)))
                  (not (file-exists-p (cache-path :gamma '(2 1 2) 2)))
                  (not (file-exists-p (cache-path :gamma '(2 2 1) 1)))
                  (not (file-exists-p (cache-path :gamma '(2 2 1) 2)))
                  (not (file-exists-p (cache-path :gamma '(2 2 2) 1)))
                  (not (file-exists-p (cache-path :gamma '(2 2 2) 2)))
                  t)))))))
 ("Refresh"
  ("Query only, writing not requested"
   ("Level 1"
    (with-cache-example
     (:initialize nil)
     (with-hierarchical-cache
      ("cache-example")
      (and (null (get))
           (progn
             (refresh "alpha1")
             (equal '((:custom "custom1"
                       :name "alpha1" :in ((:constant 1
                                            :name "beta2" :in ((:name "gamma2" :value 5) (:name "gamma1" :value 4)))
                                           (:constant 1
                                            :name "beta1" :in ((:name "gamma2" :value 4) (:name "gamma1" :value 3))))))
                    (get)))
           (progn
             (refresh)
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
                    (get)))))))
   ("Level 2"
    (with-cache-example
     (:initialize nil)
     (with-hierarchical-cache
      ("cache-example")
      (and (null (get :in))
           (progn
             (refresh "alpha1" "beta1")
             (equal '((:name "alpha1" :in ((:constant 1
                                            :name "beta1" :in ((:name "gamma2" :value 4) (:name "gamma1" :value 3))))))
                    (get)))
           (progn
             (refresh "alpha1")
             (equal '((:custom "custom1"
                       :name "alpha1" :in ((:constant 1
                                            :name "beta2" :in ((:name "gamma2" :value 5) (:name "gamma1" :value 4)))
                                           (:constant 1
                                            :name "beta1" :in ((:name "gamma2" :value 4) (:name "gamma1" :value 3))))))
                    (get))))))
    (with-cache-example
     (:initialize nil)
     (and (null (get-from-hierarchical-cache cache-name :in))
          (with-hierarchical-cache
           ("cache-example")
           (refresh "alpha1" "beta1")
           (equal '((:name "alpha1" :in ((:constant 1
                                          :name "beta1" :in ((:name "gamma2" :value 4) (:name "gamma1" :value 3))))))
                  (get)))
          (equal '((:custom "custom1"
                    :name "alpha1" :in ((:constant 1
                                         :name "beta1" :in ((:name "gamma2" :value 4) (:name "gamma1" :value 3))))))
                 (get-from-hierarchical-cache cache-name)))))
   ("Level 3"
    (with-cache-example
     (:initialize nil)
     (with-hierarchical-cache
      ("cache-example")
      (and (null (get :in))
           (progn
             (refresh "alpha1" "beta1" "gamma1")
             (equal '((:name "alpha1" :in ((:name "beta1" :in ((:value 3 :name "gamma1"))))))
                    (get)))
           (progn
             (refresh "alpha1" "beta1")
             (equal '((:name "alpha1" :in ((:constant 1
                                            :name "beta1" :in ((:name "gamma2" :value 4) (:name "gamma1" :value 3))))))
                    (get))))))
    (with-cache-example
     (:initialize nil)
     (and (null (get-from-hierarchical-cache cache-name :in))
          (with-hierarchical-cache
           ("cache-example")
           (refresh "alpha1" "beta1" "gamma1")
           (equal '((:name "alpha1" :in ((:name "beta1" :in ((:value 3 :name "gamma1"))))))
                  (get)))
          (equal '((:custom "custom1"
                    :name "alpha1" :in ((:constant 1
                                         :name "beta1" :in ((:value 3 :name "gamma1"))))))
                 (get-from-hierarchical-cache cache-name))))))
  ("Writing requested"
   (with-cache-example
    (:initialize nil)
    (flet ((cache-path (key key-indexes directory-index)
             (let ((cache-directory (ecase directory-index
                                      (1 cache-example-directory)
                                      (2 cache-example-stats-directory)
                                      (3 cache-example-meta-directory))))
               (ecase key
                 (:alpha-file (merge-pathnames (format nil "alpha~a.json" key-indexes) cache-directory))
                 (:beta-file (merge-pathnames (format nil "alpha~a/beta~a.json" (first key-indexes) (second key-indexes))
                                              cache-directory))
                 (:gamma (merge-pathnames (format nil "alpha~a/beta~a/gamma~a.json"
                                                  (first key-indexes) (second key-indexes) (third key-indexes))
                                          cache-directory))))))
      (and (not (file-exists-p (merge-pathnames "root-file.json" cache-example-directory)))
           (not (file-exists-p (merge-pathnames "item-count.json" cache-example-meta-directory)))
           (not (file-exists-p (cache-path :alpha-file 1 1)))
           (not (file-exists-p (cache-path :alpha-file 1 3)))
           (not (file-exists-p (cache-path :beta-file '(1 1) 3)))
           (not (file-exists-p (cache-path :beta-file '(1 2) 3)))
           (not (file-exists-p (cache-path :gamma '(1 1 1) 1)))
           (not (file-exists-p (cache-path :gamma '(1 1 1) 2)))
           (not (file-exists-p (cache-path :gamma '(1 1 2) 1)))
           (not (file-exists-p (cache-path :gamma '(1 1 2) 2)))
           (not (file-exists-p (cache-path :gamma '(1 2 1) 1)))
           (not (file-exists-p (cache-path :gamma '(1 2 1) 2)))
           (not (file-exists-p (cache-path :gamma '(1 2 2) 1)))
           (not (file-exists-p (cache-path :gamma '(1 2 2) 2)))
           (not (file-exists-p (cache-path :alpha-file 2 1)))
           (not (file-exists-p (cache-path :alpha-file 2 3)))
           (not (file-exists-p (cache-path :beta-file '(2 1) 3)))
           (not (file-exists-p (cache-path :beta-file '(2 2) 3)))
           (not (file-exists-p (cache-path :gamma '(2 1 1) 1)))
           (not (file-exists-p (cache-path :gamma '(2 1 1) 2)))
           (not (file-exists-p (cache-path :gamma '(2 1 2) 1)))
           (not (file-exists-p (cache-path :gamma '(2 1 2) 2)))
           (not (file-exists-p (cache-path :gamma '(2 2 1) 1)))
           (not (file-exists-p (cache-path :gamma '(2 2 1) 2)))
           (not (file-exists-p (cache-path :gamma '(2 2 2) 1)))
           (not (file-exists-p (cache-path :gamma '(2 2 2) 2)))
           (progn
             (with-hierarchical-cache
              ("cache-example" :write t)
              (refresh "alpha1" "beta1" "gamma1"))
             (and (file-exists-p (merge-pathnames "root-file.json" cache-example-directory))
                  (file-exists-p (merge-pathnames "item-count.json" cache-example-meta-directory))
                  (file-exists-p (cache-path :alpha-file 1 1))
                  (file-exists-p (cache-path :alpha-file 1 3))
                  (file-exists-p (cache-path :beta-file '(1 1) 3))
                  (not (file-exists-p (cache-path :beta-file '(1 2) 3)))
                  (file-exists-p (cache-path :gamma '(1 1 1) 1))
                  (file-exists-p (cache-path :gamma '(1 1 1) 2))
                  (not (file-exists-p (cache-path :gamma '(1 1 2) 1)))
                  (not (file-exists-p (cache-path :gamma '(1 1 2) 2)))
                  (not (file-exists-p (cache-path :gamma '(1 2 1) 1)))
                  (not (file-exists-p (cache-path :gamma '(1 2 1) 2)))
                  (not (file-exists-p (cache-path :gamma '(1 2 2) 1)))
                  (not (file-exists-p (cache-path :gamma '(1 2 2) 2)))
                  (not (file-exists-p (cache-path :alpha-file 2 1)))
                  (not (file-exists-p (cache-path :alpha-file 2 3)))
                  (not (file-exists-p (cache-path :beta-file '(2 1) 3)))
                  (not (file-exists-p (cache-path :beta-file '(2 2) 3)))
                  (not (file-exists-p (cache-path :gamma '(2 1 1) 1)))
                  (not (file-exists-p (cache-path :gamma '(2 1 1) 2)))
                  (not (file-exists-p (cache-path :gamma '(2 1 2) 1)))
                  (not (file-exists-p (cache-path :gamma '(2 1 2) 2)))
                  (not (file-exists-p (cache-path :gamma '(2 2 1) 1)))
                  (not (file-exists-p (cache-path :gamma '(2 2 1) 2)))
                  (not (file-exists-p (cache-path :gamma '(2 2 2) 1)))
                  (not (file-exists-p (cache-path :gamma '(2 2 2) 2)))))
           (progn
             (with-hierarchical-cache
              ("cache-example" :write t)
              (refresh "alpha1" "beta1"))
             (and (file-exists-p (merge-pathnames "root-file.json" cache-example-directory))
                  (file-exists-p (merge-pathnames "item-count.json" cache-example-meta-directory))
                  (file-exists-p (cache-path :alpha-file 1 1))
                  (file-exists-p (cache-path :alpha-file 1 3))
                  (file-exists-p (cache-path :beta-file '(1 1) 3))
                  (not (file-exists-p (cache-path :beta-file '(1 2) 3)))
                  (file-exists-p (cache-path :gamma '(1 1 1) 1))
                  (file-exists-p (cache-path :gamma '(1 1 1) 2))
                  (file-exists-p (cache-path :gamma '(1 1 2) 1))
                  (file-exists-p (cache-path :gamma '(1 1 2) 2))
                  (not (file-exists-p (cache-path :gamma '(1 2 1) 1)))
                  (not (file-exists-p (cache-path :gamma '(1 2 1) 2)))
                  (not (file-exists-p (cache-path :gamma '(1 2 2) 1)))
                  (not (file-exists-p (cache-path :gamma '(1 2 2) 2)))
                  (not (file-exists-p (cache-path :alpha-file 2 1)))
                  (not (file-exists-p (cache-path :alpha-file 2 3)))
                  (not (file-exists-p (cache-path :beta-file '(2 1) 3)))
                  (not (file-exists-p (cache-path :beta-file '(2 2) 3)))
                  (not (file-exists-p (cache-path :gamma '(2 1 1) 1)))
                  (not (file-exists-p (cache-path :gamma '(2 1 1) 2)))
                  (not (file-exists-p (cache-path :gamma '(2 1 2) 1)))
                  (not (file-exists-p (cache-path :gamma '(2 1 2) 2)))
                  (not (file-exists-p (cache-path :gamma '(2 2 1) 1)))
                  (not (file-exists-p (cache-path :gamma '(2 2 1) 2)))
                  (not (file-exists-p (cache-path :gamma '(2 2 2) 1)))
                  (not (file-exists-p (cache-path :gamma '(2 2 2) 2)))))
           (progn
             (with-hierarchical-cache
              ("cache-example" :write t)
              (refresh "alpha1"))
             (and (file-exists-p (merge-pathnames "root-file.json" cache-example-directory))
                  (file-exists-p (merge-pathnames "item-count.json" cache-example-meta-directory))
                  (file-exists-p (cache-path :alpha-file 1 1))
                  (file-exists-p (cache-path :alpha-file 1 3))
                  (file-exists-p (cache-path :beta-file '(1 1) 3))
                  (file-exists-p (cache-path :beta-file '(1 2) 3))
                  (file-exists-p (cache-path :gamma '(1 1 1) 1))
                  (file-exists-p (cache-path :gamma '(1 1 1) 2))
                  (file-exists-p (cache-path :gamma '(1 1 2) 1))
                  (file-exists-p (cache-path :gamma '(1 1 2) 2))
                  (file-exists-p (cache-path :gamma '(1 2 1) 1))
                  (file-exists-p (cache-path :gamma '(1 2 1) 2))
                  (file-exists-p (cache-path :gamma '(1 2 2) 1))
                  (file-exists-p (cache-path :gamma '(1 2 2) 2))
                  (not (file-exists-p (cache-path :alpha-file 2 1)))
                  (not (file-exists-p (cache-path :alpha-file 2 3)))
                  (not (file-exists-p (cache-path :beta-file '(2 1) 3)))
                  (not (file-exists-p (cache-path :beta-file '(2 2) 3)))
                  (not (file-exists-p (cache-path :gamma '(2 1 1) 1)))
                  (not (file-exists-p (cache-path :gamma '(2 1 1) 2)))
                  (not (file-exists-p (cache-path :gamma '(2 1 2) 1)))
                  (not (file-exists-p (cache-path :gamma '(2 1 2) 2)))
                  (not (file-exists-p (cache-path :gamma '(2 2 1) 1)))
                  (not (file-exists-p (cache-path :gamma '(2 2 1) 2)))
                  (not (file-exists-p (cache-path :gamma '(2 2 2) 1)))
                  (not (file-exists-p (cache-path :gamma '(2 2 2) 2)))))
           (progn
             (with-hierarchical-cache
              ("cache-example" :write t)
              (refresh))
             (and (file-exists-p (merge-pathnames "root-file.json" cache-example-directory))
                  (file-exists-p (merge-pathnames "item-count.json" cache-example-meta-directory))
                  (file-exists-p (cache-path :alpha-file 1 1))
                  (file-exists-p (cache-path :alpha-file 1 3))
                  (file-exists-p (cache-path :beta-file '(1 1) 3))
                  (file-exists-p (cache-path :beta-file '(1 2) 3))
                  (file-exists-p (cache-path :gamma '(1 1 1) 1))
                  (file-exists-p (cache-path :gamma '(1 1 1) 2))
                  (file-exists-p (cache-path :gamma '(1 1 2) 1))
                  (file-exists-p (cache-path :gamma '(1 1 2) 2))
                  (file-exists-p (cache-path :gamma '(1 2 1) 1))
                  (file-exists-p (cache-path :gamma '(1 2 1) 2))
                  (file-exists-p (cache-path :gamma '(1 2 2) 1))
                  (file-exists-p (cache-path :gamma '(1 2 2) 2))
                  (file-exists-p (cache-path :alpha-file 2 1))
                  (file-exists-p (cache-path :alpha-file 2 3))
                  (file-exists-p (cache-path :beta-file '(2 1) 3))
                  (file-exists-p (cache-path :beta-file '(2 2) 3))
                  (file-exists-p (cache-path :gamma '(2 1 1) 1))
                  (file-exists-p (cache-path :gamma '(2 1 1) 2))
                  (file-exists-p (cache-path :gamma '(2 1 2) 1))
                  (file-exists-p (cache-path :gamma '(2 1 2) 2))
                  (file-exists-p (cache-path :gamma '(2 2 1) 1))
                  (file-exists-p (cache-path :gamma '(2 2 1) 2))
                  (file-exists-p (cache-path :gamma '(2 2 2) 1))
                  (file-exists-p (cache-path :gamma '(2 2 2) 2))
                  t)))))))
 ("Delete"
  ("Query only, writing not requested"
   (with-cache-example
    ()
    (with-hierarchical-cache
     ("cache-example")
     (and (equal '((:custom "custom2"
                    :name "alpha2" :in ((:constant 1
                                         :name "beta2" :in ((:name "gamma2" :value 6) (:name "gamma1" :value 5)))
                                        (:constant 1
                                         :name "beta1" :in ((:name "gamma2" :value 5) (:name "gamma1" :value 4)))))
                   (:custom "custom1"
                    :name "alpha1" :in ((:constant 1
                                         :name "beta2" :in ((:name "gamma2" :value 5) (:name "gamma1" :value 4)))
                                        (:constant 1
                                         :name "beta1" :in ((:name "gamma2" :value 4) (:name "gamma1" :value 3))))))
                 (get :in))
          (progn
            (delete "alpha1" "beta1" "gamma1")
            (equal '((:custom "custom2"
                      :name "alpha2" :in ((:constant 1
                                           :name "beta2" :in ((:name "gamma2" :value 6) (:name "gamma1" :value 5)))
                                          (:constant 1
                                           :name "beta1" :in ((:name "gamma2" :value 5) (:name "gamma1" :value 4)))))
                     (:custom "custom1"
                      :name "alpha1" :in ((:constant 1
                                           :name "beta2" :in ((:name "gamma2" :value 5) (:name "gamma1" :value 4)))
                                          (:constant 1
                                           :name "beta1" :in ((:name "gamma2" :value 4))))))
                   (get :in)))
          (progn
            (delete "alpha1" "beta1")
            (equal '((:custom "custom2"
                      :name "alpha2" :in ((:constant 1
                                           :name "beta2" :in ((:name "gamma2" :value 6) (:name "gamma1" :value 5)))
                                          (:constant 1
                                           :name "beta1" :in ((:name "gamma2" :value 5) (:name "gamma1" :value 4)))))
                     (:custom "custom1"
                      :name "alpha1" :in ((:constant 1
                                           :name "beta2" :in ((:name "gamma2" :value 5) (:name "gamma1" :value 4))))))
                   (get :in)))
          (progn
            (delete "alpha1")
            (equal '((:custom "custom2"
                      :name "alpha2" :in ((:constant 1
                                           :name "beta2" :in ((:name "gamma2" :value 6) (:name "gamma1" :value 5)))
                                          (:constant 1
                                           :name "beta1" :in ((:name "gamma2" :value 5) (:name "gamma1" :value 4))))))
                   (get :in)))
          (progn
            (delete)
            (null (get :in)))))))
  ("Writing requested"
   (with-cache-example
    (:write t)
    (flet ((cache-path (key key-indexes directory-index)
             (let ((cache-directory (ecase directory-index
                                      (1 cache-example-directory)
                                      (2 cache-example-stats-directory)
                                      (3 cache-example-meta-directory))))
               (ecase key
                 (:alpha-file (merge-pathnames (format nil "alpha~a.json" key-indexes) cache-directory))
                 (:beta-file (merge-pathnames (format nil "alpha~a/beta~a.json" (first key-indexes) (second key-indexes))
                                              cache-directory))
                 (:gamma (merge-pathnames (format nil "alpha~a/beta~a/gamma~a.json"
                                                  (first key-indexes) (second key-indexes) (third key-indexes))
                                          cache-directory))))))
      (and (file-exists-p (merge-pathnames "root-file.json" cache-example-directory))
           (file-exists-p (merge-pathnames "item-count.json" cache-example-meta-directory))
           (file-exists-p (cache-path :alpha-file 1 1))
           (file-exists-p (cache-path :alpha-file 1 3))
           (file-exists-p (cache-path :beta-file '(1 1) 3))
           (file-exists-p (cache-path :beta-file '(1 2) 3))
           (file-exists-p (cache-path :gamma '(1 1 1) 1))
           (file-exists-p (cache-path :gamma '(1 1 1) 2))
           (file-exists-p (cache-path :gamma '(1 1 2) 1))
           (file-exists-p (cache-path :gamma '(1 1 2) 2))
           (file-exists-p (cache-path :gamma '(1 2 1) 1))
           (file-exists-p (cache-path :gamma '(1 2 1) 2))
           (file-exists-p (cache-path :gamma '(1 2 2) 1))
           (file-exists-p (cache-path :gamma '(1 2 2) 2))
           (file-exists-p (cache-path :alpha-file 2 1))
           (file-exists-p (cache-path :alpha-file 2 3))
           (file-exists-p (cache-path :beta-file '(2 1) 3))
           (file-exists-p (cache-path :beta-file '(2 2) 3))
           (file-exists-p (cache-path :gamma '(2 1 1) 1))
           (file-exists-p (cache-path :gamma '(2 1 1) 2))
           (file-exists-p (cache-path :gamma '(2 1 2) 1))
           (file-exists-p (cache-path :gamma '(2 1 2) 2))
           (file-exists-p (cache-path :gamma '(2 2 1) 1))
           (file-exists-p (cache-path :gamma '(2 2 1) 2))
           (file-exists-p (cache-path :gamma '(2 2 2) 1))
           (file-exists-p (cache-path :gamma '(2 2 2) 2))
           (progn
             (with-hierarchical-cache
              ("cache-example" :write t)
              (delete "alpha1" "beta1" "gamma1"))
             (and (file-exists-p (merge-pathnames "root-file.json" cache-example-directory))
                  (file-exists-p (merge-pathnames "item-count.json" cache-example-meta-directory))
                  (file-exists-p (cache-path :alpha-file 1 1))
                  (file-exists-p (cache-path :alpha-file 1 3))
                  (file-exists-p (cache-path :beta-file '(1 1) 3))
                  (file-exists-p (cache-path :beta-file '(1 2) 3))
                  (not (file-exists-p (cache-path :gamma '(1 1 1) 1)))
                  (not (file-exists-p (cache-path :gamma '(1 1 1) 2)))
                  (file-exists-p (cache-path :gamma '(1 1 2) 1))
                  (file-exists-p (cache-path :gamma '(1 1 2) 2))
                  (file-exists-p (cache-path :gamma '(1 2 1) 1))
                  (file-exists-p (cache-path :gamma '(1 2 1) 2))
                  (file-exists-p (cache-path :gamma '(1 2 2) 1))
                  (file-exists-p (cache-path :gamma '(1 2 2) 2))
                  (file-exists-p (cache-path :alpha-file 2 1))
                  (file-exists-p (cache-path :alpha-file 2 3))
                  (file-exists-p (cache-path :beta-file '(2 1) 3))
                  (file-exists-p (cache-path :beta-file '(2 2) 3))
                  (file-exists-p (cache-path :gamma '(2 1 1) 1))
                  (file-exists-p (cache-path :gamma '(2 1 1) 2))
                  (file-exists-p (cache-path :gamma '(2 1 2) 1))
                  (file-exists-p (cache-path :gamma '(2 1 2) 2))
                  (file-exists-p (cache-path :gamma '(2 2 1) 1))
                  (file-exists-p (cache-path :gamma '(2 2 1) 2))
                  (file-exists-p (cache-path :gamma '(2 2 2) 1))
                  (file-exists-p (cache-path :gamma '(2 2 2) 2))))
           (progn
             (with-hierarchical-cache
              ("cache-example" :write t)
              (delete "alpha1" "beta1"))
             (and (file-exists-p (merge-pathnames "root-file.json" cache-example-directory))
                  (file-exists-p (merge-pathnames "item-count.json" cache-example-meta-directory))
                  (file-exists-p (cache-path :alpha-file 1 1))
                  (file-exists-p (cache-path :alpha-file 1 3))
                  (not (file-exists-p (cache-path :beta-file '(1 1) 3)))
                  (file-exists-p (cache-path :beta-file '(1 2) 3))
                  (not (file-exists-p (cache-path :gamma '(1 1 1) 1)))
                  (not (file-exists-p (cache-path :gamma '(1 1 1) 2)))
                  (not (file-exists-p (cache-path :gamma '(1 1 2) 1)))
                  (not (file-exists-p (cache-path :gamma '(1 1 2) 2)))
                  (file-exists-p (cache-path :gamma '(1 2 1) 1))
                  (file-exists-p (cache-path :gamma '(1 2 1) 2))
                  (file-exists-p (cache-path :gamma '(1 2 2) 1))
                  (file-exists-p (cache-path :gamma '(1 2 2) 2))
                  (file-exists-p (cache-path :alpha-file 2 1))
                  (file-exists-p (cache-path :alpha-file 2 3))
                  (file-exists-p (cache-path :beta-file '(2 1) 3))
                  (file-exists-p (cache-path :beta-file '(2 2) 3))
                  (file-exists-p (cache-path :gamma '(2 1 1) 1))
                  (file-exists-p (cache-path :gamma '(2 1 1) 2))
                  (file-exists-p (cache-path :gamma '(2 1 2) 1))
                  (file-exists-p (cache-path :gamma '(2 1 2) 2))
                  (file-exists-p (cache-path :gamma '(2 2 1) 1))
                  (file-exists-p (cache-path :gamma '(2 2 1) 2))
                  (file-exists-p (cache-path :gamma '(2 2 2) 1))
                  (file-exists-p (cache-path :gamma '(2 2 2) 2))))
           (progn
             (with-hierarchical-cache
              ("cache-example" :write t)
              (delete "alpha1"))
             (and (file-exists-p (merge-pathnames "root-file.json" cache-example-directory))
                  (file-exists-p (merge-pathnames "item-count.json" cache-example-meta-directory))
                  (not (file-exists-p (cache-path :alpha-file 1 1)))
                  (not (file-exists-p (cache-path :alpha-file 1 3)))
                  (not (file-exists-p (cache-path :beta-file '(1 1) 3)))
                  (not (file-exists-p (cache-path :beta-file '(1 2) 3)))
                  (not (file-exists-p (cache-path :gamma '(1 1 1) 1)))
                  (not (file-exists-p (cache-path :gamma '(1 1 1) 2)))
                  (not (file-exists-p (cache-path :gamma '(1 1 2) 1)))
                  (not (file-exists-p (cache-path :gamma '(1 1 2) 2)))
                  (not (file-exists-p (cache-path :gamma '(1 2 1) 1)))
                  (not (file-exists-p (cache-path :gamma '(1 2 1) 2)))
                  (not (file-exists-p (cache-path :gamma '(1 2 2) 1)))
                  (not (file-exists-p (cache-path :gamma '(1 2 2) 2)))
                  (file-exists-p (cache-path :alpha-file 2 1))
                  (file-exists-p (cache-path :alpha-file 2 3))
                  (file-exists-p (cache-path :beta-file '(2 1) 3))
                  (file-exists-p (cache-path :beta-file '(2 2) 3))
                  (file-exists-p (cache-path :gamma '(2 1 1) 1))
                  (file-exists-p (cache-path :gamma '(2 1 1) 2))
                  (file-exists-p (cache-path :gamma '(2 1 2) 1))
                  (file-exists-p (cache-path :gamma '(2 1 2) 2))
                  (file-exists-p (cache-path :gamma '(2 2 1) 1))
                  (file-exists-p (cache-path :gamma '(2 2 1) 2))
                  (file-exists-p (cache-path :gamma '(2 2 2) 1))
                  (file-exists-p (cache-path :gamma '(2 2 2) 2))))
           (progn
             (with-hierarchical-cache
              ("cache-example" :write t)
              (delete))
             (and (file-exists-p (merge-pathnames "root-file.json" cache-example-directory))
                  (file-exists-p (merge-pathnames "item-count.json" cache-example-meta-directory))
                  (not (file-exists-p (cache-path :alpha-file 1 1)))
                  (not (file-exists-p (cache-path :alpha-file 1 3)))
                  (not (file-exists-p (cache-path :beta-file '(1 1) 3)))
                  (not (file-exists-p (cache-path :beta-file '(1 2) 3)))
                  (not (file-exists-p (cache-path :gamma '(1 1 1) 1)))
                  (not (file-exists-p (cache-path :gamma '(1 1 1) 2)))
                  (not (file-exists-p (cache-path :gamma '(1 1 2) 1)))
                  (not (file-exists-p (cache-path :gamma '(1 1 2) 2)))
                  (not (file-exists-p (cache-path :gamma '(1 2 1) 1)))
                  (not (file-exists-p (cache-path :gamma '(1 2 1) 2)))
                  (not (file-exists-p (cache-path :gamma '(1 2 2) 1)))
                  (not (file-exists-p (cache-path :gamma '(1 2 2) 2)))
                  (not (file-exists-p (cache-path :alpha-file 2 1)))
                  (not (file-exists-p (cache-path :alpha-file 2 3)))
                  (not (file-exists-p (cache-path :beta-file '(2 1) 3)))
                  (not (file-exists-p (cache-path :beta-file '(2 2) 3)))
                  (not (file-exists-p (cache-path :gamma '(2 1 1) 1)))
                  (not (file-exists-p (cache-path :gamma '(2 1 1) 2)))
                  (not (file-exists-p (cache-path :gamma '(2 1 2) 1)))
                  (not (file-exists-p (cache-path :gamma '(2 1 2) 2)))
                  (not (file-exists-p (cache-path :gamma '(2 2 1) 1)))
                  (not (file-exists-p (cache-path :gamma '(2 2 1) 2)))
                  (not (file-exists-p (cache-path :gamma '(2 2 2) 1)))
                  (not (file-exists-p (cache-path :gamma '(2 2 2) 2)))
                  t))))))))