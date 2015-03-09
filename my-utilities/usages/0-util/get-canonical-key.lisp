("get-canonical-key gives the canonical key synonymous of the given key. It must be passed as a second argument a list of keyword lists, the first of each being the canonical keys for the synonymous next ones."
 ("Examples"
  (eql :k1 (get-canonical-key :k1 '((:k1 :k2 :k3) (:l1 :l2) (:test :to :see))))
  (eql :k1 (get-canonical-key :k2 '((:k1 :k2 :k3) (:l1 :l2) (:test :to :see))))
  (eql :k1 (get-canonical-key :k3 '((:k1 :k2 :k3) (:l1 :l2) (:test :to :see))))
  (null (get-canonical-key :k4 '((:k1 :k2 :k3) (:l1 :l2) (:test :to :see))))
  (eql :l1 (get-canonical-key :l2 '((:k1 :k2 :k3) (:l1 :l2) (:test :to :see))))
  (eql :test (get-canonical-key :see '((:k1 :k2 :k3) (:l1 :l2) (:test :to :see))))))