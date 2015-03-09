("Example"
 (graph= (make-graph (list (make-node "foo")
                           (make-node "bar")
                           (make-node "baz"))
                     (list (make-edge (make-node "foo")
                                      (make-node "bar"))
                           (make-edge (make-node "bar")
                                      (make-node "baz"))
                           (make-edge (make-node "baz")
                                      (make-node "foo")))
                     :directed
                     t)
         (make-graph (list (make-node "bar")
                           (make-node "foo")
                           (make-node "baz"))
                     (list (make-edge (make-node "bar")
                                      (make-node "baz"))
                           (make-edge (make-node "foo")
                                      (make-node "bar"))
                           (make-edge (make-node "baz")
                                      (make-node "foo")))
                     :directed
                     t)))

