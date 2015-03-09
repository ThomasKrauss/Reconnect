("Example"
 (edge= (make-edge (make-node "foo") (make-node "bar"))
        (make-edge (make-node "foo") (make-node "bar")))
 (null (edge= (make-edge (make-node "foo") (make-node "bar"))
              (make-edge (make-node "foo-foo") (make-node "bar"))))
 (null (edge= (make-edge (make-node "foo") (make-node "bar"))
              (make-edge (make-node "foo") (make-node "bar-bar")))))

