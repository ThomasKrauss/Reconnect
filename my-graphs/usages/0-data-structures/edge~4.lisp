("Examples"
 (null (edge? 3))
 (null (edge? "this is an edge?"))
 (null (edge? (list (make-node "foo"))))
 (null (edge? (list (make-node "foo") (make-node "bar"))))
 (null (edge? (list (make-node "foo") (make-node "bar") "test")))
 (let ((edge
        (list (make-node "foo") (make-node "bar") "test" "Misc.")))
   (equal (edge? edge) edge)))

