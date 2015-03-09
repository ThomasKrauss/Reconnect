("Example"
 (string= (node-class (make-node "foo" :class "test")) "test")
 (let ((node (make-node "foo" :class "test")))
   (setf (node-class node) "bar")
   (string= "bar" (node-class node))))

