("Examples"
 (string= "foo" (node-name (make-node "foo")))
 (let ((node (make-node "foo")))
   (setf (node-name node) "bar")
   (string= "bar" (node-name node))))

