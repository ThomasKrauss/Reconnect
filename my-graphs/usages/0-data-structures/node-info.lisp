("Example"
 (string= (node-info (make-node "foo" :info "Misc.")) "Misc.")
 (let ((node (make-node "foo" :info "Misc.")))
   (setf (node-info node) "Updated.")
   (string= "Updated." (node-info node))))

