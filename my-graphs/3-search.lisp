(in-package :my-graphs)

(defun make-graph-bfs-machinery (graph)
  "Return a machinery to perform a breadth-first search on the given graph."
  (let ((machinery
         (let (processed discovered parent-idx)
           (dlambda
             (:reset ()
              (let ((size (length (graph-nodes graph))))
                (setf processed (make-array size :initial-element nil)
                      discovered (make-array size :initial-element nil)
                      parent-idx (make-array size :initial-element -1))))
             (:discovered? (node)
              (elt discovered (graph-node-position node graph)))
             (:search (node &key process-vertex-early process-vertex-late process-edge)
              (let ((q (make-queue))
                    (p (graph-node-position node graph)))
                (enqueue node q)
                (setf (elt discovered p) t)
                (loop while (not (empty-queue-p q))
                      do (let* ((u (dequeue q))
                                (u-pos (graph-node-position u graph)))
                           (awhen process-vertex-early
                             (funcall it u))
                           (dolist (name (graph-node-successor-names u graph))
                             (multiple-value-bind
                                 (v v-pos)
                                 (graph-node name graph)
                               (awhen process-edge
                                 (funcall it u v))
                               (when (null (elt discovered v-pos))
                                 (setf (elt discovered v-pos) t
                                       (elt parent-idx v-pos) u-pos)
                                 (enqueue v q)))
                             (setf (elt processed u-pos) t)
                             (awhen process-vertex-late
                               (funcall it u)))))))))))
    (funcall machinery :reset)
    machinery))

(defun make-graph-bfs-machinery% (graph)
  "Return a machinery to perform a breadth-first search on the given graph."
  (let ((machinery
         (let (processed discovered parent-idx)
           (dlambda
             (:reset ()
              (let ((size (length (graph-nodes graph))))
                (setf processed (make-array size :initial-element nil)
                      discovered (make-array size :initial-element nil)
                      parent-idx (make-array size :initial-element -1))))
             (:discovered? (node)
              (elt discovered (position-node node graph)))
             (:search (node &key process-vertex-early process-vertex-late process-edge)
              (let ((queue (make-queue))
                    (index (position-node node graph)))
                (enqueue node queue)
                (setf (elt discovered index) t)
                (loop while (not (empty-queue-p queue))
                      do (let* ((node (dequeue queue))
                                (index (position-node node graph)))
                           (awhen process-vertex-early
                             (funcall it node))
                           (dolist (node-name (find-node-successors node graph))
                             (multiple-value-bind
                                 (successor-node successor-node-index)
                                 (locate-node node-name graph)
                               (awhen process-edge
                                 (funcall it node successor-node))
                               (when (null (elt discovered successor-node-index))
                                 (setf (elt discovered successor-node-index) t
                                       (elt parent-idx successor-node-index) index)
                                 (enqueue successor-node queue)))
                             (setf (elt processed index) t)
                             (awhen process-vertex-late
                               (funcall it node)))))))))))
    (funcall machinery :reset)
    machinery))

(define-condition unreferenced-node (error)
  ((name :initarg :name :reader name))
  (:report (lambda (condition stream)
             (format stream "The node of name ~a is not in the list of nodes but it is referenced in edges"
                     (name condition)))))

(defun make-graph-dfs-machinery (graph &key lenient-search)
  "Make a machinery for depth-first walking the given graph.
If lenient-search is true, the search ignore the node it has arrived at if it is not in the list of nodes.
For more control, use the restart ignore-node on error unreferenced-node."
  (let ((machinery
         (let (processed discovered parent-idx finished time entry-time exit-time directed-graph)
           (labels ((dfs-search (node &key process-vertex-early process-vertex-late process-edge)
                      (unless finished
                        (let ((p (graph-node-position node graph)))
                          (incf time)
                          (setf (elt discovered p) t
                                (elt entry-time p) time)
                          (awhen process-vertex-early
                            (funcall it node))
                          (dolist (name (graph-node-successor-names node graph))
                            (restart-case
                                (multiple-value-bind
                                    (successor-node successor-position)
                                    (graph-node name graph)
                                  (if successor-position
                                    (progn
                                      (setf (elt parent-idx successor-position) p)
                                      (cond
                                       ((not (elt discovered successor-position))
                                        (awhen process-edge
                                          (funcall it node successor-node))
                                        (dfs-search successor-node
                                                    :process-vertex-early process-vertex-early
                                                    :process-edge process-edge
                                                    :process-vertex-late process-vertex-late))
                                       ((and process-edge
                                             (or (not (elt processed successor-position))
                                                 directed-graph))
                                        (funcall process-edge node successor-node))))
                                    (error 'unreferenced-node :name name))
                                  (when finished
                                    (return)))
                              (ignore-node () nil)))
                          (awhen process-vertex-late
                            (funcall it node))
                          (incf time)
                          (setf (elt exit-time p) time
                                (elt processed p) t)))))
           (dlambda
             (:reset ()
              (let ((size (length (graph-nodes graph))))
                (setf processed (make-array size :initial-element nil)
                      discovered (make-array size :initial-element nil)
                      parent-idx (make-array size :initial-element -1)
                      finished nil
                      time 0
                      entry-time (make-array size :initial-element 0)
                      exit-time (make-array size :initial-element 0)
                      directed-graph (graph-directed? graph))))
             (:finished ()
              (setf finished t))
             (:parent-idx (&optional node)
              (if node
                (elt parent-idx (graph-node-position node graph))
                parent-idx))
             (:discovered? (node)
              (elt discovered (graph-node-position node graph)))
             (:classify-edge (u v)
              (let* ((u-pos (graph-node-position u graph))
                     (v-pos (graph-node-position v graph))
                     (processed? (elt processed v-pos)))
                (cond
                 ((= (elt parent-idx v-pos)
                     u-pos)
                  'dfs-tree)
                 ((and (elt discovered v-pos)
                       (not processed?))
                  'dfs-back)
                 ((and processed?
                       (< (elt entry-time u-pos)
                          (elt entry-time v-pos)))
                  'dfs-forward)
                 ((and processed?
                       (< (elt entry-time v-pos)
                          (elt entry-time u-pos)))
                  'dfs-cross)
                 (t (warn "Unclassified edge: ~a, ~a" u v)))))
             (:search (node &key process-vertex-early process-vertex-late process-edge)
              (if lenient-search
                (handler-bind ((unreferenced-node (lambda (c)
                                                    (declare (ignore c))
                                                    (invoke-restart 'ignore-node))))
                  (dfs-search node
                              :process-vertex-early process-vertex-early
                              :process-edge process-edge
                              :process-vertex-late process-vertex-late))
                (dfs-search node
                          :process-vertex-early process-vertex-early
                          :process-edge process-edge
                          :process-vertex-late process-vertex-late))))))))
    (funcall machinery :reset)
    machinery))

(defun make-graph-dfs-machinery% (graph &key lenient-search)
  "Make a machinery for depth-first walking the given graph.
If lenient-search is true, the search ignore the node it has arrived at if it is unknown.
For more control, use the restart ignore-node on error unreferenced-node."
  (let ((machinery
         (let (processed discovered parent-idx finished time entry-time exit-time directed-graph)
           (labels ((dfs-search (node &key process-vertex-early process-vertex-late process-edge)
                      (unless finished
                        (let ((index (position-node node graph)))
                          (incf time)
                          (setf (elt discovered index) t
                                (elt entry-time index) time)
                          (awhen process-vertex-early
                            (funcall it node))
                          (dolist (node-name (find-node-successors node graph))
                            (restart-case
                                (multiple-value-bind
                                    (successor-node successor-index)
                                    (locate-node node-name graph)
                                  (if successor-index
                                    (progn
                                      (setf (elt parent-idx successor-index) index)
                                      (cond
                                       ((not (elt discovered successor-index))
                                        (awhen process-edge
                                          (funcall it node successor-node))
                                        (dfs-search successor-node
                                                    :process-vertex-early process-vertex-early
                                                    :process-edge process-edge
                                                    :process-vertex-late process-vertex-late))
                                       ((and process-edge
                                             (or (not (elt processed successor-index))
                                                 directed-graph))
                                        (funcall process-edge node successor-node))))
                                    (error 'unreferenced-node :name node-name))
                                  (when finished
                                    (return)))
                              (ignore-node () nil)))
                          (awhen process-vertex-late
                            (funcall it node))
                          (incf time)
                          (setf (elt exit-time index) time
                                (elt processed index) t)))))
           (dlambda
             (:reset ()
              (let ((size (length (graph-nodes graph))))
                (setf processed (make-array size :initial-element nil)
                      discovered (make-array size :initial-element nil)
                      parent-idx (make-array size :initial-element -1)
                      finished nil
                      time 0
                      entry-time (make-array size :initial-element 0)
                      exit-time (make-array size :initial-element 0)
                      directed-graph (graph-directed? graph))))
             (:finished ()
              (setf finished t))
             (:parent-idx (&optional node)
              (if node
                (elt parent-idx (position-node node graph))
                parent-idx))
             (:discovered? (node)
              (elt discovered (position-node node graph)))
             (:classify-edge (u v)
              (let* ((u-index (position-node u graph))
                     (v-index (position-node v graph))
                     (processed? (elt processed v-index)))
                (cond
                 ((= (elt parent-idx v-index)
                     u-index)
                  'dfs-tree)
                 ((and (elt discovered v-index)
                       (not processed?))
                  'dfs-back)
                 ((and processed?
                       (< (elt entry-time u-index)
                          (elt entry-time v-index)))
                  'dfs-forward)
                 ((and processed?
                       (< (elt entry-time v-index)
                          (elt entry-time u-index)))
                  'dfs-cross)
                 (t (warn "Unclassified edge: ~a, ~a" u v)))))
             (:search (node &key process-vertex-early process-vertex-late process-edge)
              (if lenient-search
                (handler-bind ((unreferenced-node (lambda (c)
                                                    (declare (ignore c))
                                                    (invoke-restart 'ignore-node))))
                  (dfs-search node
                              :process-vertex-early process-vertex-early
                              :process-edge process-edge
                              :process-vertex-late process-vertex-late))
                (dfs-search node
                          :process-vertex-early process-vertex-early
                          :process-edge process-edge
                          :process-vertex-late process-vertex-late))))))))
    (funcall machinery :reset)
    machinery))