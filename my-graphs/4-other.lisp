(in-package :my-graphs)

(defun graph-topological-sort (graph &key lenient-search)
  (let (result)
    (with-machineries ((dfs (make-graph-dfs-machinery graph :lenient-search lenient-search)))
      (loop for node in (graph-nodes graph)
            when (not (dfs :discovered? node))
            do (dfs :search node
                    :process-vertex-late (lambda (u)
                                           (push u result))
                    :process-edge
                    (lambda (u v)
                      (when (eq 'dfs-back (dfs :classify-edge u v))
                        (warn "Directed cycle found! The graph is not acyclic because of edge ~a -> ~a~%" u v))))))
    result))

(defun sort-nodes-topologically (graph &key lenient-search)
  "=> nodes"
  (let (nodes)
    (with-machineries ((dfs (make-graph-dfs-machinery% graph :lenient-search lenient-search)))
      (loop for node in (graph-nodes graph)
            when (not (dfs :discovered? node))
            do (dfs :search node
                    :process-vertex-late (lambda (node)
                                           (push node nodes))
                    :process-edge
                    (lambda (node successor-node)
                      (when (eq 'dfs-back (dfs :classify-edge node successor-node))
                        (warn "Directed cycle found! The graph is not acyclic because of edge ~a -> ~a~%"
                              node successor-node))))))
    nodes))

(defun graph-connected-components (graph)
  "Compute the connected components of the given nodes and edges.
There are no subleties when the graph is undirected. But when it is, this function computes the weakly connected components of the graph.
The difference with the strongly connected components is that we just consider the presence of an edge, not its actual direction. The directed graph is just considered as an undirected one and edges are crossed regardless if the move is legal or not."
  (let* (components
         (undirected-graph (graph-generate-undirected-graph graph)))
    (with-machineries ((bfs (make-graph-bfs-machinery undirected-graph)))
      (loop for node in (graph-nodes undirected-graph)
            when (null (bfs :discovered? node))
            do (progn
                 (push () components)
                 (bfs :search node :process-vertex-early (lambda (x) (push x (first components))))))
      (loop for component in components
            collect (make-subgraph component graph)))))

(defun split-in-connected-components (graph)
  "=> graphs
Give the list of the connected components of the given graph.
If it is directed, the weakly connected components are returned, that is two nodes are considered connected as soon as there is an edge between them, its direction being inconsequent."
  (let (nodes-lst)
    (with-machineries ((bfs (make-graph-bfs-machinery (undirected-graph<-graph graph))))
      (loop for node in (graph-nodes graph)
            when (null (bfs :discovered? node))
            do (progn
                 (push () nodes-lst)
                 (bfs :search node :process-vertex-early (lambda (x) (push x (first nodes-lst))))))
      (loop for nodes in nodes-lst
            collect (if (graph-directed? graph)
                      (lenient-graph<-directed-graph graph :nodes nodes :name "")
                      (strict-graph<-graph graph :nodes nodes))))))

(defun split-in-clusters (graph reference-nodes split-function)
  ; graph -> graphs (cc) -> graph-cluster-set
  (let (sorted-nodes)
    (loop for component in (graph-connected-components graph)
          if (= (length (graph-nodes component)) 1)
          collect (first (graph-nodes component)) into singles
          else
          collect (multiple-value-bind
                      (clusters nodes)
                      (funcall split-function component)
                    (setf sorted-nodes (append sorted-nodes nodes))
                    (list :clusters clusters))
          into components
          finally (return (list :name (graph-name graph)
                                :nodes (append (copy-tree singles)
                                               (loop for node in (reverse sorted-nodes)
                                                     collect (find (node-name node) reference-nodes
                                                                   :test #'equal :key #'node-name)))
                                :components components
                                :singles (list :nodes singles))))))

(defun graph-cluster-set<-graph (graph split-function)
  "=> graph-cluster-set
The split function will be called on each connected component of the given graph and must returned a list of graphs.
From this list, a graph-cluster will be built. Thus, for each connected component, a graph-cluster will be made and all of them will be returned as a graph-cluster-set."
  (make-graph-cluster-set
   (loop for graph in (split-in-connected-components graph)
         collect (make-graph-cluster (funcall split-function graph)))))

(defun keep-only-nodes (graph nodes)
  "Derive a graph from the given graph whose links only point to nodes from the given list."
  ; strict-graph<-graph
  (let ((nodes (mapcar #'extended-node-name nodes)))
    (make-graph (graph-nodes graph)
                (let (result)
                  (dolist (link (graph-links graph))
                    (awhen (loop for successor in (link-successors link)
                                 when (member (link-successor-name successor) nodes :test #'equal)
                                 collect successor)
                      (push (make-link (link-start-node-name link) it (link-class link)) result)))
                  (nreverse result))
                :name (or (graph-name graph) "")
                :directed (graph-directed? graph))))

(defun derive-strict-clustered-graph (clustered-graph)
  "Deriving a strict module graph from the given general module graph consists of trimming all the links of successors which are not actions from the given module (and therefore not of the same system) such that only the internal structure of the module is kept while still having all the usage information."
  ; select the cluster which share the same name that the clustered-graph in each graph-cluster
  ; to strict version with the nodes of clustered-graph
  ; graph -> graphs (cc), collect all graphs that way then -> graph-set
  (let (single-nodes components)
    (dolist (component (clustered-graph-components clustered-graph))
      (dolist (component (graph-connected-components
                          (keep-only-nodes
                           (clustered-graph-component-cluster (graph-name clustered-graph) component)
                           (graph-nodes clustered-graph))))
        (if (= 1 (length (graph-nodes component)))
          (push (first (graph-nodes component)) single-nodes)
          (push component components))))
    (list :name (graph-name clustered-graph)
          :singles (let-a (append (clustered-graph-singles-nodes clustered-graph)
                                  single-nodes)
                     (list :nodes it))
          :components (mapcar (lambda (component)
                                (list :clusters (list component)))
                              components)
          :nodes (graph-nodes clustered-graph))))