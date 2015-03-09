(in-package :my-graphs)

(defun make-subgraph (nodes graph &key name)
  "Return a graph whose nodes -but not links!- are restricted to the given nodes.
Therefore, the links can refer to other nodes.
Useful for clusters, in which case the graph should be named."
  (make-graph (copy-tree nodes)
              (let (result)
                (dolist (node nodes)
                  (dolist (link (graph-link node graph))
                    (push (copy-tree link) result)))
                (nreverse result))
              :name
              (or name "")
              :directed
              (graph-directed? graph)))

(defun remove-links-by-nodes (links nodes &key (lenient t))
  "=> links
Remove all the links whose head node isn't in the given nodes.
When lenient is nil, which is not the default, the successor nodes of the links that haven't been removed will be trimmed of all the nodes which are not in the given nodes, so that the resulting links are fit for a strict graph."
  (loop for link in links
        when (known-node? (link-head-node-name link) nodes)
        collect (if lenient
                  (copy-tree link)
                  (make-link% (link-head-node-name link)
                              (remove-if-not (lambda (node-name)
                                               (known-node? node-name nodes))
                                             (link-successor-nodes-names link))
                              :class (link-class link)))))

(defun lenient-graph<-directed-graph (directed-graph &key (nodes nil nodes-supplied?)
                                                     (links nil links-supplied?) (edges nil edges-supplied?)
                                                     (name nil name-supplied?))
  "=> lenient-graph
Make a lenient graph out of the given directed graph, copying its properties unless they have been overridden by the key parameters.
Nodes will thus be overridden, so will links. Edges will be merged in though. Then, by virtue of being lenient, any link with a head node's name not in the graph's nodes. Successor nodes are not constrained and left untouched.
Note that if the given graph is undirected, nil is returned since a graph can be lenient and undirected. Split in clusters if you want several undirected graphs connected together."
  (when (graph-directed? directed-graph)
    (make-graph% (if nodes (copy-tree nodes) (graph-nodes directed-graph))
                 :links (let-a (merge-edges-in-links edges (or links (graph-links directed-graph)))
                          (if nodes
                            (remove-links-by-nodes it nodes)
                            it))
                 :name (if name-supplied? name (graph-name directed-graph))
                 :lenient t)))

(defun strict-graph<-graph (graph &key (nodes nil nodes-supplied?)
                                  (links nil links-supplied?) (edges nil edges-supplied?)
                                  (name nil name-supplied?))
  "=> strict-graph
Make a strict graph out of the given graph, copying its properties unless they have been overridden by the key parameters.
Nodes will thus be overridden, so will links. Edges will be merged in though. Then, because of strictness, any link with a head node's name not in the graph's nodes will be ignored from the resulting graph, and for links still being kept, every of their successor node not in the graph's nodes will be removed too."
  (let ((links (let-a (merge-edges-in-links edges (or links (graph-links graph)) :directed t)
                 (if nodes
                   (remove-links-by-nodes it nodes)
                   it))))
    (make-graph% (if nodes (copy-tree nodes) (graph-nodes graph))
                 :links (if (graph-directed? graph)
                          links
                          (complete-with-opposite-edges links))
                 :name (if name-supplied? name (graph-name graph))
                 :directed (graph-directed? graph)
                 :lenient nil)))

(defun undirected-graph<-graph (graph &key (nodes nil nodes-supplied?)
                                      (links nil links-supplied?) (edges nil edges-supplied?)
                                      (name nil name-supplied?))
  "=> undirected-graph
Make an undirected graph out the given graph, copying its properties unless they have been overridden by the key parameters.
Nodes will thus be overridden, so will links. Edges will be merged in though. Then, to make the graph undirected and because it also implies strictness, links will be enriched with opposite edges while also trimmed of any link with a head node not in the graph's nodes and links that are still being kept will have their successor nodes trimmed of unknown nodes."
  (make-graph% (if nodes (copy-tree nodes) (graph-nodes graph))
               :links (complete-with-opposite-edges
                       (let-a (merge-edges-in-links edges (or links (graph-links graph)) :directed t)
                         (if nodes
                           (remove-links-by-nodes it nodes)
                           it)))
               :name (if name-supplied? name (graph-name graph))
               :directed nil
               :lenient nil))

(defun make-direct-ancestors-graph (graph)
  "=> graph
  Reverse the direction of the edges. The nodes, as well as the links, of the resulting graph have no class attached."
  (let ((direct-ancestors-graph (make-graph nil nil)))
    (dolist (link (graph-links graph))
      (let ((ancestor-name (link-start-node-name link)))
        (dolist (successor (link-successors link))
          (graph-add-link (make-link (link-successor-name successor)
                                     (list ancestor-name))
                          direct-ancestors-graph))))
    direct-ancestors-graph))

(defun make-direct-predecessors-graph (directed-graph &key (name nil name-supplied?))
  "=> directed-graph
Reverse the direction of the edges of the graph. Predecessors are now successors and successors have become predecessors. Every other characteristic ---strictness, name and classes--- are forwarded.
If the graph is strict, no data will be lost.
But if the it is lenient, there will be a loss of information on nodes. Indeed, the resulting nodes will only be made of the head nodes of the reversed edges. If these nodes were known of the initial graph where they are tail nodes, their classes and additionnal information will be forwarded. For all the other nodes, whose which were pure head nodes, these data will not be included in the resulting graph."
  (when (graph-directed? directed-graph)
    (let ((links (mapcar #'link<-edge (compute-opposite-edges (graph-links directed-graph)))))
      (make-graph% (merge-link-nodes-in-nodes links nil :lenient (graph-lenient? directed-graph))
                   :links links
                   :directed t
                   :lenient (graph-lenient? directed-graph)
                   :name (if name-supplied? name (graph-name directed-graph))))))

(defun graph-generate-undirected-graph (graph)
  (if (not (graph-directed? graph))
      graph
    (let ((undirected-graph
           (make-graph (graph-nodes graph) nil :directed nil)))
      (loop for link in (graph-links graph)
            do (graph-add-link link undirected-graph))
      undirected-graph)))

