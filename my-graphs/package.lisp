(in-package :cl)

(defpackage :my-graphs (:use :cl :cl-fad :cl-ppcre :my-utilities)
  (:export
   :dot-plain-string
   :make-node
   :node-name
   :node=
   :extended-node-name
   :node-class
   :node-info
   :make-link
   :link-start-node-name
   :link-class
   :link-successors
   :link-add-successor
   :link-successor-name
   :link-successor-label
   :make-graph
   :graph-nodes
   :graph-add-node
   :graph-node
   :graph-node-position
   :graph-links
   :graph-link
   :graph-add-link
   :graph-node-successor-names
   :graph-directed?
   :graph-name
   :clustered-graph-components
   :clustered-graph-component-clusters
   :clustered-graph-component-cluster
   :clustered-graph-component-stats
   :clustered-graph-clusters
   :clustered-graph-singles
   :clustered-graph-singles-nodes
   :clustered-graph-singles-stats
   :make-subgraph
   :make-direct-ancestors-graph
   :derive-strict-clustered-graph
   :drawing<-subgraphs
   :graph-generate-undirected-graph
   :node-position-id
   :get-simple-graph-positions
   :get-clustered-graph-positions
   :get-strict-clustered-graph-positions
   :graph-connected-components
   :split-in-clusters
   :graph-topological-sort))