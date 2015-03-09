(in-package :my-graphs)

(defun make-raw-graph (nodes+ &key links edges (directed t) (name "") lenient)
  "=> raw-graph
A raw-graph follows the very same rules of a regular graph, up to one exception: it only holds the name of the nodes, not the node themselves."
  (let* ((links (let-a (merge-edges-in-links edges links)
                  (if directed
                    it
                    (complete-with-opposite-edges it))))
         (lenient (when directed lenient))
         (nodes (mapcar #'name<-node+ (merge-link-nodes-in-nodes links nodes+ :lenient lenient))))
    (as-plist nodes links directed name lenient)))

(defun raw-graph? (item)
  "=> raw-graph
A raw graph is a property list with the following keys: (:name :nodes :links :directed :lenient).
So it's like a regular graph except there's no constraint on its nodes."
  (and (plistp item)
       (list= (plist-keys item) '(:name :nodes :links :directed :lenient))
       (every #'link? (getf item :links))))

(defun raw-graph<-graph (graph)
  "=> (raw-graph nodes)
Make a raw graph out of the given graph, keeping only the name of the nodes as being nodes themselves and returning as a secondary value the primary nodes of the graph."
  (values
   (make-raw-graph (mapcar #'name<-node+ (graph-nodes graph))
                   :links (graph-links graph) :directed (graph-directed? graph)
                   :name (graph-name graph) :lenient (graph-lenient? graph))
   (graph-nodes graph)))

(defun graph<-raw-graph (raw-graph nodes)
  "=> graph
Make a graph from the given raw-graph, fetching the full nodes information in the given nodes.
A warning will be issued in case some names held by the raw-graph cannot be found in the given nodes. A graph will still be returned but with the raw nodes when full nodes haven't be found."
  (let (missing-names found-nodes)
    (dolist (node-name (graph-nodes raw-graph))
      (aif (known-node? node-name nodes)
           (push it found-nodes)
           (push (push node-name missing-names) found-nodes)))
    (awhen missing-names
      (warn "Some node's names were not found while making a graph from the raw-graph ~a~%Cannot find: ~a in ~a"
            raw-graph it nodes))
    (make-graph% (nreverse found-nodes)
                 :links (graph-links raw-graph) :directed (graph-directed? raw-graph)
                 :lenient (graph-lenient? raw-graph) :name (graph-name raw-graph))))

(defun make-graph-cluster (graphs &key name)
  "=> graph-cluster
A graph cluster is a collection of raw-graphs with the full nodes information stored externally."
  (let (raw-graphs nodes)
    (dolist (graph graphs)
      (multiple-value-bind
          (raw-graph raw-graph-nodes)
          (raw-graph<-graph graph)
        (push raw-graph raw-graphs)
        (setf nodes (append nodes raw-graph-nodes))))
    (as-plist raw-graphs nodes name)))

(defun graph-cluster? (item)
  "=> graph-cluster
A graph cluster is a property list with the following keys: (:nodes :raw-graphs :name)."
  (and (plistp item)
       (list= (plist-keys item) '(:nodes :raw-graphs :name))
       (every #'node? (getf item :nodes))
       (every #'raw-graph? (getf item :raw-graphs))))

(defun graph-cluster-nodes (graph-cluster)
  "=> nodes"
  (getf graph-cluster :nodes))

(defun graph-cluster-raw-graphs (graph-cluster)
  "=> raw-graphs"
  (getf graph-cluster :raw-graphs))

(defun graph-cluster-name (graph-cluster)
  "=> item"
  (getf graph-cluster :name))

(defun graph-cluster-graphs (graph-cluster)
  "=> graphs"
  (mapcar (lambda (raw-graph)
            (graph<-raw-graph raw-graph (getf graph-cluster :nodes)))
           (getf graph-cluster :raw-graphs)))

(defun make-raw-graph-cluster (raw-graphs &key name)
  "=> raw-graph-cluster"
  (as-plist raw-graphs name))

(defun raw-graph-cluster? (item)
  "=> graph-cluster
A raw graph cluster is a property list with the following keys: (:raw-graphs :name)."
  (and (plistp item)
       (list= (plist-keys item) '(:raw-graphs :name))
       (every #'raw-graph? (getf item :raw-graphs))))

(defun raw-graph-cluster<-graph-cluster (graph-cluster)
  "=> raw-graph-cluster"
  (values (make-raw-graph-cluster (graph-cluster-raw-graphs graph-cluster)
                                  :name (graph-cluster-name graph-cluster))
          (graph-cluster-nodes graph-cluster)))

(defun make-graph-set (graphs &key name)
  "=> graph-set
A graph set is a graph cluster where the raw-graphs have been trimmed of graphs with only one node. These graphs are stored in the singles property. The full node information of all raw graphs is stored in the nodes property."
  (let (raw-graphs nodes singles)
    (dolist (graph graphs)
      (multiple-value-bind
          (raw-graph raw-graph-nodes)
          (raw-graph<-graph graph)
        (if (= 1 (length (graph-nodes raw-graph)))
          (push raw-graph singles)
          (push raw-graph raw-graphs))
        (setf nodes (append nodes raw-graph-nodes))))
    (as-plist raw-graphs nodes singles name)))

(defun graph-set? (item)
  "=> graph-set
A graph set is a property list with the following keys: (:nodes :singles :raw-graphs :name)."
  (and (plistp item)
       (list= (plist-keys item) '(:nodes :raw-graphs :name :singles))
       (every #'raw-graph? (getf item :raw-graphs))
       (every #'raw-graph? (getf item :singles))))

(defun graph-set-raw-graphs (graph-set)
  "=> raw-graphs"
  (getf graph-set :raw-graphs))

(defun graph-set-graphs (graph-set)
  "=> graphs"
  (mapcar (lambda (raw-graph)
            (graph<-raw-graph raw-graph (getf graph-set :nodes)))
          (getf graph-set :raw-graphs)))

(defun make-graph-cluster-set (graph-clusters &key name)
  "=> graph-cluster-set
A graph cluster set has the same structure than a graph set except it holds raw graph clusters instead of raw graphs."
  (let (raw-graph-clusters nodes singles)
    (dolist (graph-cluster graph-clusters)
      (multiple-value-bind
          (raw-graph-cluster raw-graph-cluster-nodes)
          (raw-graph-cluster<-graph-cluster graph-cluster)
        (if (= 1 (length raw-graph-cluster-nodes))
          (push raw-graph-cluster singles)
          (push raw-graph-cluster raw-graph-clusters))
        (setf nodes (append nodes raw-graph-cluster-nodes))))
    (as-plist raw-graph-clusters nodes singles name)))

(defun graph-cluster-set? (item)
  "=> graph-cluster-set
A graph cluster set is a property list with the following keys: (:nodes :singles :raw-graph-clusters :name)."
  (and (plistp item)
       (list= (plist-keys item) '(:nodes :raw-graph-clusters :name :singles))
       (every #'raw-graph-cluster? (getf item :raw-graph-clusters))
       (every #'raw-graph-cluster? (getf item :singles))))