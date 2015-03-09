(in-package :my-graphs)

(defun make-node (name &key class info)
  "=> node
  Constructor for a node.
Avoid using a list as the name because a node is seen as a list of 3 elements for now."
  (when (listp name)
    (warn "Using the list ~a as the name of a node may cause conflicts when building edges and links.~%Because they only hold names and not the actual nodes, and nodes are internally represented as lists.~%Until the development tools are able to better handle structures or objets, this representation will probably still be used."))
  (list name class info))

(defun node? (item)
  "=> node
  A node is a list: (name class info)."
  (and (listp item) (= 3 (length item)) item))

(defun node-name (node)
  "=> name
  Return the name of the node."
  (first node))

(defsetf node-name (node) (value)
  `(setf (first ,node)
         ,value))

(defun node= (a b)
  "=> node
  Two nodes are strongly equal when they have the same name, same classes and same information.
  Return the first node when it is so."
  (and (my-equal (node-name a) (node-name b))
       (my-equal (node-class a) (node-class b))
       (my-equal (node-info a) (node-info b))
       a))

(defun node-equal (a b)
  "=> node
Two nodes are weakly equal when they have the same name.
Their classes and additional information do not matter.
Return the first node when it is so."
  (and (my-equal (node-name a) (node-name b))
       a))

(defun name<-node+ (node+)
  "=> item
A node+ is either a node or just its name.
Return the name of the node."
  (if (node? node+)
    (node-name node+)
    node+))

(defun node<-node+ (node+)
  "=> node
Return a node, either the node+ if it is already a node or a node whose name is node+."
  (if (node? node+)
    node+
    (make-node node+)))

(defun extended-node-name (node) (if (atom node) node (node-name node)))

(defun node-class (node)
  "=> item
  Return the class of the node."
  (second node))

(defsetf node-class (node) (value)
  `(setf (second ,node)
         ,value))

(defun node-info (node)
  "=> item
  Return the additional information attached to the node."
  (third node))

(defsetf node-info (node) (value)
  `(setf (third ,node)
         ,value))

(defun make-edge (head-node+ tail-node+ &key class)
  "=> edge"
  (list (name<-node+ head-node+)
        (name<-node+ tail-node+)
        class))

(defun edge? (item)
  "=> edge
  An edge is a list: (head-node-name tail-node-name class info)."
  (and (listp item)
       (= 3 (length item))
       item))

(defun edge-head-node-name (edge)
  "=> item
  Return the name of the node at the head of the edge."
  (first edge))

(defun edge-tail-node-name (edge)
  "=> item
  Return the name of the node at the tail of the edge."
  (second edge))

(defun edge-class (edge)
  "=> item
  Return the class of the edge."
  (third edge))

(defun edge-equal (a b &key (directed t))
  "=> edge
  Two edges are weakly equal when their head nodes have the same name and their tail nodes also have the same name. Their classes are ignored.
  By default, it is thus assumed the given edges are part of directed graphs. If not, set the directed key argument to nil and the two edges will be considered equal regardless of heads and tails as long as their names are equal two by two."
  (let ((directed-result
         (and (my-equal (edge-head-node-name a) (edge-head-node-name b))
              (my-equal (edge-tail-node-name a) (edge-tail-node-name b)))))
    (if directed
        directed-result
      (or directed-result
          (and (my-equal (edge-head-node-name a) (edge-tail-node-name b))
               (my-equal (edge-tail-node-name a) (edge-head-node-name b)))))))

(defun edge= (a b &key (directed t))
  "=> edge
  Two edges are strongly equal when their head nodes have the same name, when their tail nodes also have the same name and when they have the same class.
  By default, it is thus assumed the given edges are part of directed graphs. If not, set the directed key argument to nil and the two edges will be considered equal regardless of heads and tails as long as their names are equal two by two."
  (let ((directed-result
         (and (my-equal (edge-class a) (edge-class b))
              (my-equal (edge-head-node-name a) (edge-head-node-name b))
              (my-equal (edge-tail-node-name a) (edge-tail-node-name b)))))
    (if directed
        directed-result
      (or directed-result
          (and (my-equal (edge-class a) (edge-class b))
               (my-equal (edge-head-node-name a) (edge-tail-node-name b))
               (my-equal (edge-tail-node-name a) (edge-head-node-name b)))))))

(defun revert-edge (edge)
  "=> edge
Revert the edge: the head becomes the tail, the tail becomes the head, the class is unchanged."
  (make-edge (edge-tail-node-name edge)
             (edge-head-node-name edge)
             :class (edge-class edge)))

(defun make-link (start-node successors &optional class)
  "A link object is a pack of edges which are all starting on the same node and are of the same class.
It is represented as a list: (start-node-name &optional class (&rest successor-node-name))
Class must be a keyword otherwise it will be ignored (but a warning will be issued).
As a convenience, make-links allows:
- start-node to actually not be an atom: it will be considered as the name of the start-node.
- successors to be a list with certain elements that are as well only an atom. For such elements, that will be considered
as a the name of the node.
- successors itself can be an atom in which case it will be considered there is only one successor and that will be its name."
  (let ((result (list (extended-node-name start-node))))
    (push class result)
    (push (cond ((null successors) nil)
                ((atom successors) (list successors))
                (t
                 (mapcar (lambda (node) (extended-node-name node))
                         successors)))
          result)
    (nreverse result)))

(defun make-link% (head-node+ successor-nodes+ &key class)
  "=> link
Make a link from the head node+ to the given successor nodes+."
  (list (name<-node+ head-node+)
        class
        (mapcar #'name<-node+ successor-nodes+)))

(defun link? (item)
  "=> link
A link is a list (head-node-name class successor-node-names)."
  (and (listp item)
       (= 3 (length item))))

(defun link-head-node-name (link)
  "=> node
Return the name of the head node of the link."
  (first link))

(defun link-class (link)
  "=> item
Return the class of the given link."
  (second link))

(defsetf link-class (link) (value)
  `(setf (second ,link)
         ,value))

(defun link-equal (a b)
  "=> link
Two links are weakly equal when their head node's names are the same and their classes are also the same.
The names of their successor nodes do not matter."
  (and (my-equal (link-head-node-name a) (link-head-node-name b))
       (my-equal (link-class a) (link-class b))
       a))

(defun link= (a b)
  "=> link
Two links are strongly equal when their head node's name are the same, their same class are the same and their successor nodes' names are the same."
  (and (my-equal (link-head-node-name a) (link-head-node-name b))
       (my-equal (link-class a) (link-class b))
       (list= (link-successor-nodes-names a) (link-successor-nodes-names b) :test #'my-equal)
       a))

(defun link-start-node-name (link)
  "Return the start node name of the given links."
  (first link))

(defun link-successors (link) (third link))

(defsetf link-successors (link) (value)
  `(setf (third ,link)
         ,value))

(defun link-successor-nodes-names (link)
  "=> items
Return the names of the successor nodes of the link."
  (third link))

(defsetf link-successor-nodes-names (link) (value)
  `(setf (third ,link)
         ,value))

(defun link-successor-name (successor)
  (if (listp successor) (first successor) successor))

(defun link-successor-label (successor)
  (when (listp successor) (second successor)))

(defun link-add-successor (link value)
  (unless (member (link-successor-name value)
                  (mapcar #'link-successor-name (link-successors link))
                  :test
                  #'equal)
    (setf (third link) (cons value (third link))))
  link)

(defun known-node? (node+ nodes+)
  "=> (node+ index)
A node is known of a list of nodes when it is weakly equal with at least one node in it.
When known, return the first element of the nodes+ to which it was equal."
  (let ((node (node<-node+ node+))
        found index)
    (loop for i from 0
          for in-node+ in nodes+
          while (not found)
          when (node-equal node (node<-node+ in-node+))
          do (setf found in-node+ index i))
    (values found index)))

(defun known-edge? (edge edges &key (directed t))
  "=> edge
An edge is known of a list of edges when it is strongly equal with at least one edge in it.
By default, edges are assumed to be directed.
Return the first edge to which it was equal."
  (and (member edge edges :test (lambda (a b)
                                  (edge= a b :directed directed)))
       edge))

(defun known-link? (link links)
  "=> (link index)
A link is known of a list of link when it is weakly equal with at least one link in it.
Return the first link to which it was equal."
  (let (found index)
    (loop for i from 0
          for in-link in links
          while (not found)
          when (link-equal link in-link)
          do (setf found in-link index i))
    (values found index)))

(defun add-successor (node+ link)
  "=> link
Unless known, add the node+ to the successor nodes' names of the given link.
Return the updated link."
  (let ((name (name<-node+ node+)))
    (unless (known-node? node+ (link-successor-nodes-names link))
      (setf (link-successor-nodes-names link) (cons name (link-successor-nodes-names link))))
    link))

(defun make-graph
       (nodes links &key (directed t) (name "") lenient)
  "=> graph
  The default for a graph is to be directed."
  (list :nodes
        nodes
        :links
        links
        :directed
        directed
        :name
        name
        :lenient
        lenient))

(defun link<-edge (edge)
  "=> link
Make a link from the given edge.
The resulting link will of course only have one successor. Its class will be the edge's class."
  (make-link% (edge-head-node-name edge)
              (list (edge-tail-node-name edge))
              :class (edge-class edge)))

(defun edges<-link (link)
  "=> edges
Make edges from the given link, one for each of its successors.
The resulting edges will of course all have the same head node. Their classes will be the link's class."
  (loop for node-name in (link-successor-nodes-names link)
        collect (make-edge (link-head-node-name link)
                           node-name
                           :class (link-class link))))

(defun merge-edges-in-links (edges links &key (directed t))
  "=> links
Include each unknown link derived from the given edges in the given links. When the link is known, add all its unknown successors to the present link.
If directed is nil, also merge the opposite edges."
  (let ((result-links (copy-tree links)))
    (flet ((merge-edge (edge)
             (let ((new-link (link<-edge edge)))
               (multiple-value-bind
                   (link index)
                   (known-link? new-link result-links)
                 (if index
                   (setf (elt result-links index)
                         (add-successor (edge-tail-node-name edge) link))
                   (push new-link result-links))))))
      (dolist (edge edges)
        (merge-edge edge)
        (unless directed
          (merge-edge (revert-edge edge)))))
    result-links))

(defun compute-opposite-edges (links)
  "=> edges
Compute the opposite edges of every edge made from the given links."
  (mapcar #'revert-edge (loop for link in links
                              append (edges<-link link))))

(defun complete-with-opposite-edges (links)
  "=> links
Complete the given links with all opposite edges made from them.
The resulting links are fit for undirected graphs."
  (merge-edges-in-links (compute-opposite-edges links) links))

(defun merge-link-nodes-in-nodes (links nodes+ &key (lenient t))
  "=> nodes
Include each unknown head node of the given links in the given nodes.
If lenient is nil, which is not the default, this action will also include all the unknown successors of each link."
  (remove-duplicates
   (append (mapcar #'node<-node+ nodes+)
           (mapcar (lambda (node-name)
                     (make-node node-name))
                   (append
                    (mapcar #'link-head-node-name links)
                    (unless lenient
                      (loop for link in links
                            append (link-successor-nodes-names link))))))
   :test #'node-equal))

(defun make-graph% (nodes &key links edges (directed t) (name "") lenient)
  "=> graph
A graph is a structure holding nodes and connections between them called edges. Internally however, things are organized by links because it makes a lot of processes operating on graphs much easier.
Nodes are uniquely identified by their names alone. So the standard equality of nodes used in a graph is the weak one, node-equal. An edge however is identified by the strong equality edge=, that is its class matters. So it is too for links but in their case that means they are identified by the weak equality link-equal.
To rephrase it, when you work with a graph, you connect things together. You qualify them and maybe attach some additional information to them. But it's their names that matter to identify them. Hence the use of the weak equality between nodes.
When you connect nodes, it is for a reason. And the qualification of the connection should reflect that reason. Hence the use of the strong equality for edges, because you can connect two nodes for several reasons and each of these connections should be thought of as different.
And finally, a link is about a way of connecting nodes. So the important notion is the node from whence you connect as well as the qualification of the connection. The basic question to identify a link is: \"does connecting from node Foo by virtue of reason Lambda means something for the given graph?\" Therefore the actual successor nodes of the connection do not matter.
By default, a graph is unnamed, directed and strict.
This last characteristic is enforced by this action. All the nodes not listed in the given nodes that are yet part of the given links and edges are automatically added to the nodes of the graph.
If :lenient is true however, only the head nodes of links and edges are added to the nodes of the graph. Note that being lenient only applies for a directed graph. Since in an undirected graph, all connections happen both ways, every node has to be in the graph. Therefore lenient is always nil when directed is nil too."
  (let* ((links (let-a (merge-edges-in-links edges links)
                  (if directed
                    it
                    (complete-with-opposite-edges it))))
         (lenient (when directed lenient))
         (nodes (merge-link-nodes-in-nodes links nodes :lenient lenient)))
    (as-plist nodes links directed name lenient)))

(defun graph? (item)
  "=> graph
A graph is a property list with the following keys: (:name :nodes :links :directed :lenient)."
  (and (plistp item)
       (list= (plist-keys item) '(:name :nodes :links :directed :lenient))
       (every #'node? (getf item :nodes))
       (every #'link? (getf item :links))
       item))

(defun graph-nodes (graph)
  "=> nodes
Return the nodes at the core of the graph.
In the case of a strict graph, it's all the nodes but for a lenient graph, unknown nodes are permitted as tail nodes."
  (getf graph :nodes))

(defun graph-links (graph)
  "=> links
Return the links of the graph."
  (getf graph :links))

(defsetf graph-links (graph) (value)
  `(setf (getf ,graph :links)
         ,value))

(defun graph-edges (graph)
  "=> edges
Return the edges of the graph.
Note that since a graph only contains the connections in the form of links, this action implies computing the edges each time. If the graph is directed, all connections will be returned but if not, only one edge will be returned per connected nodes even if they are technically connected in both ways. In such case, please do not assume anything about which node gets to be the head and which one gets to be the tail."
  (let (edges)
    (dolist (link (graph-links graph))
      (let ((head-node-name (link-head-node-name link)))
        (dolist (node-name (link-successor-nodes-names link))
          (let ((edge (make-edge head-node-name node-name :class (link-class link))))
            (if (graph-directed? graph)
              (push edge edges)
              (unless (known-edge? edge edges :directed nil)
                (push edge edges)))))))
    edges))

(defun graph-directed? (graph)
  "=> boolean
A directed graph means the order of nodes in an edge is meaningful. There's a head node and a tail node."
  (getf graph :directed))

(defun graph-lenient? (graph)
  "=> boolean
A lenient graph is a graph permitting to have unknown tail nodes. Useful when working with several graphs being clusters of a whole set of nodes. The nodes not encompassed can then be part of other clusters.
Note that an undirected graph is never lenient since tail nodes are also head nodes."
  (getf graph :lenient))

(defun graph-strict? (graph)
  "=> boolean
A strict graph is a non-lenient graph. All nodes must be known, tail nodes included.
An undirected graph is always strict since connections happen both ways."
  (not (graph-lenient? graph)))

(defun graph-equal (a b)
  "=> graph
Two graphs are weakly equal when they have the same characteristics (directed or undirected, lenient or strict) and when they contain the same nodes and links, in the weak sense of the terms (node-equal and link-equal). Therefore, they are allowed to differ on the exact successors for each link."
  (and (equal (graph-directed? a) (graph-directed? b))
       (equal (graph-lenient? a) (graph-lenient? b))
       (list= (graph-nodes a) (graph-nodes b) :test #'node-equal)
       (list= (graph-links a) (graph-links b) :test #'link-equal)
       a))

(defun graph= (a b)
  "=> graph
Two graphs are strongly equal when they have the same characteristics (directed or undirected, lenient or strict) and when they contain the same nodes and links, in the strong sense of the terms (node= and link=). Therefore, for each link, the successors must also match."
  (and (equal (graph-directed? a) (graph-directed? b))
       (equal (graph-lenient? a) (graph-lenient? b))
       (list= (graph-nodes a) (graph-nodes b) :test #'node-equal)
       (list= (graph-links a) (graph-links b) :test #'link-equal)
       a))

(defun graph-name (graph)
  "=> item
Return the name of the graphs, if any."
  (getf graph :name))

(defsetf graph-name (graph) (value)
  `(setf (getf ,graph :name) ,value))

(defun locate-node (node+ graph)
  "=> (node index)
Locate the node in the graph and return it, if it is known. Return nil otherwise."
  (known-node? node+ (graph-nodes graph)))

(defun find-node (node+ graph)
  "=> node
Find the node in the graph."
  (multiple-value-bind
      (node index)
      (locate-node node+ graph)
    (declare (ignore index))
    node))

(defun position-node (node+ graph)
  "=> index
Return the index of the node in the graph."
  (multiple-value-bind
      (node index)
      (locate-node node+ graph)
    (declare (ignore node))
    index))

(defun graph-node (name-or-pos graph)
  "Return both the node and its index."
  (cond ((or (stringp name-or-pos) (symbolp name-or-pos))
         (let (found pos)
           (loop for i from 0
                 for node in (graph-nodes graph)
                 while (not found)
                 when (equal name-or-pos (node-name node))
                   do (setf found node pos i))
           (values found pos)))
        ((integerp name-or-pos)
         (values (elt (graph-nodes graph) name-or-pos) name-or-pos))
        (t
         (error "Cannot find a node with information ~a. Give a name or a index to retrieve a node."
                name-or-pos))))

(defun graph-node-position (node graph)
  (multiple-value-bind (node pos)
      (graph-node (extended-node-name node) graph)
    (declare (ignore node))
    pos))

(defun graph-add-node (node graph) (push node (getf graph :nodes)))

(defun add-node (node+ graph)
  "=> graph
Add the node to the graph, if it is unknown. Return the graph."
  (let ((node (node<-node+ node+)))
    (unless (find-node node graph)
      (push node (getf graph :nodes)))
    graph))

(defun graph-link (node graph &optional (class nil class-provided?))
  "Locate the links with node as a start node.
If class is given, return both the link with that class and its position.
Note that when class is given as nil, this function will return the link which has no class."
  (let ((start-node-name (extended-node-name node)))
    (if class-provided?
        (let (found pos)
          (loop for i from 0
                for link in (graph-links graph)
                while (not found)
                when (and (equal start-node-name
                                 (link-start-node-name link))
                          (equal class (link-class link)))
                  do (setf found link pos i))
          (values found pos))
      (loop for link in (graph-links graph)
            when (equal start-node-name (link-start-node-name link))
              collect link))))

(defun locate-link (link graph)
  "=> (link index)
Locate the link in the graph and return it if it is known. Return nil otherwise."
  (known-link? link (graph-links graph)))
    
(defun find-link (link graph)
  "=> link
Find the link in the graph."
  (multiple-value-bind
      (link index)
      (locate-link link graph)
    (declare (ignore index))
    link))

(defun position-link (link graph)
  "=> index
Return the index of the link in the given graph."
  (multiple-value-bind
      (link index)
      (locate-link link graph)
    (declare (ignore link))
    index))

(defun graph-add-link (link graph)
  (flet ((add-new-node (name)
           (unless (graph-node name graph)
             (graph-add-node (make-node name) graph)))
         (merge-link (new-link)
           (let ((start-node-name (link-start-node-name new-link)))
             (multiple-value-bind (link pos)
                 (graph-link start-node-name
                             graph
                             (link-class new-link))
               (if pos
                   (setf (elt (getf graph :links) pos)
                         (make-link start-node-name
                                    (append (link-successors link)
                                            (link-successors new-link))
                                    (link-class new-link)))
                 (push new-link (getf graph :links)))))))
    (let ((start-node-name (link-start-node-name link)))
      (add-new-node start-node-name)
      (merge-link link)
      (dolist (name (mapcar #'link-successor-name
                            (link-successors link)))
        (add-new-node name)
        (when (not (graph-directed? graph))
          (merge-link (make-link name (list start-node-name))))))))

(defun add-link (link graph)
  "=> graph
Add the link to the graph, when unknown, and return the graph.
If the link is known, all its unknown successors will be added to the present link.
Note this action preserves the graph's leniency.
That is, whether if the graph is lenient or strict, all unknown head nodes will complement the nodes of the graph. And additionnally, if the graph is strict, the unknown nodes of every links' successors will be added to the graph."
  (flet ((merge-link (new-link)
           (let ((head-node-name (link-head-node-name new-link)))
             (multiple-value-bind
                 (link index)
                 (locate-link new-link graph)
               (if index
                 (setf (elt (graph-links graph) index)
                       (make-link% head-node-name
                                   (remove-duplicates
                                    (append (link-successor-nodes-names link)
                                            (link-successor-nodes-names new-link))
                                    :test #'my-equal)
                                   :class (link-class new-link)))
                 (push new-link (graph-links graph)))))))
    (let ((head-node-name (link-head-node-name link)))
      (add-node head-node-name graph)
      (merge-link link)
      (dolist (node-name (link-successor-nodes-names link))
        (when (graph-strict? graph)
          (add-node node-name graph))
        (when (not (graph-directed? graph))
          (merge-link (make-link% node-name (list head-node-name))))))))

(defun add-edge (edge graph)
  "=> graph
Add the edge to the graph, unless known, and return the graph.
Note this action preserves the graph's leniency.
The head node will be added to the graph's nodes if it is unknown and if the graph is strict, the same will be performed for the edge's tail node."
  (add-link (link<-edge edge) graph))

(defun graph-node-successor-names (node graph)
  (let (result)
    (dolist (link (graph-link node graph))
      (dolist (successor (link-successors link))
        (push (link-successor-name successor) result)))
    (nreverse result)))

(defun find-links-with-head-node (head-node+ graph)
  "=> links
Return all the links having the given head-node+ as their head node."
  (let ((node-name (name<-node+ head-node+)))
    (loop for link in (graph-links graph)
          when (my-equal (link-head-node-name link)
                         node-name)
          collect link)))

(defun find-node-successors (node+ graph &key (class nil class-provided?))
  "=> nodes+
Find the successors of the given node+, if any, in the graph.
If the class parameter is informed, this action will try to find the link weakly equal to the one with node+ as its head node and the given class as its class.
If the class perimeter is not provided, this action will search for all the links having the node+ as their head node."
  (if class-provided?
    (awhen (find-link (make-link% node+ nil :class class) graph)
      (link-successor-nodes-names it))
    (loop for link in (find-links-with-head-node node+ graph)
          append (link-successor-nodes-names link))))

(defun node-position-id (node-position) (first node-position))

(defun clustered-graph-components (clustered-graph)
  (getf clustered-graph :components))

(defsetf clustered-graph-components (clustered-graph) (value)
  `(setf (getf ,clustered-graph
               :components)
         ,value))

(defun clustered-graph-component-clusters (component)
  (getf component :clusters))

(defun clustered-graph-component-cluster (name component)
  (get-named-plist name (clustered-graph-component-clusters component)))

(def-access clustered-graph-component-stats
            nil
            component
            (getf component :stats))

(defun clustered-graph-clusters (clustered-graph)
  (mapcar #'clustered-graph-component-clusters
          (clustered-graph-components clustered-graph)))

(defun clustered-graph-singles (clustered-graph)
  (getf clustered-graph :singles))

(defun clustered-graph-singles-nodes (clustered-graph)
  (getf (getf clustered-graph :singles) :nodes))

(defsetf clustered-graph-singles-nodes (clustered-graph) (value)
  `(setf (getf (getf ,clustered-graph
                                            :singles)
                                      :nodes)
                                ,value))

(defun clustered-graph-singles-stats (clustered-graph)
  (getf (getf clustered-graph :singles) :stats))

(defsetf clustered-graph-singles-stats (clustered-graph) (value)
  `(setf (getf (getf ,clustered-graph
                                            :singles)
                                      :stats)
                                ,value))

