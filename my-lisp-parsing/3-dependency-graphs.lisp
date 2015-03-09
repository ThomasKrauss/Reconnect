(in-package :my-lisp-parsing)

(defparameter *dot-node-class-map*
  '((:solid :default)
    (:dashed :unused)
    (:dotted :to-be-used)
    (:bold :narrow)
    (:rounded :exported)
    (:diagonals :wide)
    (:filled :end)
    (:striped :ok)
    (:wedged :error)
    (:radial :warning))
  "Ten node styles are available in Dot: solid, dashed, dotted, bold, rounded, diagonals, filled, striped, wedged and radial. For purpose of sources parsing, 8 classes are used. Here's their purpose.
First of all, an action is either internal or external to its package.
Then it may be unused, meaning that either no actions are using it or that all actions that use it are themselves unused. In other words, the unused property is transitive and you can thus identify big unused dependency chains.
There is another flavor of uselessness for internal actions: to-be-used. All the actions of the useless chain are still unused but at least one of them is external. This flavor helps to distinguish truly useless chains, because they are entirely internal to a system, from chains containing actions that are part of the API and may thus not be useless much longer.
If an action is used, then it may be interesting to know a little bit more how it is used. The narrow class is used to indicate an action whose scope of use is limited. If the action is internal, its relevance is considered narrow when used only in its own module. If the action is external, its relevance is narrow when it is only in its own system.
Finally, the following 3 classes are about usage result and are mutually exclusive:
- ok: all usages are ok
- error: at least one usage failed
- warning: all usages are ok (no usage is fine too) but the action relies on other actions which are ko.")

(defparameter *dot-edge-class-map*
  '((:solid :run)
    (:dotted :install)
    (:dashed :error))
  "Four edge styles are available in Dot:
solid, dashed, dotted and bold.")

(defconstant +my-systems-cluster-name+ "my-systems")

(defconstant +other-systems-cluster-name+ "other-systems")

(defun count-action-usage (used-actions)
  (let (action-register)
    (dolist (action-name used-actions)
      (aif (position action-name action-register :key #'first)
           (setf (elt action-register it)
                 (list action-name (1+ (second (elt action-register it)))))
           (push (list action-name 1) action-register)))
    action-register))

(defun make-weight-register (action-register)
  (let (weight-register)
    (loop for (action-name weight) in action-register
          do (aif (position weight weight-register :key #'first)
                  (setf (elt weight-register it)
                        (list weight (cons action-name (second (elt weight-register it)))))
                  (push (list weight (list action-name)) weight-register)))
    weight-register))

(defun make-weighted-action-links (action-name used-actions link-class)
  (loop for (weight used-actions) in (make-weight-register (count-action-usage used-actions))
        collect (make-link action-name used-actions (append link-class (list :weight weight)))))

(defun add-declared-dependencies (root-graph)
  (let ((my-systems (system-list-all-loaded)))
    (dolist (node (graph-nodes root-graph))
      (let ((system-name (node-name node)))
        (when (member system-name my-systems :test #'equal)
          (graph-add-link (make-link system-name
                                     (get-system-declared-dependencies system-name)
                                     (list :install))
                          root-graph)))))
  root-graph)

(defun make-module-graph (actions module-name)
  "Return the list of connected components from the given actions.
Return as a secondary value the list of nodes that really constitute the nodes of the graph, since the given actions can be linked to actions of other modules or systems."
  (let ((graph (make-graph nil nil :name module-name))
        nodes)
    (dolist (action actions)
      (let-a (if (atom (getf action :name))
               (make-node (getf action :name))
               (make-node (first (getf action :name)) :class (second (getf action :name))))
        (push it nodes)
        (graph-add-node it graph))
      (awhen (getf action :run-actions)
        (mapcar (lambda (link)
                  (graph-add-link link graph))
                (make-weighted-action-links (getf action :name) it (list :run))))
      (awhen (getf action :install-actions)
        (mapcar (lambda (link)
                  (graph-add-link link graph))
                (make-weighted-action-links (getf action :name) it (list :install)))))
    (values graph nodes)))

(defun make-system-graph (system-name clustered-graphs)
  (let ((graph (make-graph nil nil :name system-name))
        nodes)
    (dolist (clustered-graph clustered-graphs)
      (let* ((module-name (graph-name clustered-graph))
             (node (make-node module-name)))
        (graph-add-node node graph)
        (push node nodes)
        (awhen (remove-duplicates
                (flatten (loop for component in (clustered-graph-components clustered-graph)
                             if (remove module-name
                                        (loop for cluster in (clustered-graph-component-clusters component)
                                              collect (graph-name cluster))
                                        :test #'string=)
                             collect it))
              :test #'string=)
          (graph-add-link (make-link module-name it) graph))))
    (values graph nodes)))

(defun make-root-graph (system-items)
  (let ((graph (make-graph nil nil :name +my-systems-cluster-name+))
        nodes)
    (dolist (system-graph (mapcar (lambda (system-item)
                                    (getf system-item :graph))
                                  system-items))
      (let* ((system-name (graph-name system-graph))
             (node (make-node system-name)))
        (graph-add-node node graph)
        (push node nodes)
        (awhen (flatten
                (mapcar (lambda (component)
                          (graph-nodes (find +other-systems-cluster-name+
                                             (clustered-graph-component-clusters component)
                                             :test #'string=
                                             :key (lambda (cluster)
                                                    (getf cluster :name)))))
                        (clustered-graph-components system-graph)))
          (graph-add-link (make-link system-name it) graph))))
    (add-declared-dependencies graph)
    (values graph nodes)))

(defun update-module-graph-node (action action-names system-name system-graphs)
  "Search and update: return the name of the module containing the action if it is in the same system
or the name of the system if it is not the same."
  (let (found
        (target-system-name (symbol-system-name action)))
    (loop for clustered-graph in (getf+ system-graphs target-system-name :in)
          while (not found)
          do (let ((module-name (graph-name clustered-graph)))
               (awhen (position action (graph-nodes clustered-graph) :test #'equal :key #'node-name)
                 (setf found module-name
                       (node-info (elt (graph-nodes clustered-graph) it))
                       (append (node-info (elt (graph-nodes clustered-graph) it)) action-names)))))
    (if (string= target-system-name system-name)
      found
      target-system-name)))

(defun cluster-dependency-component (component name nodes other-cluster-fn)
  (let ((node-clusters (list (list name)))
        (direct-ancestors-graph (make-direct-ancestors-graph component)))
    (flet ((add-node (node cluster-name)
             (aif (position cluster-name node-clusters :test #'equal :key #'first)
                  (setf (elt node-clusters it)
                        (cons cluster-name (append (rest (elt node-clusters it))
                                                   (list node))))
                  (push (list cluster-name node) node-clusters)))
           (get-successor-names (node)
             (mapcar #'link-successor-name
                     (link-successors (graph-link node direct-ancestors-graph nil)))))
      (dolist (node (graph-nodes component))
        (aif (position (node-name node) nodes :test #'equal :key #'node-name)
             (progn
               (setf (node-info (elt nodes it))
                     (append (node-info (elt nodes it))
                             (get-successor-names node)))
               (add-node node name))
             (add-node node (funcall other-cluster-fn node (get-successor-names node)))))
      (let-a (loop for cluster in node-clusters
                   collect (make-subgraph (rest cluster) component :name (first cluster)))
        (values it
                (graph-topological-sort
                 (find name it :test #'string= :key (lambda (cluster) (getf cluster :name)))
                 :lenient-search t))))))

(defun build-module-clustered-graph (module-name system-name system-graphs)
  "Deeply organizing the various connected components of a module graph to get to the module graph structure:
the property list (:singles :components :nodes).
It needs much information in order to acquire efficiently all the following benefits:
- have the real, topologically sorted list of nodes that are part of the modules (since actions may be linked to actions of other modules or systems)
- the list of components reduced to a single node (they do not need further graph processing and are thus handled as a pure node, not a graph)
- the other connected components, as a list of clusters which are separating the various actions as follow: actions are packed per module into a cluster if they are of the given system-name else they are packed into a only one cluster.
Calling make-module-graph also have the following side-effect on the given system-graphs:
any action of any system that happens to be used by the actions gets its related information updated to reflect that.
In other words, once make-module-graph have terminated its computations, every other nodes in all other module-graphs are up to date regarding if they are used and if so, by which actions.
The cluster of default name must appear at the end so that the Dot layout using clusters do not start to layout links that should go outside the cluster inside it (it's the central cluster: no links exist between the other clusters. By appearing first, Dot will not know this central cluster's links point to other clusters and will by default put the end nodes in it).
The argument module-graphs will be modified: each node maintain in its info slot the list of actions using it.
A module graph is a list of the connected components of the overall graph of dependencies about the actions of a module.
In practice, it is a plist of:
- the actions that are in the given module
- the list of connected components not reduced to a single node (and, therefore, which have no links to another system different of the Common Lisp system or any implementation-specific systems).
- the list of connected components which are reduced to single nodes.
Each such connected components is organized in clusters in the following manner:
- all actions of a module which is part of the same system as the module of the given actions are packed in a cluster
- all actions from a given different system are packed together, regardless of their module
In other words, for the system behind the module of the given actions, all the actions of this system are packed in several clusters with a module-level granularity, whereas for actions of other systems, they are just packed together with a system-level granularity only.
In order to have this organization in clusters, the argument module-graphs must be informed. The info of the nodes of each module graph will be updated to have the list of all actions using them."
  (with-system (system-name)
    (multiple-value-bind
        (graph nodes)
        (make-module-graph (parse-module-actions module-name system-name) module-name)
      (split-in-clusters
       graph nodes
       (lambda (component)
         (cluster-dependency-component component module-name nodes
                                       (lambda (node successor-names)
                                         (update-module-graph-node (node-name node) successor-names
                                                                   system-name system-graphs))))))))

(defun build-system-clustered-graph (system-name clustered-graphs)
  (multiple-value-bind
      (graph nodes)
      (make-system-graph system-name clustered-graphs)
    (split-in-clusters
     graph nodes
     (lambda (component)
       (cluster-dependency-component component system-name nodes
                                     (lambda (node successor-names)
                                       (declare (ignore node successor-names))
                                       +other-systems-cluster-name+))))))

(defun build-root-graph (system-items)
  (multiple-value-bind
      (graph nodes)
      (make-root-graph system-items)
    (split-in-clusters
     graph nodes
     (lambda (component)
       (cluster-dependency-component component +my-systems-cluster-name+ nodes
                                     (lambda (node successor-names)
                                       (declare (ignore node successor-names))
                                       +other-systems-cluster-name+))))))