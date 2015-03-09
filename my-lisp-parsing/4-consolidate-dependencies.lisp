(in-package :my-lisp-parsing)

(defun wrap-as-keywords (lst)
  (apply #'as-keyword (loop for item in lst
                            for i from 0
                            when (< 0 i)
                            collect #\Space
                            collect item)))

(defun parse-action-node-class (class-name)
  (loop for tag in class-name
        with usage-tag
        when (member tag '(:ok :error :warning))
        do (setf usage-tag tag)
        else
        collect tag into tags
        finally (return (values tags usage-tag))))

(defun compute-nodes-statistics (nodes)
  (let (result)
    (dolist (class-name (mapcar #'node-class nodes))
      (multiple-value-bind
          (tags usage-tag)
          (parse-action-node-class class-name)
        (let ((stat-class (wrap-as-keywords tags)))
          (setf (getf result stat-class)
                (totalize-stats (getf result stat-class)
                                (append (list :total 1)
                                        (aif usage-tag (list it 1))))))))
    result))

(defun list-external-symbols (package)
  (let (result)
    (do-external-symbols (s package)
      (push s result))
    result))

(defun make-action-tagger ()
  "By default, no class are associated to an action. At first any action is considered to be unexported and used only in its own module. Then some classes can be added and even combined but only if they do not share the same topic.
Relevance topic, one of:
- narrow: the action is relevant to its system, outside of its birth module
- wide: the action is globally relevant
Intended relevance topic (one class only):
- exported: the action is meant to be globally relevant by being exported from its package
Use topic:
- unused: the action is unused, either by being truly not used at all or by being part of a chain of unused actions.
- to-be-used: a special case of unused action which must not be exported and must be part of a chain of unused actions with at least an exported unused action ahead of it. This helps to distinguish truly internal unused chains from whose which are unused yet with some parts meant to be globally relevant. As such, these chains might not stay useless for long.
- end: the action is not used at all in any other module or system but it is used externally and its result are provided to a person (such as being exposed through a server). This function does not tag an action with :end, it has to be already provided up ahead."
  (let (unused-actions unused-api)
    (dlambda
      (:tag (node cluster clustered-graph)
       (declare (ignore cluster))
       (let ((nodes (graph-nodes clustered-graph))
             (system-name (symbol-system-name (node-name node)))
             (is-exported? (and (not (keywordp (node-name node)))
                                (member (node-name node) (list-external-symbols (symbol-package (node-name node))))))
             (use-list (node-info node))
             (use (unless (member :end (node-class node)) :unused))
             relevance
             class)
         (when is-exported?
           (push :exported class))
         (dolist (symbol use-list)
           (cond
            ((member symbol unused-api)
             (when use
               (setf use :to-be-used)))
            ((not (member symbol unused-actions))
             (setf use nil)))
           (if (or (keywordp symbol)
                   (string= system-name (symbol-system-name symbol)))
             (unless (and (eq relevance :wide)
                          (member symbol nodes :key #'node-name))
               (setf relevance :narrow))
             (setf relevance :wide)))
         (unless (null use)
           (if (or is-exported?
                   (eq :to-be-used use))
             (push (node-name node) unused-api)
             (push (node-name node) unused-actions)))
         (awhen relevance (push it class))
         (awhen use (push it class))
         class))
      (:unused-actions () unused-actions)
      (:unused-api () unused-api))))

(defun graph-change-links-to-class (start-node-name class-name update-fn graph)
  "update-fn is a function returning true for each successor that needs to be in the new link, tagged with the given class-name."
  (let (new-class-link new-successors new-class-link-position delete-positions)
    (multiple-value-bind
        (new-links positions)
        (loop for link in (graph-links graph)
              for i from 0
              if (equal start-node-name (link-start-node-name link))
              collect i into positions
              and collect link into new-links
              and do (when (equal class-name (link-class link))
                       (setf new-class-link link
                             new-class-link-position i))
              finally (return (values new-links positions)))
      (unless new-class-link
        (setf new-class-link (make-link start-node-name nil class-name)))
      (loop for link in new-links
            for position in positions
            do (let (successors)
                 (dolist (successor (link-successors link))
                   (if (funcall update-fn successor)
                     (push successor new-successors)
                     (push successor successors)))
                 (if successors
                   (setf (link-successors link) successors
                         (elt (graph-links graph) position) link)
                   (push position delete-positions))))
      (dolist (successor new-successors)
        (link-add-successor new-class-link successor))
      (when (and new-class-link-position
                 (link-successors new-class-link))
        (setf (elt (graph-links graph) new-class-link-position) new-class-link))
      (when delete-positions
        (setf (graph-links graph)
              (loop for link in (graph-links graph)
                    for i from 0
                    unless (member i delete-positions)
                    collect link)))
      (when (and (not new-class-link-position)
                 (link-successors new-class-link))
        (push new-class-link (graph-links graph))))
    new-successors))

(defun make-action-usages-tagger ()
  (let (ko-actions)
    (dlambda
      (:tag (node cluster clustered-graph)
       (let (class
             (module-name (graph-name clustered-graph)))
         (when (fboundp (node-name node))
           (awhen (getf (get-from-hierarchical-cache :usages
                                                     (symbol-system-name (node-name node))
                                                     module-name
                                                     (node-name node))
                        :stats)
             (let (ko-class)
               (cond
                ((/= 0 (getf it :error))
                 (setf ko-class :error))
                ((or (/= 0 (getf it :warning))
                     (graph-change-links-to-class (node-name node) (list :error)
                                                  (lambda (successor)
                                                    (member (link-successor-name successor) ko-actions :test #'equal))
                                                  cluster))
                 (setf ko-class :warning)))
               (if ko-class
                 (progn
                   (push ko-class class)
                   (push (node-name node) ko-actions))
                 (when (/= 0 (getf it :ok))
                   (push :ok class))))))
         class))
      (:ko-actions () ko-actions))))

(defun make-module-tagger ()
  "A module inherits some classes from the actions it contains, except default. Here's how:
- narrow: the default for a module because it is naturally supposed to make sense at the level of the system, meaning it is used by other modules. In such case, it contains only default and/or unexported narrow actions, at least one of them.
- wide: the module is globally relevant. It contains at least one wide-relevant action.
- exported: the module is meant to be globally relevant by containing at least one exported action.
- unused: all actions are unused.
- to-be-used: all actions are unused, none is exported and at least one is to-be-used. As soon as there is an exported action, even if unused, the classes for the module will be 'unused exported', since it is stronger to have an exported action, even if unused, than an unused internal action on which an unused exported action depends.
- end: there is at least one end action and only end, unused or narrow actions."
  (dlambda
    (:tag (node cluster clustered-graph)
     (declare (ignore cluster))
     (let ((module-name (node-name node))
           (system-name (graph-name clustered-graph))
           (total 0)
           (allowed-classes '(:narrow :wide :exported :unused :to-be-used :end))
           result 
           class)
       (mapcar (lambda (class)
                 (setf (getf result class) 0))
               allowed-classes)
       (loop for node in (graph-nodes (get-from-hierarchical-cache :dependencies system-name module-name))
             do (incf total)
             do (dolist (class (node-class node))
                  (when (member class allowed-classes)
                    (incf (getf result class)))))
       (when (< 0 (getf result :exported))
         (push :exported class))
       (cond
        ((< 0 (getf result :wide))
         (push :wide class))
        ((or (< 0 (getf result :narrow))
             (< (getf result :wide) total))
         (push :narrow class)))
       (when (= total (getf result :unused))
         (if (and (< 0 (getf result :to-be-used))
                  (= 0 (getf result :exported)))
           (push :to-be-used class)
           (push :unused class)))
       (when (and
              (< 0 (getf result :end))
              (= 0
                 (getf result :wide)
                 (getf result :exported)
                 (getf result :to-be-used)))
         (setf class (list :end)))
       class))))
       
       
(defun make-module-usages-tagger ()
  "A module has the same usage classes as an action. Here's how it is generalized, ordered by highest priority first:
- error: as soon as one action is in error
- warning: as soon as one action is in warning but none is in error
- ok: when all actions are tested and ok
- none: no problems but some usages are missing"
  (dlambda
    (:tag (node cluster clustered-graph)
     (declare (ignore cluster))
     (let* ((module-name (node-name node))
            (system-name (graph-name clustered-graph))
            (ok-count 0)
            (total 0)
            (clustered-graph (get-from-hierarchical-cache :dependencies system-name module-name))
            is-ko is-on-ko)
       (loop for node in (graph-nodes clustered-graph)
             while (not is-ko)
             do (incf total)
             do (dolist (class-name (node-class node))
                  (case class-name
                    (:error (setf is-ko t))
                    (:warning (setf is-on-ko t))
                    (:ok (incf ok-count)))))
       (let ((class (cond
                     (is-ko
                      (list :error))
                     (is-on-ko
                      (list :warning))
                     ((and (< 0 ok-count)
                           (= ok-count total))
                      (list :ok))
                     (t
                      nil))))
         class)))))

(defun make-system-tagger ()
  "A system inherits some classes from the modules it contains by the same process a modules inherits them from its actions. Here's how:
- narrow: the default for a system. It only makes sense for itself. As such, it contains only unexported narrow modules.
- wide: the system is globally relevant. It contains at least one wide-relevant module.
- exported: the system is meant to be globally relevant by containing at least one exported action.
- unused: all modules are unused.
- to-be-used: all modules are unused, none is exported and at least one is to-be-used.
- end: there is at least one end module and only end, unused or narrow modules."
  (dlambda
    (:tag (node cluster clustered-graph)
     (declare (ignore cluster clustered-graph))
     (let ((system-name (node-name node))
           (total 0)
           (allowed-classes '(:narrow :wide :exported :unused :to-be-used :end))
           result 
           class)
       (mapcar (lambda (class)
                 (setf (getf result class) 0))
               allowed-classes)
       (loop for node in (graph-nodes (get-from-hierarchical-cache :dependencies system-name :graph))
             do (incf total)
             do (dolist (class (node-class node))
                  (when (member class allowed-classes)
                    (incf (getf result class)))))
       (when (< 0 (getf result :exported))
         (push :exported class))
       (cond
        ((< 0 (getf result :wide))
         (push :wide class))
        ((or (< 0 (getf result :narrow))
             (< (getf result :wide) total))
         (push :narrow class)))
       (when (= (getf result :unused)
                total)
         (if (and (< 0 (getf result :to-be-used))
                  (= 0 (getf result :exported)))
           (push :to-be-used class)
           (push :unused class)))
       (when (and
              (< 0 (getf result :end))
              (= 0
                 (getf result :wide)
                 (getf result :exported)
                 (getf result :to-be-used)))
         (setf class (list :end)))
       class))))

(defun make-system-usages-tagger ()
  "A system has the same usage classes as an action or a module. It follows the same rules as for a module in fact. Here's what they are, ordered by highest priority first:
- ko: as soon as one action is ko
- on-ko: as soon as one action is on-ko
- ok: when all actions are tested and ok
- none: no problems but some usages are missing, maybe all"
  (dlambda
    (:tag (node cluster clustered-graph)
     (declare (ignore cluster clustered-graph))
     (let ((ok-count 0)
           (total 0)
           (system-graph (get-from-hierarchical-cache :dependencies (node-name node) :graph))
           is-ko is-on-ko)
       (loop for node in (graph-nodes system-graph)
             while (not is-ko)
             do (incf total)
             do (dolist (class-name (node-class node))
                  (case class-name
                    (:error (setf is-ko t))
                    (:warning (setf is-on-ko t))
                    (:ok (incf ok-count)))))
       (let ((class (cond
                     (is-ko
                      (list :error))
                     (is-on-ko
                      (list :warning))
                     ((and (< 0 ok-count)
                           (= ok-count total))
                      (list :ok))
                     (t
                      nil))))
         class)))))

(defun consolidate-dependencies (clustered-graph tagger &key reverse)
  (dolist (node (if reverse
                  (reverse (graph-nodes clustered-graph))
                  (graph-nodes clustered-graph)))
    (let (class-names found)
      (loop for single-node in (clustered-graph-singles-nodes clustered-graph)
            for i from 0
            while (not found)
            do (when (node= node single-node)
                 (setf class-names (append (node-class node)
                                           (funcall tagger :tag node nil clustered-graph))
                       (node-class (elt (clustered-graph-singles-nodes clustered-graph) i)) class-names
                       found t)))
      (unless found
        (loop for component in (clustered-graph-components clustered-graph)
              for i from 0
              while (not found)
              do (let ((cluster (clustered-graph-component-cluster (graph-name clustered-graph) component)))
                   (awhen (graph-node-position node cluster)
                     (setf class-names (append (node-class node)
                                           (funcall tagger :tag node cluster clustered-graph))
                           (node-class (elt (graph-nodes cluster) it)) class-names
                           found t)))))
      (setf (node-class node) class-names))))

(defun list-systems-in-topological-order ()
  (let ((system-names (system-list-all-loaded)))
    (flet ((get-system-dependencies (system-name)
             (remove-if-not (lambda (x)
                              (member x system-names :test #'string=))
                            (mapcar (lambda (x)
                                      (string-downcase (symbol-name x)))
                                    (rest (find 'asdf:load-op
                                                (asdf:component-depends-on (make-instance 'asdf:load-op)
                                                                           (asdf:find-system system-name))
                                                :key #'first))))))
      (let-a (loop for system-name in system-names
                   collect (make-link system-name (get-system-dependencies system-name)))
        (let-a (make-graph (mapcar (lambda (x) (make-node (first x))) it)
                           it)
          (mapcar #'node-name (nreverse (graph-topological-sort it))))))))

(defun consolidate-dependencies-classes ()
  "Consolidate dependencies classes.
This process needs to run backward from the top, final modules of the top, final systems because of the unused and api-unused tags. By definition, they denote an action which is, respectively, either never used or only used by unused actions (the unused tag) or used by only unused actions, one of which at least being api-unused or part of the API (the api-unused tag). In both cases, actions which are using the considered action must be tagged appropriately beforehand, hence the backward flow."
  (let ((action-tagger (make-action-tagger))
        (module-tagger (make-module-tagger)))
    (dolist (system-name (nreverse (list-systems-in-topological-order)))
      (dolist (module-name (nreverse (system-module-names system-name)))
        (consolidate-dependencies (get-from-hierarchical-cache :dependencies system-name module-name)
                                  action-tagger
                                  :reverse t))
      (consolidate-dependencies (get-from-hierarchical-cache :dependencies system-name :graph)
                                module-tagger
                                :reverse t))))

(defun consolidate-usages-classes ()
  "Consolidate usages classes.
This process needs to be run forward from the bottom, initial modules of the bottom, initial systems because of the warning tag. By definition, it denotes an action which rely on some actions in error or in warning while not being in error itself. Such actions it relies on must therefore be appropriately tagged before tagging the former action."
  (let ((action-usages-tagger (make-action-usages-tagger))
        (module-usages-tagger (make-module-usages-tagger)))
    (dolist (system-name (list-systems-in-topological-order))
      (dolist (module-name (system-module-names system-name))
        (consolidate-dependencies (get-from-hierarchical-cache :dependencies system-name module-name)
                                  action-usages-tagger))
      (consolidate-dependencies (get-from-hierarchical-cache :dependencies system-name :graph)
                                module-usages-tagger))))

(defun consolidate-root-graph (root-graph)
  (let ((system-tagger (make-system-tagger))
        (system-usages-tagger (make-system-usages-tagger)))
    (consolidate-dependencies root-graph system-tagger)
    (consolidate-dependencies root-graph system-usages-tagger)
    root-graph))

(defun consolidate-clustered-graph-statistics (clustered-graph)
  "Per category of nodes, count the total number of them, the total number of ok and error"
  (let ((singles-statistics (compute-nodes-statistics (clustered-graph-singles-nodes clustered-graph)))
        (component-statistics (loop for component in (clustered-graph-components clustered-graph)
                                    for index from 0
                                    collect (setf (clustered-graph-component-stats
                                                   (elt (clustered-graph-components clustered-graph) index))
                                                  (compute-nodes-statistics
                                                    (graph-nodes
                                                     (find (graph-name clustered-graph)
                                                           (clustered-graph-component-clusters component)
                                                           :test #'equal :key #'graph-name)))))))
    (setf (clustered-graph-singles-stats clustered-graph)
          singles-statistics
          (getf clustered-graph :stats)
          (reduce #'totalize-sub-stats
                  (append (list singles-statistics)
                          component-statistics)))
    clustered-graph))

(defun consolidate-statistics ()
  (loop for system-item in (get-from-hierarchical-cache :dependencies)
        for index from 0
        do (loop for clustered-graph in (getf system-item :in)
                 for index from 0
                 do (setf (elt (getf system-item :in) index)
                          (consolidate-clustered-graph-statistics clustered-graph)))
        do (setf (getf (getf system-item :graph) :stats)
                 (totalize-encompassed-stats (getf system-item :in) #'totalize-sub-stats)
                 (elt (get-from-hierarchical-cache 'dependencies) index)
                 system-item)))