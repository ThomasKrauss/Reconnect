(in-package :my-lisp-parsing)

(defun print-dependency-node (node &optional (system-name "CL"))
  (list
   (if (symbolp (node-name node))
     (print-code-chunk (node-name node) system-name)
     (node-name node))
   (dot-plain-string (node-class node))))

(defun get-strict-dependency-graph-positions (graph system-name component-statistics-fn)
  (let* ((strict-clustered-graph (derive-strict-clustered-graph graph))
         (graph-positions (get-strict-clustered-graph-positions strict-clustered-graph
                                                                :node-class-map *dot-node-class-map*
                                                                :edge-class-map *dot-edge-class-map*)))
    (with-system (system-name)
      (list :graphs (loop for positions in (getf graph-positions :graphs)
                          for component in (clustered-graph-components strict-clustered-graph)
                          collect (progn
                                    (setf (getf positions :stats)
                                          (funcall component-statistics-fn
                                                   (graph-nodes (first (clustered-graph-component-clusters component)))))
                                    positions))
            :singles (list :nodes (mapcar (lambda (node)
                                            (print-dependency-node node system-name))
                                          (clustered-graph-singles-nodes strict-clustered-graph))
                           :stats (compute-nodes-statistics (clustered-graph-singles-nodes strict-clustered-graph)))))))

(defun get-strict-module-graph-positions (module-graph &rest names)
  (get-strict-dependency-graph-positions module-graph (first names) #'compute-nodes-statistics))

(defun get-strict-system-graph-positions (system-item &rest names)
  (let ((system-name (first names)))
    (get-strict-dependency-graph-positions (getf system-item :graph)
                                           system-name
                                           (lambda (nodes)
                                             (totalize-encompassed-stats
                                              (mapcar (lambda (node)
                                                        (getf+ system-item :in (node-name node)))
                                                      nodes)
                                              #'totalize-sub-stats)))))

(defun get-strict-root-graph-positions (root-graph)
  (get-strict-dependency-graph-positions root-graph
                                         "CL"
                                         (lambda (nodes)
                                           (totalize-encompassed-stats
                                            (mapcar (lambda (node)
                                                      (get-from-hierarchical-cache :dependencies
                                                                                     (node-name node)
                                                                                     :graph))
                                                    nodes)
                                            #'totalize-sub-stats))))

(defun print-system-and-module-list (system-items)
  (nreverse
   (loop for system-item in system-items
         collect (list (getf system-item :name)
                       (nreverse
                        (mapcar (lambda (module-item)
                                  (getf module-item :name))
                                (getf system-item :in)))))))

(defun get-system-and-module-list ()
  (print-system-and-module-list (get-from-hierarchical-cache :dependencies)))

(defun print-root-graph (system-items)
  (get-strict-root-graph-positions
   (consolidate-root-graph
    (build-root-graph system-items))))

(defun get-used-systems (system-name)
  (flatten
   (mapcar (lambda (component)
             (graph-nodes (find +other-systems-cluster-name+
                                (clustered-graph-component-clusters component)
                                :test #'string=
                                :key (lambda (cluster)
                                       (getf cluster :name)))))
           (clustered-graph-components (getf (get-from-hierarchical-cache :dependencies system-name) :graph)))))

(defun get-dependent-systems (system-name)
  (loop for other-system-name in (rest (member system-name (list-systems-in-topological-order) :test #'string=))
        when (member system-name (get-used-systems other-system-name) :test #'string=)
        collect other-system-name))

(defun print-module-use-lists (system-item &rest names)
  (declare (ignore names))
  (loop for module-graph in (getf system-item :in)
        collect (list :name (graph-name module-graph)
                      :in (reduce #'append
                                  (mapcar (lambda (item)
                                            (getf item :in))
                                          (print-action-use-lists module-graph))))))

(defun print-action-use-lists (module-graph &rest names)
  (declare (ignore names))
  (loop for node in (graph-nodes module-graph)
        collect (list :name (node-name node)
                      :in (let (result)
                            (dolist (action-name (node-info node))
                              (setf (getf+ result (symbol-system-name action-name) :in)
                                    (append (getf+ result (symbol-system-name action-name) :in)
                                            (list (format nil "~(~a~)" action-name)))))
                            result))))

(define-hierarchical-cache :dependencies (((merge-pathnames "dependencies/"
                                                           (cache-directory "my-lisp-parsing"))
                                          (merge-pathnames "use/"
                                                           (cache-directory "my-lisp-parsing"))
                                          (cache-directory "my-lisp-parsing")
                                          (cache-directory "my-lisp-parsing"))
                                         :root (nil
                                                nil
                                                ("dependencies" print-root-graph)
                                                ("systems-and-modules-list" print-system-and-module-list))
                                         :system (get-strict-system-graph-positions print-module-use-lists)
                                         :module (get-strict-module-graph-positions print-action-use-lists))
  :module (build-module-clustered-graph module-name system-name
                                        (get-from-hierarchical-cache :dependencies :in))
  :system (list :graph (build-system-clustered-graph system-name
                                                     (get-from-hierarchical-cache :dependencies system-name :in)))
  :root (list-systems-in-topological-order))

(defun get-using-actions (action-name)
  (let (found
        (system-name (symbol-system-name action-name)))
    (loop for module-graph in (get-from-hierarchical-cache :dependencies system-name :in)
          while (not found)
          do (awhen (find action-name (graph-nodes module-graph) :test #'equal :key #'node-name)
               (setf found it)))
    (awhen found
      (values (node-info it) it))))

(defun refresh-dependencies-cache ()
  (let ((events (with-hierarchical-cache (:dependencies)
                  (refresh)
                  (get-events))))
    (consolidate-dependencies-classes)
    (consolidate-usages-classes)
    (consolidate-statistics)
    (write-hierarchical-cache :dependencies events)))

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

;(defun list-actions-in-topological-order (module-name system-name)

; Actions
(defun get-defined-actions (filename)
  "Get all defined actions in the given file.
Currently only detects defun and defmacro forms."
  (let (result)
    (with-open-file (s filename :direction :input)
      (loop for expr = (read s nil nil)
            while expr
            do (case (first expr)
                 ((defun defmacro) (push (second expr) result)))))
    (nreverse result)))