(in-package :my-graphs)

(my-systems:defun-to-external-program dot ("Graphviz Dot" "dot" "-V")
  (file-or-string &key (layout "dot") (output-type "plain") output-to-file)
  (let* ((tmp-file (merge-pathnames "dot.tmp" cl-user::*basedir*))
         (input-file (cond
                      ((pathnamep file-or-string)
                       (if (and (not (directory-pathname-p file-or-string))
                                (file-exists-p file-or-string))
                           file-or-string
                         (error "Dot called with path to ~a: it must exist and be a file!" file-or-string)))
                      ((< 2000 (length file-or-string))
                       (with-open-file (s tmp-file
                                          :direction :output
                                          :if-exists :supersede)
                         (write-string file-or-string s))
                       tmp-file))))
    (unwind-protect (with-output-to-string (s)
                      (call-command (join-str (append (unless input-file
                                                        (list "echo" (my-systems:cmd-escape file-or-string :escape-levels 2) "|"))
                                                      (list "dot"
                                                            "-K" layout
                                                            "-T" output-type
                                                            "-y"
                                                            "-q")
                                                      (when output-to-file
                                                        (if (pathnamep output-to-file)
                                                            (list "-o" (format nil "~a" output-to-file))
                                                          (list "-O")))
                                                      (when input-file
                                                        (list (format nil "~a" input-file))))
                                              #\Space)
                                    :output-stream s
                                    :guarded t))
      (delete-file tmp-file nil))))

(defun node<-maybe-node (maybe-node)
  (if (listp maybe-node)
    maybe-node
    (make-node maybe-node)))

(defun dot-string (item)
  "=> string
A string for Dot is some content in double quotes.
The given item will thus be formated aesthetically enclosed by double quotes.
If the given item is a list, each item will be formatted aesthetically and separated from one another by a space. The whole will be enclosed in double quotes.
Additionnally, everything will be printed to lowercase but that's an artifact reflecting my narrow use of graphs. I will remove such a constraint because it has nothing to do with Dot."
  (if (atom item)
    (format nil "\"~(~a~)\"" item)
    (format nil "\"~(~{~a~^ ~}~)\"" item)))

(defun dot-string% (item)
  "=> string
A string for Dot is some content in double quotes.
The given item will thus be formated aesthetically enclosed by double quotes, unless it is already a dot-string.
If the given item is a list, each item will be formatted aesthetically and separated from one another by a space. The whole will be enclosed in double quotes."
  (if (atom item)
    (if (and (stringp item)
             (char= #\" (elt item 0) (elt item (1- (length item)))))
      item
      (format nil "\"~a\"" item))
    (format nil "\"~{~a~^ ~}\"" item)))

(defun dot-plain-string (item)
  "=> string
A dot plain string is the content of a string for Dot, not enclosed by double quotes.
Otherwise, it just follows the same rules."
  (if (atom item)
    (format nil "~(~a~)" item)
    (format nil "~(~{~a~^ ~}~)" item)))

(defun dot-plain-string% (item)
  "=> string
A dot plain string is the content of a string for Dot, not enclosed by double quotes.
Otherwise, it just follows the same rules."
  (if (atom item)
    (format nil "~a" item)
    (format nil "~{~a~^ ~}" item)))

(defun dot-node-identifier (node)
  "=> string
The identifier of a node is a string allowing Dot to uniquely identified a given node, even when it has the same label as other ones.
Basically, it is the string for Dot of the name of the node except when the name is a symbol.
In such a case, the symbol name is printed prefixed by its package name."
  (let ((name (node-name (node<-maybe-node node))))
    (cond
     ((symbolp name)
      (format nil "~a:~a" (package-name (symbol-package name)) name))
     (t
      name))))

(defun dot-node-identifier% (node+)
  "=> string
The identifier of a node is a string allowing Dot to uniquely identified a given node, even when it has the same label as other ones.
Basically, it is the string for Dot of the name of the node except when the name is a symbol.
In such a case, the symbol name is printed prefixed by its package name."
  (let ((name (name<-node+ node+)))
    (dot-string%
     (cond
      ((symbolp name)
       (format nil "~a:~a" (package-name (symbol-package name)) name))
      (t
       name)))))

(defun to-dot-class (class-names class-map)
  (flet ((inner (class-name)
           (if class-map
             (first (find class-name class-map :key #'second :test #'equal))
             class-name)))
    (if (atom class-names)
      (inner class-names)
      (loop for dot-class-name in (mapcar #'inner class-names)
            unless (null dot-class-name)
            collect dot-class-name))))

(defun from-dot-class (dot-class-names class-map)
  (flet ((inner (dot-class-name)
           (if class-map
             (second (find dot-class-name class-map :key #'first))
             dot-class-name)))
    (if (atom dot-class-names)
      (inner dot-class-names)
      (loop for class-name in (mapcar #'inner dot-class-names)
            unless (null class-name)
            collect class-name))))

(defparameter *max-label-length* 100
  "The maximum length of the name of a node before it is truncated.")

(defun dot-label (node)
  "The label of node is the string for Dot of its name, truncated at the end when its size is over *max-label-length*"
  (let ((string (dot-string (node-name (node<-maybe-node node)))))
    (if (> (length string) *max-label-length*)
        (concatenate 'string
                     (subseq string 0 (- *max-label-length* 3))
                     "...")
        string)))

(defun dot-label% (node+)
  "The label of node is the string for Dot of its name, truncated at the end when its size is over *max-label-length*"
  (let ((string (dot-string% (name<-node+ node+))))
    (if (> (length string) *max-label-length*)
        (concatenate 'string
                     (subseq string 0 (- *max-label-length* 3))
                     "...")
        string)))

(defun dot<-nodes (nodes &optional node-class-map)
  "Output to the *standard-output* the list of the given nodes in Dot format."
  (dolist (node nodes)
    (fresh-line)
    (format t "\"~a\" [label=~a~:[~;, style=~:*~a~]];"
            (dot-node-identifier node)
            (dot-label node)
            (awhen (to-dot-class (node-class node) node-class-map)
              (dot-string it)))))

(defun dot<-nodes% (nodes+ &optional node-class-map)
  "Output to the *standard-output* the list of the given nodes in Dot format."
  (dolist (node+ nodes+)
    (fresh-line)
    (format t "~a [label=~a~:[~;, style=~:*~a~]];"
            (dot-node-identifier% node+)
            (dot-label% node+)
            (awhen (and (node? node+)
                        (to-dot-class (node-class node+) node-class-map))
              (dot-string% it)))))

(defun separate-weight-in-class (class-names)
  (aif (position :weight class-names)
       (let ((weight (elt class-names (1+ it)))
             (class-names (loop for class-name in class-names
                                for i from 0
                                unless (or (= i it) (= i (1+ it)))
                                collect class-name)))
         (values class-names weight))
       (values class-names 1)))

(defun dot<-links (links directed edge-class-map)
  "Output to the *standard-output* the list of the given edges in the Dot format required regarding if it is about a directed graph or not."
  (let ((format-template
         (concatenate 'string
                      "\"~a\" "
                      (if directed "->" "--")
                      " \"~a\" [~{~a = ~a~^, ~}];")))
    (dolist (link links)
      (dolist (successor (link-successors link))
        (fresh-line)
        (format t format-template
                (dot-node-identifier (link-start-node-name link))
                (dot-node-identifier (make-node (link-successor-name successor)))
                (append (awhen (link-successor-label successor)
                          (list "label" it))
                        (multiple-value-bind
                            (class-names weight)
                            (separate-weight-in-class (link-class link))
                          (append (list "weight" weight)
                                  (awhen (to-dot-class class-names edge-class-map)
                                    (list "style" (dot-string it)))))))))))

(defun parse-edge-class (class-names)
  "=> (dot-styles dot-attributes)"
  (let (dot-styles
        (dot-attributes (list :weight 1))
        dot-attribute-name)
    (dolist (item class-names)
      (cond
       (dot-attribute-name
        (setf (getf dot-attributes dot-attribute-name) item
              dot-attribute-name nil))
       ((and (keywordp item)
             (member item '(:weight :ltail :lhead)))
        (setf dot-attribute-name item))
       ((and (keywordp item)
             (member item '(:invis)))
        (push (string-downcase (symbol-name item)) dot-styles))
       (t
        (push item dot-styles))))
    (values (nreverse dot-styles)
            (loop for item in dot-attributes
                  for index from 1
                  when (oddp index)
                  collect (string-downcase (symbol-name item))
                  else
                  collect item))))

(defun dot<-edges (edges directed edge-class-map)
  (let ((format-template
         (concatenate 'string
                      "~a "
                      (if directed "->" "--")
                      " ~a [~{~a = ~a~^, ~}];")))
    (dolist (edge edges)
      (fresh-line)
      (format t format-template
              (dot-node-identifier% (edge-head-node-name edge))
              (dot-node-identifier% (edge-tail-node-name edge))
              (multiple-value-bind
                  (dot-styles dot-attributes)
                  (parse-edge-class (edge-class edge))
                (append dot-attributes
                        (awhen (to-dot-class dot-styles edge-class-map)
                          (list "style" (dot-string% it)))))))))

(defmacro as-dot-call (fn var)
  `(,fn ,var
        :node-font-size node-font-size
        :edge-font-size edge-font-size
        :font-name font-name
        :node-class-map node-class-map
        :edge-class-map edge-class-map))

(defun dot<-graph (graph &key (node-font-size 12) (edge-font-size 12) (font-name "Anonymous Pro Bold")
                         node-class-map edge-class-map)
  "Output to the *standard-output* the specification in Dot format of the given graph.
The size of the font and the font itself have a direct impact on the position of the various nodes and edges.
Thus, they must be controlled if you are interested in the geometric information Dot can provide in output plain.
As defaults:
- font is enforced to be Anonymous Pro Bold because it is monospaced and switching to the non-bold version will work just fine if the graph was initially drawn with the bold version.
- font size is enforced to 12pt.
I have actually verified on Firefox that this configuration will have the same rendering as setting the font-size to 1em or 16px (see http://kyleschaeffer.com/development/css-font-size-em-vs-px-vs-pt-vs/).
Since the target of all my projects is viewing things with web browsers, this configuration is the easiest to depart from."
  (princ (if (graph-directed? graph)
               "digraph{"
               "graph{"))
      (format t "node [fontsize = ~a, fontname = ~s];" node-font-size font-name)
      (format t "edge [fontsize = ~a, fontname = ~s, arrowhead = none];" edge-font-size font-name)
      (dot<-nodes (graph-nodes graph) node-class-map)
      (dot<-links (graph-links graph) (graph-directed? graph) edge-class-map)
      (princ "}"))

(defun dot<-graph% (graph+ &key (node-font-size 12) (edge-font-size 12) (font-name "Anonymous Pro Bold")
                           node-class-map edge-class-map)
  "Output to the *standard-output* the specification in Dot format of the given graph.
The size of the font and the font itself have a direct impact on the position of the various nodes and edges.
Thus, they must be controlled if you are interested in the geometric information Dot can provide in output plain.
As defaults:
- font is enforced to be Anonymous Pro Bold because it is monospaced and switching to the non-bold version will work just fine if the graph was initially drawn with the bold version.
- font size is enforced to 12pt.
I have actually verified on Firefox that this configuration will have the same rendering as setting the font-size to 1em or 16px (see http://kyleschaeffer.com/development/css-font-size-em-vs-px-vs-pt-vs/).
Since the target of all my projects is viewing things with web browsers, this configuration is the easiest to depart from."
  (cond
   ((or (raw-graph? graph+)
        (graph? graph+))
    (if (graph-lenient? graph+)
      (as-dot-call dot<-lenient-graph graph+)
      (as-dot-call dot<-strict-graph graph+)))
   ((raw-graph-cluster? graph+)
    (as-dot-call dot<-clusters (graph-cluster-raw-graphs graph+)))
   ((graph-cluster? graph+)
    (as-dot-call dot<-clusters (graph-cluster-graphs graph+)))
   ((graph-set? graph+)
    (as-dot-call dot<-clusters (graph-set-graphs graph+)))
   (t
    (error "Unknow type of graph ~a" graph+))))

(defun dot-general-style (node-font-size edge-font-size font-name)
  (format t "node [fontsize = ~a, fontname = ~s];" node-font-size font-name)
  (format t "edge [fontsize = ~a, fontname = ~s, arrowhead = dot];" edge-font-size font-name))

(defun dot<-strict-graph (graph &key (node-font-size 12) (edge-font-size 12) (font-name "Anonymous Pro Bold")
                                node-class-map edge-class-map)
  (princ (if (graph-directed? graph)
           "digraph{"
           "graph{"))
  (dot-general-style node-font-size edge-font-size font-name)
  (dot<-nodes% (graph-nodes graph) node-class-map)
  (dot<-edges (graph-edges graph) (graph-directed? graph) edge-class-map)
  (princ "}"))

(defun non-empty-name? (name)
  (and (if (stringp name)
         (string/= name "")
         t)
       name))

(defun dot<-lenient-graph (graph &key (node-font-size 12) (edge-font-size 12) (font-name "Anonymous Pro Bold")
                                 node-class-map edge-class-map)
  (let ((out-nodes (mapcar (lambda (node-name)
                             (make-node node-name))
                           (remove-duplicates
                            (loop for link in (graph-links graph)
                                  append (loop for node-name in (cons (link-head-node-name link)
                                                                      (link-successor-nodes-names link))
                                               unless (find-node node-name graph)
                                               collect node-name))
                            :test #'my-equal))))
    (unless (non-empty-name? (graph-name graph))
      (setf (graph-name graph) "in"))
    (as-dot-call dot<-clusters (list graph (make-graph% out-nodes :name "out" :directed (graph-directed? graph))))))

(defun dot<-clusters (graphs &key (node-font-size 12) (edge-font-size 12) (font-name "Anonymous Pro Bold")
                              node-class-map edge-class-map)
  (princ (if (graph-directed? (first graphs))
           "digraph{"
           "graph{"))
  (princ "rankdir = TB;")
  (princ "compound = true;")
  ;(princ "remincross = true;")
  ;(princ "concentrate = true;")
  (princ "labelloc=\"b\";")
  (dot-general-style node-font-size edge-font-size font-name)
  (let ((index -1)
        (recursive-index -1)
        names)
    (labels ((print-cluster-nodes (graph)
               (terpri)
               (multiple-value-bind
                   (name label)
                   (aif (non-empty-name? (graph-name graph))
                        (values it it)
                        (values (incf index) nil))
                 (format t "~%subgraph \"cluster-~a\" {" name)
                 (when label
                   (princ (concatenate 'string "label=\"" label "\";")))
                 (princ "rank = \"source\";")
                 (princ "pencolor = lightgrey;")
                 (princ "bgcolor = lightgrey;")
                 (dot<-nodes% (graph-nodes graph) node-class-map)
                 (princ "}")
                 name))
             (print-recursively-nodes (graphs)
               (when graphs
                 (format t "~%subgraph \"cluster-rec~a\" {" (incf recursive-index))
                 (push (print-cluster-nodes (first graphs)) names)
                 (print-recursively-nodes (rest graphs))
                 (princ "}")))
             (print-cluster-binding-edges (name1 graph1 name2 graph2)
               (let (edges)
                 (dolist (node1 (graph-nodes graph1))
                   (dolist (node2 (graph-nodes graph2))
                     (push (make-edge node1 node2
                                      :class (list :ltail (mkstr "\"cluster-" name1 "\"")
                                                   :lhead (mkstr "\"cluster-" name2 "\"")
                                                   :invis))
                           edges)))
                 (dot<-edges edges (graph-directed? (first graphs)) edge-class-map))))
      (let ((graphs (reverse graphs)))
        (print-recursively-nodes graphs)
        (loop for i from 1 to recursive-index
              do (print-cluster-binding-edges (elt (reverse names) (1- i))
                                              (elt graphs (1- i))
                                              i
                                              (elt graphs i)))
        (dolist (graph graphs)
          (dot<-edges (graph-edges graph) (graph-directed? graph) edge-class-map)))))
  (princ "}"))

; subgraphs
(defun dot-name (exp)
  (format nil "\"~(~a~)\"" exp))

(defun dot<-subgraphs (graphs &key (node-font-size 12) (edge-font-size 12) (font-name "Anonymous Pro Bold")
                              node-class-map edge-class-map)
  "Print as subgraphs the given graphs.
The graphs must be either all directed or all undirected but this function will not check for this. It will use the directedness of the first graph and ignore the rest.
The graphs should be named but an empty or nil name will be ignored silently. A name must be unique but this function will not check for this either."
  (princ (if (graph-directed? (first graphs))
           "digraph{"
           "graph{"))
  (princ "rankdir = TB;")
  (princ "compound = true;")
  ;(princ "remincross = true;")
  ;(princ "concentrate = true;")
  (princ "labelloc=\"b\";")
  (format t "node [fontsize = ~a, fontname = ~s];" node-font-size font-name)
  (format t "edge [fontsize = ~a, fontname = ~s, arrowhead = none];" edge-font-size font-name)
  (let ((index -1)
        (recursive-index -1)
        names)
    (labels ((get-first-node-name (graph)
               (dot-name (node-name (first (graph-nodes graph)))))
             (print-subgraph (graph)
               (terpri)
               (let* ((empty-name? (or (null (graph-name graph)) (equal "" (graph-name graph))))
                      (name (if empty-name? (incf index) (graph-name graph))))
                 (format t "~%subgraph \"cluster-~a\" {" name)
                 (unless empty-name?
                   (princ (concatenate 'string "label=\"" name "\";")))
                 (princ "rank = \"source\";")
                 (princ "pencolor = lightgrey;")
                 (princ "bgcolor = lightgrey;")
                 (dot<-nodes (graph-nodes graph) node-class-map)
                 (princ "}")
                 name))
             (as-recursive-clusters (graphs)
               (when graphs
                 (format t "~%subgraph \"cluster-rec~a\" {" (incf recursive-index))
                 (push (print-subgraph (first graphs)) names)
                 (as-recursive-clusters (rest graphs))
                 (princ "}")))
             (bind-clusters (name1 g1 name2 g2)
               (dolist (n1 (graph-nodes g1))
                 (dolist (n2 (graph-nodes g2))
                   (format t "\"~a\" -> \"~a\" [ltail=\"cluster-~a\", lhead=\"cluster-rec~a\", style=\"invis\"];~%"
                           (dot-node-identifier n1)
                           (dot-node-identifier n2)
                           name1 name2)))))
      (let ((graphs (reverse graphs)))
        (as-recursive-clusters graphs)
        (loop for i from 1 to recursive-index
              do (bind-clusters (elt (reverse names) (1- i))
                                (elt graphs (1- i))
                                i
                                (elt graphs i)))
        (dolist (graph graphs)
          (dot<-links (graph-links graph) (graph-directed? graph) edge-class-map)))))
  (princ "}"))

(defun drawing<-graph (filename graph &key (layout "dot") (output-type "png") node-class-map edge-class-map)
  (dot (with-output-to-string (*standard-output*)
         (dot<-graph graph :node-class-map node-class-map :edge-class-map edge-class-map))
       :layout layout :output-type output-type :output-to-file filename)
  filename)

(defun drawing<-graph% (filename graph &key (layout "dot") (output-type "png") node-class-map edge-class-map)
  (dot (with-output-to-string (*standard-output*)
         (dot<-graph% graph :node-class-map node-class-map :edge-class-map edge-class-map))
       :layout layout :output-type output-type :output-to-file filename)
  filename)

(defun drawing<-subgraphs (filename graphs &key (layout "dot") (output-type "png") node-class-map edge-class-map)
  (dot (with-output-to-string (*standard-output*)
         (dot<-subgraphs graphs :node-class-map node-class-map :edge-class-map edge-class-map))
       :layout layout :output-type output-type :output-to-file filename))

(defun dot-clusters (node-positions subgraphs font-size)
  "Clusters are not part of the Dot language, only certain engines take them into account.
In any case, the plain output of Dot have no information about clusters therefore they have to be computed.
Once the minimal dimensions have been determined, a padding equal to the font-size is applied except on the top where the padding is twice the font-size because the name of the cluster goes there."
  (let (clusters
        (nodes-per-subgraphs (loop for subgraph in subgraphs
                                   collect (list (graph-name subgraph)
                                                 (mapcar (lambda (node)
                                                           (dot-node-identifier node))
                                                         (graph-nodes subgraph))))))
    (macrolet ((correct-dimension (dimension compare-fn value)
                 `(when (,compare-fn (getf cluster ,dimension) ,value)
                    (setf (getf (elt clusters it) ,dimension) ,value)))
               (% (action a b)
                 `(,action (elt node-position ,a) (elt node-position ,b))))
      (dolist (node-position node-positions)
        (let ((cluster-name (first (find (node-position-id node-position) nodes-per-subgraphs
                                         :test (lambda (id ids)
                                                 (member id ids :test #'string=))
                                         :key #'second)))
               (xmin (% - 3 5))
               (xmax (% + 3 5))
               (ymin (% - 4 6))
               (ymax (% + 4 6)))
          (aif (position cluster-name clusters :test #'string= :key (lambda (cluster) (getf cluster :name)))
               (let ((cluster (elt clusters it)))
                 (correct-dimension :x > xmin)
                 (correct-dimension :y > ymin)
                 (correct-dimension :width < xmax)
                 (correct-dimension :height < ymax))
               (push (list :name cluster-name :class "" :x xmin :y ymin :width xmax :height ymax) clusters)))))
    (symbol-macrolet ((cluster (elt clusters i)))
      (dotimes (i (length clusters))
        (decf (getf cluster :x) font-size)
        (decf (getf cluster :y) font-size)
        (decf (getf cluster :width) (- (getf cluster :x) font-size))
        (decf (getf cluster :height) (- (getf cluster :y) (* 2 font-size)))
        (setf cluster (plist-values cluster))))
    clusters))

(defun dot-parser (str &key with-edge-label node-class-map edge-class-map font-size subgraphs)
  "A point is (nearly) 72 times an inch and 12 points correspond to 16 pixels.
The target width is 1000px and things are thus scaled to fit."
  (let (graph-spec nodes edges labels
                   (dot-data (mapcar (lambda (x) (mapcar (lambda (y) (string-trim (list #\") y)) x))
                                     (mapcar (lambda (x) (split "\\s" x))
                                             (split "\\n" str))))
                   (target-width 1000)
                   pixel-width)
    (labels ((inch-to-pixel (nb-as-string)
               (/ (* 72 16 (read-from-string nb-as-string))
                  12))
             (scale (nb-as-string)
               (if pixel-width
                 (/ (* (if (stringp nb-as-string)
                         (inch-to-pixel nb-as-string)
                         nb-as-string)
                       target-width)
                    pixel-width)))
             (format-dot-class (dot-class-names dot-class-map)
               (dot-plain-string (from-dot-class
                                  (mapcar #'as-keyword dot-class-names)
                                  dot-class-map))))
      (dolist (val (butlast dot-data))
        (when val
          (cond
           ((string= (first val) "graph")
            (setf graph-spec
                  (mapcar #'inch-to-pixel
                          (subseq val 1)))
            (setf pixel-width (second graph-spec)))
           ((string= (first val) "node")
            (push (append (list (elt val 1)
                                (elt val 6)
                                (format-dot-class (subseq val 7) node-class-map))
                          (manual-list-transform (mapcar #'scale
                                                         (subseq val 2 6))
                            _ _ (/ _ 2) (/ _ 2)))
                  nodes))
           ((string= (first val) "edge")
            (let* ((spline-points-nb (parse-integer (elt val 3)))
                   (read-position (+ 4 (* 2 spline-points-nb)))
                   (edge-styles (subseq val (+ read-position (if with-edge-label 3 0)))))
              (unless (member "invis" edge-styles :test #'string=)
                (let ((spline-points (mapcar #'scale (subseq val 4 read-position)))
                      (label-position (when with-edge-label
                                        (mapcar #'scale
                                                (subseq val read-position (+ 3 read-position)))))
                      (point1 (elt val 1))
                      (point2 (elt val 2)))
                  (push (cons (list point1 point2)
                              (cons (format-dot-class edge-styles edge-class-map)
                                    spline-points))
                        edges)
                  (when with-edge-label
                    (push (cons (list point1 point2)
                                label-position)
                          labels)))))))))
      (let* ((font-size (scale font-size))
             (clusters (when subgraphs (dot-clusters nodes subgraphs font-size))))
        (as-plist :width target-width :height (scale (third graph-spec))
                  nodes edges labels clusters font-size)))))

(defun get-simple-graph-positions (graph &key node-class-map edge-class-map)
  "Font is enforced to be Anonymous Pro Bold and font size is enforced to 12(pt).
I have actually verified on Firefox that this configuration will have the same rendering as setting the font-size to 1em or 16px (see http://kyleschaeffer.com/development/css-font-size-em-vs-px-vs-pt-vs/).
Since the target of all my project is viewing things with web browsers, this configuration is the easiest one to depart from."
  (dot-parser
   (dot (with-output-to-string (*standard-output*)
          (dot<-graph graph :node-class-map node-class-map :edge-class-map edge-class-map)))
   :node-class-map node-class-map
   :edge-class-map edge-class-map
   :font-size 12))

(defun get-clustered-graph-positions (subgraphs &key node-class-map edge-class-map)
  "Font is enforced to be Anonymous Pro Bold and font size is enforced to 12(pt).
I have actually verified on Firefox that this configuration will have the same rendering as setting the font-size to 1em or 16px (see http://kyleschaeffer.com/development/css-font-size-em-vs-px-vs-pt-vs/).
Since the target of all my project is viewing things with web browsers, this configuration is the easiest one to depart from."
  (dot-parser
   (dot (with-output-to-string (*standard-output*)
          (dot<-subgraphs subgraphs
                          :node-class-map node-class-map
                          :edge-class-map edge-class-map)))
   :node-class-map node-class-map
   :edge-class-map edge-class-map
   :font-size 12
   :subgraphs subgraphs))

(defun get-strict-clustered-graph-positions (strict-clustered-graph &key node-class-map edge-class-map)
  (list :graphs (loop for component in (clustered-graph-components strict-clustered-graph)
                      collect (get-simple-graph-positions
                               (first (clustered-graph-component-clusters component))
                               :node-class-map node-class-map
                               :edge-class-map edge-class-map))
        :singles (list :nodes (clustered-graph-singles-nodes strict-clustered-graph))))