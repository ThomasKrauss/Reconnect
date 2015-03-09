(in-package :my-graphs)

(defun doc-graph-with-links (action-name)
  `(let ((nodes (loop for i from 1 to 7
                     if (not lenient)
                     collect (make-node (mkstr i))
                     collect (make-node (mkstr (code-char (+ 64 i))))))
        (links (list (make-link% "A" '("B" "1"))
                     (make-link% "B" '("2"))
                     (make-link% "C" '("D" "3"))
                     (make-link% "D" '("4"))
                     (make-link% "E" '("F" "5"))
                     (make-link% "F" '("6")))))
    (if constructor
      (,action-name nodes :links links :directed directed :lenient lenient)
      (let ((graph (,action-name nil :directed directed :lenient lenient)))
        (dolist (node nodes)
          (add-node node graph))
        (dolist (link links)
          (add-link link graph))
        graph))))

(defun doc-graph-with-edges (action-name)
  `(let ((nodes (loop for i from 1 to 7
                      unless lenient collect (make-node (mkstr i))
                      collect (make-node (mkstr (code-char (+ 64 i))))))
         (edges (append (loop for i from 1 to 6
                              collect (make-edge (make-node (mkstr (code-char (+ 64
                                                                                 i))))
                                                 (make-node (mkstr i))))
                        (list (make-edge (make-node "A") (make-node "B"))
                              (make-edge (make-node "C") (make-node "D"))
                              (make-edge (make-node "E") (make-node "F"))))))
     (if constructor
       (,action-name nodes :edges edges :directed directed :lenient lenient)
       (let ((graph (,action-name nil :directed directed :lenient lenient)))
         (dolist (node nodes)
           (graph-add-node node graph))
         (dolist (edge edges)
           (add-edge edge graph))
         graph))))

(defmacro doc-graph-with (connection-type graph-type)
  (let ((action-name (case graph-type
                       (:basic 'make-graph%)
                       (:raw 'make-raw-graph))))
    (case connection-type
      (:links (doc-graph-with-links action-name))
      (:edges (doc-graph-with-edges action-name)))))


; Functions building graphs

(defun make-doc-graph-with-links (&key constructor directed lenient)
  (doc-graph-with :links :basic))

(defun make-doc-raw-graph-with-links (&key constructor directed lenient)
  (doc-graph-with :links :raw))

(defun make-doc-graph-with-edges (&key constructor directed lenient)
  (doc-graph-with :edges :basic))

(defun make-doc-raw-graph-with-edges (&key constructor directed lenient)
  (doc-graph-with :edges :raw))

(defun make-doc-graph-cluster (&key constructor directed lenient)
  (declare (ignore constructor))
  (make-graph-cluster
   (split-in-connected-components
    (make-doc-graph-with-links :directed directed :lenient lenient))))

(defun make-doc-raw-graph-cluster (&key constructor directed lenient)
  (declare (ignore constructor))
  (make-raw-graph-cluster
   (split-in-connected-components
    (make-doc-raw-graph-with-links :directed directed :lenient lenient))))

(defun make-doc-graph-set (&key constructor directed lenient)
  (declare (ignore constructor))
  (make-graph-set
   (split-in-connected-components
    (make-doc-graph-with-links :directed directed :lenient lenient))))

(defun make-doc-graph-cluster-set (&key constructor directed lenient)
  (declare (ignore constructor))
  (graph-cluster-set<-graph
    (make-doc-graph-with-links :directed directed :lenient lenient)
    (lambda (graph)
      (let (letter-nodes number-nodes)
        (dolist (node (graph-nodes graph))
          (if (alpha-char-p (elt (node-name node) 0))
            (push node letter-nodes)
            (push node number-nodes)))
        (mapcar (lambda (nodes)
                  (if (graph-directed? graph)
                    (lenient-graph<-directed-graph graph :nodes nodes)
                    (strict-graph<-graph graph :nodes nodes)))
                (list letter-nodes number-nodes))))))
              
; UI

(defun make-section (title files)
  (to-web:html
    (:p :style "margin-top: 40px; text-align: center; font-size: 150%; background-color: #EDEDED" (:print title))
    (:table
     (dolist (files (group files 2))
       (to-web:html
         (:tr
          (dolist (file files)
            (when file
              (to-web:html (:td :style "padding: 0 20px"
                            (:img :src (:print (file-namestring file)))
                            (:p :style "text-align: center" (:print (let-a (file-namestring file)
                                                                      (subseq it 0 (position #\. it :from-end t)))))
                            ))))))))))

(defun draw (filename graph)
  (drawing<-graph% (merge-pathnames (mkstr filename ".png")
                                    (my-systems:system-work-directory "my-graphs"))
                   graph))

(defun draw-call (keys &key directed lenient)
  (draw (join-str (mapcar #'symbol-name
                          (let (lst)
                            (push (first keys) lst)
                            (if directed (push :directed lst) (push :undirected lst))
                            (if lenient (push :lenient lst) (push :strict lst))
                            (if (member :constructor keys) (push :constructor lst) (push :by-add lst))
                            (if (member :links keys)
                              (push :links lst)
                              (push :edges lst))
                            (reverse lst)))
                  #\-)
         (funcall
          (case (first keys)
            (:basic (if (member :links keys)
                      #'make-doc-graph-with-links
                      #'make-doc-graph-with-edges))
            (:raw (if (member :links keys)
                    #'make-doc-raw-graph-with-links
                    #'make-doc-raw-graph-with-edges))
            (:cluster #'make-doc-graph-cluster)
            (:raw-cluster #'make-doc-raw-graph-cluster)
            (:graph-set #'make-doc-graph-set)
            (:graph-cluster-set #'make-doc-graph-cluster-set))
          :constructor (when (member :constructor keys) t)
          :directed directed
          :lenient lenient)))

(defun section (graph-type &key directed lenient)
  (let ((keys-lst (case graph-type
                    ((:basic :raw)
                     '((:constructor :links) (:links) (:constructor :edges) (:edges)))
                    ((:cluster :raw-cluster :graph-set :graph-cluster-set)
                     '((:links))))))
    (make-section (mkstr (case graph-type
                           (:basic "Basic")
                           (:raw "Raw")
                           (:cluster "Cluster")
                           (:raw-cluster "Raw cluster")
                           (:graph-set "Graph set")
                           (:graph-cluster-set "Graph cluster set"))
                         " - "
                         (if directed
                           "Directed"
                           "Undirected")
                         " & "
                         (if lenient
                           "lenient"
                           "strict")
                         " graph")
                   (mapcar (lambda (keys)
                             (draw-call (cons graph-type keys)
                                        :directed directed
                                        :lenient lenient))
                             keys-lst))))

(defun generate-doc (&optional exclude)
  (to-web:with-html-output-to-file ((merge-pathnames "graph-doc.html"
                                                     (my-systems:system-work-directory "my-graphs")))
    (dolist (key '(:basic :raw :cluster :raw-cluster :graph-set)); :graph-cluster-set))
      (unless (member key exclude)
        (section key :directed nil :lenient nil)
        (section key :directed nil :lenient t)
        (section key :directed t :lenient nil)
        (section key :directed t :lenient t)))))