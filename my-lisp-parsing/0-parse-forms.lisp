(in-package :my-lisp-parsing)

(defun make-inner-expand-macro-code (expand-name)
  `(macrolet ((,expand-name (form &environment env)
                (with-gensyms (expansion expanded-p)
                  `(multiple-value-bind
                       (,expansion ,expanded-p)
                       (macroexpand-1 ',form ,env)
                     (declare (ignore ,expanded-p))
                     ,expansion))))))

(defun compute-inner-expansion (sexpr environment)
  (with-gensyms (expand-1)
    (eval (embed-in-environment
           `(,expand-1 ,sexpr)
           (add-scope (make-inner-expand-macro-code expand-1) environment)))))

(defun collect-atoms (initial-list atoms)
  (if atoms
    (if (listp atoms)
      (append atoms initial-list)
      (cons atoms initial-list))
    initial-list))

(defun parse-actions-in-definitions (definitions environment &key use-definitions)
  (let (run-actions
        install-actions)
    (dolist (body (mapcar (lambda (x)
                            (rest (rest x)))
                          (second definitions)))
      (multiple-value-bind
          (used-run-actions used-install-actions)
          (parse-actions-in-sexpr nil nil nil body (if use-definitions
                                          (add-scope definitions environment)
                                          environment))
        (setf run-actions (append run-actions used-run-actions)
              install-actions (append install-actions used-install-actions))))
    (values run-actions install-actions)))

(defmacro forward-definition-parsing (&key use-definitions)
  (with-gensyms (definitions run-actions install-actions)
    `(let ((,definitions (subseq sexpr 0 2)))
       (multiple-value-bind
           (,run-actions ,install-actions)
           (parse-actions-in-definitions ,definitions environment :use-definitions ,use-definitions)
         (values (rest (rest sexpr))
                 ,run-actions
                 ,install-actions
                 ,definitions)))))

(defun parse-actions-in-sexpr (sexpr &optional run-actions install-actions other-perimeters
                                     (environment (make-environment)))
  (let ((ignore-symbol-names '("DECLARE" "HASH-VALUE" "HASH-KEY")))
    (macrolet ((rec-call (sexpr &key run-actions install-actions other-perimeters environment)
                 `(parse-actions-in-sexpr ,sexpr
                                          ,(aif run-actions it 'run-actions)
                                          ,(aif install-actions it 'install-actions)
                                          ,(aif other-perimeters it 'other-perimeters)
                                          ,(aif environment it 'environment))))
      (if (or (null sexpr)
              (atom sexpr))
          (if (and (not (null sexpr))
                   (find-symbol-macrolet sexpr environment))
              (rec-call (compute-inner-expansion sexpr environment))
            (if other-perimeters
                (rec-call (first other-perimeters)
                          :other-perimeters (rest other-perimeters))
              (values (nreverse run-actions)
                      (nreverse install-actions))))
        (if (atom (first sexpr))
            (multiple-value-bind (action-perimeter
                                  used-run-actions
                                  used-install-actions
                                  scope)
                (acond
                 ((find-action (first sexpr) environment)
                  (case it
                    (:run
                     (rest sexpr))
                    (:install
                     (list (compute-inner-expansion sexpr environment)))))
                 ((cl-function? (first sexpr))
                  (values (rest sexpr) it))
                 ((cl-macro? (first sexpr))
                  (values (list (macroexpand-1 sexpr))
                          nil
                          (unless (equal 'my-backquote:backquote it)
                            it)))
                 ((cl-special-action? (first sexpr))
                  (multiple-value-bind (action-perimeter
                                        used-run-actions
                                        used-install-actions
                                        scope)
                      (case it
                        ((catch go if load-time-value locally multiple-value-call multiple-value-prog1
                           progn progv setq tagbody throw unwind-protect)
                         (rest sexpr))
                        (function
                         (list (rest sexpr)))
                        ((block eval-when return-from the)
                         (rest (rest sexpr)))
                        ((let let*)
                         (append (reduce #'append (mapcar (lambda (x)
                                                            (when (listp x) (rest x)))
                                                          (first (rest sexpr))))
                                 (rest (rest sexpr))))
                        (quote
                         nil)
                        ((flet macrolet symbol-macrolet)
                         (forward-definition-parsing))
                        ('labels
                         (forward-definition-parsing :use-definitions t))
                        (t
                         (error "Unable to parse the special operator ~a" it)))
                    (values action-perimeter
                            (cons (first sexpr) used-run-actions)
                            used-install-actions
                            scope)))
                 ((find (symbol-name (first sexpr)) ignore-symbol-names :test #'string=)
                  nil)
                 (t
                  (error "Find the non-action ~s of package ~a as the first element of ~s"
                         (first sexpr) (package-name (symbol-package (first sexpr))) sexpr)))
              (parse-actions-in-sexpr
               (first action-perimeter)
               (collect-atoms run-actions used-run-actions)
               (collect-atoms install-actions used-install-actions)
               (append (rest action-perimeter) other-perimeters)
               (aif scope (add-scope it environment) environment)))
          (if (eq 'lambda (first (first sexpr)))
              (let ((lambda-body (rest (rest (first sexpr)))))
                (rec-call (first lambda-body)
                          :run-actions (cons 'lambda run-actions)
                          :other-perimeters (append other-perimeters
                                                    (rest lambda-body)
                                                    (rest sexpr))))
            (let ((*print-escape* t)
                  (*print-circle* nil)
                  (*print-array* t)
                  (*print-level* nil)
                  (*print-length* nil)
                  (*print-pretty* t))
              (error "Find the non-lambda expression ~s as action in ~s" (first sexpr) sexpr))))))))

(defun filter-symbols (lst &key keep-packages (ignore-packages '(:common-lisp :compiler :system :lispworks :setf :dspec)))
  (remove-if (lambda (x)
               (if keep-packages
                 (not (member (as-keyword (package-name (symbol-package x)))
                              keep-packages))
                 (member (as-keyword (package-name (symbol-package x)))
                         ignore-packages)))
             lst))

(defun parse-module-actions (module-name system-name &key keep-packages (ignore-packages '(:common-lisp :compiler :system :lispworks :setf :dspec)))
  "restrict can either be :module, :system or nil.
This order reflect the simplicity of the graph, from simple to complex.
Indeed, with :module, only links between actions of the given module will be considered.
With :system, only links between actions of the given system will be considered. Compared to the previous graph, all the other modules of the given system are back in the picture.
With nil, every action, whatever their related system, is considered when linked to one of the given module."
  "keep-packages has priority over ignore-packages"
  (flet ((filter (lst)
           (filter-symbols lst :keep-packages keep-packages :ignore-packages ignore-packages)))
    (let (result)
      (with-open-file (s (system-module-file module-name system-name))
        (loop for sexpr = (read s nil)
              while sexpr
              do (multiple-value-bind
                     (sexpr name)
                     (get-action-body sexpr)
                   (when sexpr
                     (my-bind
                         ((#'filter run-actions install-actions))
                         (parse-actions-in-sexpr nil nil nil sexpr)
                       (push (as-titled-plist name run-actions install-actions) result))))))
      (nreverse result))))
