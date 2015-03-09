(in-package :librarian)

(defconstant +system-doc-file+ "system.txt")

(defconstant +resources-doc-file+ "resources.txt")

(defconstant +modules-doc-file+ "modules.txt")

(defun get-resource-names (form)
  (parse-destructuring-lambda-list (third form) t))

; basic names (meaningful or not, as for instance x, y, n)
; plural forms
; hyphenated rules + maker + predicates => debugging that shows what is what

; an action produces what?
; a resource is used in which systems? Originate from?

(defun list-resource-names (action-definitions)
  (let (external-resource-names internal-resource-names)
    (macrolet ((collect (value keys place)
                 (with-gensyms (key position)
                   `(dolist (,key ,keys)
                      (let ((,position (position ,key ,place :key #'first)))
                        (if ,position
                          (setf (elt ,place ,position)
                                (list ,key (append (second (elt ,place ,position))
                                                   (list ,value))))
                          (push (list ,key (list ,value)) ,place)))))))
      (dolist (action-definition action-definitions)
        (multiple-value-bind
            (type action-name)
            (form-type action-definition)
          (when (or (eq type :function)
                    (eq type :macro))
            (let-a (get-resource-names action-definition)
              (if (is-symbol-external? action-name)
                (collect action-name it external-resource-names)
                (collect action-name it internal-resource-names)))))))
    (values external-resource-names
            (remove-if (lambda (element)
                         (member (first element) external-resource-names :key #'first))
                       internal-resource-names))))

(defun list-module-resource-names (system-name module-name)
  (list-resource-names (load-module-actions module-name system-name)))

(defun list-system-resource-names (system-name)
  (list-resource-names (loop for module-name in (system-module-names system-name)
                             append (load-module-actions module-name system-name))))

(defun html-titled-list (title lst)
  (html
    (:ul
     (:li :class "header" (:print title))
     (dolist (element lst)
       (html
         (:li
          (:span (:print (first element)))
          (:ul
           (dolist (element (second element))
             (html (:li (:print element)))))))))))

(defun html-resource-name-list (target-directory &rest names)
  (declare (ignore names))
  (labels ((my-symbol-name (symbol)
             (string-downcase (symbol-name symbol)))
           (sort-symbol-list (lst)
             (sort (mapcar (lambda (element)
                             (list (my-symbol-name (first element))
                                   (mapcar #'my-symbol-name (second element))))
                           lst)
                   (lambda (a b)
                     (string< (first a) (first b))))))
    (with-html-output-to-file ((merge-pathnames "index.html" target-directory))
      (base-page ("Resource list per system" :css '("librarian.css")
                                             :js '("librarian.js"))
        (:ul :id "resource-list"
         (dolist (system-name (mapcar #'first (get-system-and-module-list)))
           (html
             (:li (:span :class "header" (:print system-name))
              (multiple-value-bind
                  (external-resource-names internal-resource-names)
                  (list-system-resource-names system-name)
                (html-titled-list "External" (sort-symbol-list external-resource-names))
                (html-titled-list "Internal" (sort-symbol-list internal-resource-names)))))))))))

; PER MODULE

; === Panel 1: resources ===
; - production
; insert documentation
; insert debug the associated usages
; insert clustered graph dependencies
; => that's action view!

; === Panel 2: the rest of the actions ===
; topo sort actions + separate the actions by the predicates
; repeat action view

; - edition
; save doc result
; diff doc result => diff of list= (or list-equal, whatever) That is only on the resources panel

; === Panel 2: the rest of the actions ===
; action-name + doc
; Assume signature explanation after the first newline => THAT'S HOW TO GET WHAT IS PRODUCED!!
; Examples and details from debug usages
; Implementation notes from versions
; Clustered graphs for dependencies

; === Panel 3: a summary for the module itself ===
; some stats?
; then up to build a document about the system itself

(defun identified-resource-name? (resource-name)
  (fboundp (find-symbol (mkstr resource-name "?") (symbol-package resource-name))))

(defun list-module-identified-resource-names (system-name module-name)
  (remove-if-not #'identified-resource-name?
                 (mapcar #'first (list-module-resource-names system-name module-name))))

(defun list-system-identified-resource-names (system-name)
  (remove-if-not #'identified-resource-name?
                 (mapcar #'first (list-system-resource-names system-name))))

(defun get-action-signature (action-name)
  (format nil "~:[=>~;~:*~(~a~) =>~] ~:[?~;~:*~{~(~a~)~^, ~}~]"
          (third (load-action-definition action-name))
          (awhen (get-symbol-documentation action-name)
            (when (string= "=>" (subseq it 0 2))
              (cl-ppcre:split "\\s" (string-left-trim '(#\Space) (subseq it 2 (position #\Newline it))))))))

(defun get-action-documentation (action-name)
  (awhen (get-symbol-documentation action-name)
    (if (string= "=>" (subseq it 0 2))
      (let ((pos (position #\Newline it)))
        (if pos
          (subseq it (1+ pos))
          it))
      it)))

(defun html-action-view (system-name module-name action-name)
  (html
    (:div :class "header"
     (:span :class "actionName" (:print (string-downcase (symbol-name action-name))))
     (:div :class "signature"
      (dolist (string (cl-ppcre:split "\\s" (get-action-signature action-name)))
        (if (member string '("&optional" "&key" "&rest" "&whole" "&body") :test #'string=)
          (html (:span :class "keyword" (:print (mkstr #\Space string))))
          (html (:span (:print (mkstr #\Space string))))))))
    (:pre :class "summary"
     (:print (get-action-documentation action-name)))
    (:div :class "usages"
     (layout-debugged-usages action-name module-name system-name))))

(defun html-module-view (system-name module-name)
  (html
    (:ul :class "doc"
     (dolist (action-name (module-action-names module-name system-name))
       (html (:li (html-action-view system-name module-name action-name)))))))

(defun test-doc (target-directory &rest names)
  (declare (ignore names))
  (let ((module-name "0-layout")
        (system-name "my-systems"))
    (with-html-output-to-file ((merge-pathnames "index.html" target-directory))
      (base-page ("Documentation test" :css '("pure-min.css" "librarian.css" "debug.css")
                                       :js '("librarian.js"))
        (html-module-view system-name module-name)))))

(defun refresh ()
  (to-web::watch "librarian") 
  (update-web-result "librarian" "index"))

(define-web-result ("librarian" :static-site)
  :font (copy-file "Anonymous-Pro.ttf")
  :html (test-doc "index"))