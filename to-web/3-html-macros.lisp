(in-package :to-web)

(define-html-macro :css (&rest args)
  (multiple-value-bind
      (filename prefix)
      (if (eq :relative (first args))
        (values (second args) "css/")
        (values (first args) "/css/"))
    (with-gensyms (filepath)
      `(let ((,filepath ,filename))
         (push ,filepath *css-output*)
         (html (:link :type "text/css" :rel "stylesheet"
                :href (mkstr ,prefix ,filepath)))))))

(defparameter *js-watcher-files* '("/socket.io/socket.io.js"
                                   "/refresh.js"))

(defparameter *jsx-files* '("charts.js" "graph.js" "components.js" "bar-chart.js" "dashboard.js" "system.js" "module.js" "editors.js" "8-the-killer.js" "code-information.js" "code-editor.js"))

(define-html-macro :js (&rest args)
  (multiple-value-bind
      (filename prefix)
      (if (eq :relative (first args))
        (values (second args) "js/")
        (values (first args) "/js/"))
    (with-gensyms (filepath watcher-resource? jsx-file? full-url?)
      `(let* ((,filepath ,filename)
              (,watcher-resource? (member ,filepath *js-watcher-files* :test #'string=))
              (,jsx-file? (member ,filepath *jsx-files* :test #'string=))
              (,full-url? (when ,filepath (string= "http" (subseq ,filepath 0 4)))))
         (unless (or ,watcher-resource? ,full-url?)
           (push ,filepath *js-output*))
         (html (:script :type (if ,jsx-file? "text/jsx" "text/javascript")
                :src (if ,watcher-resource?
                       ,filepath
                       (if ,full-url?
                         ,filepath
                         (mkstr ,prefix ,filepath)))))))))
                      
(defparameter *js-communication-filename* "js/ajax-communication.js")

(define-html-macro :com (&rest args)
  (declare (ignore args))
  `(:script :type "text/javascript" :src *js-communication-filename*))

(define-html-macro :insert (&rest args)
  (with-gensyms (element)
    `(dolist (,element ,(first args))
       (emit-html ,element))))

(define-html-macro :a! (&rest args)
  `(:a :target "_blank" ,@args))