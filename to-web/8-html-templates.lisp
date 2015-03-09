(in-package :to-web)

(defmacro base-page ((title &key css js) &body body)
  (with-gensyms (single-css single-js)
    `(html
       (:html :xmlns "http://www.w3.org/1999/xhtml"  :xml\:lang "en" :lang "en"
        (:head 
         (:meta :http-equiv "Content-Type" :content "text/html;charset=utf-8")
         (:meta :name "viewport" :content "width=device-width, initial-scale=1")
         (:title ,title)
         (dolist (,single-css ,css)
           (html (:css ,single-css))))
        (:body
         ,@body
         (when *watched*
           (html
             ,@(mapcar (lambda (file)
                         `(:js ,file))
                       *js-watcher-files*)))
         (dolist (,single-js ,js)
           (html (:js ,single-js))))))))

(defmacro article-page ((title) &body body)
  `(base-page (,title :css '("article.css"))
     (:div :class "main"
      (:div :class "page"
       (:div :class "article"
        ,@body)))))