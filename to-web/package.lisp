(in-package :cl)
(defpackage :to-web (:use :cl :my-utilities :my-systems :formatter
                     :cl-fad :cl-ppcre :hunchentoot :st-json :parenscript)
  (:shadowing-import-from :parenscript :this)
  (:export
   :launch-watcher
   :get-core-watcher-url
   :get-rest-watcher-url
   :emit-html
   :html
   :with-html-output
   :with-html-output-to-string
   :with-html-output-to-file
   :html<-text
   :html<-text-string
   :html<-text-file
   :install-watcher
   :install-resource
   :define-web-result
   :build-web-result
   :is-built-final?
   :update-web-result
   :build-web-editor
   :build-all-web-results
   :build-all-web-editors
   :rebuild-web-result
   :rebuild-web-editor
   :start-common-editor-server
   :define-web-editor
   ; util
   :with-post-parameters
   :with-get-parameters
   :as-jsonp
   :wrap-html-emitter-by-map
   ; html-templates
   :base-page
   :article-page))