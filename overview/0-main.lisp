(in-package :overview)

(my-systems:defun-to-external-program firefox ("Mozilla Firefox" "firefox" '("-v" "|" "more"))
  (urls)
  "Open the given urls in Firefox."
  (call-command `("firefox" ,@urls)))

(my-systems:defun-to-external-program opera ("Opera" "launcher" nil)
  (urls)
  "Open the given urls in Opera.
Non-verifiable installation: opera does not respond to the --version command-line argument, at least on Windows."
  (call-command `("launcher" ,@urls)))

(my-systems:defun-to-external-program chrome ("Google Chrome" "chrome" nil)
  (urls &key maximized (place :center))
  "Open the given urls in Chrome.
Window can be maximized and placed on either the left, right or center (default) screen.
Non-verifiable installation: chrome does not respond to the --version command-line argument, at least on Windows."
  (let ((options (list "--no-first-run" "--chrome-frame"))); "--new-window")))
    (when maximized
      (push "--start-maximized" options))
    (case place
      (:left (push "--window-position=-2000,0" options))
      (:right (push "--window-position=2000,0" options))
      (t (push "--window-position=0,0" options)))
    (call-command (cons "chrome" (append urls options)))))

(defun make-dashboard-page (filename title js-filename target-directory)
  (with-html-output-to-file ((merge-pathnames filename target-directory))
    (base-page (title :css '("pure-min.css" "charts.css" "components.css" "jquery-ui-1.10.3.custom.css" "main2.css")
                      :js `("react.min.js" "JSXTransformer.js" "jquery-1.10.2.min.js" "jquery-ui-1.10.3.custom.min.js"
                                           "d3.min.js" "functions.js" "charts.js" "graph.js" "components.js" "bar-chart.js"
                                           ,js-filename))
      (:div :id "layout"))))

(defun make-index-page (target-directory &rest names)
  (declare (ignore names))
  (make-dashboard-page "index.html" "Dashboard" "dashboard.js" target-directory))

(defun make-system-page (target-directory &rest names)
  (declare (ignore names))
  (make-dashboard-page "system.html" "System" "system.js" target-directory))

(defun make-module-page (target-directory &rest names)
  (declare (ignore names))
  (make-dashboard-page "module.html" "Module" "module.js" target-directory))

(define-web-result ("overview" :static-site :under-core-watcher? t)
  :font (copy-file "Anonymous-Pro.ttf" "Anonymous-Pro-B.ttf")
  :html (make-index-page "index.html")
  :html (make-system-page "system.html")
  :html (make-module-page "module.html"))

(defun install-work-environment ()
  (macrolet ((commands (&rest lst)
               `(progn
                  ,@(loop for (message cmd) in (group lst 2)
                          collect `(format t ,(concatenate 'string message "~%"))
                          collect cmd))))
    (commands "Ensure all system directories exist" (my-systems:ensure-all-system-directories-exist)
              "Installing watcher server" (install-watcher :install-node-modules t)
              "Build all web results and web editors" (progn
                                                        (build-all-web-results)
                                                        (build-all-web-editors)))))

(defun initialize-work-environment ()
  (macrolet ((commands (&rest lst)
               `(progn
                  ,@(loop for (message cmd) in (group lst 2)
                          collect `(format t ,(concatenate 'string message "~%"))
                          collect cmd))))
    (commands "Start common editor server" (start-common-editor-server)
              "Print all backup files" (my-systems:print-backup-files)
              "Compute package symbols" (my-lisp-parsing:refresh-package-symbols-cache)
              "Compute line and form counts" (my-lisp-parsing:refresh-line-and-form-counts)
              "Compute compile problems" (my-lisp-parsing:refresh-compile-problems-cache)
              "Compute all usages" (my-lisp-parsing:refresh-usages-cache)
              "Compute dependencies" (my-lisp-parsing:refresh-dependencies-cache)
              "Launch sites" (progn
                               (launch-watcher :watcher-type :core)
                               (launch-watcher))
              "Launching Opera (top dashboard)" (opera (list (format nil "~a/index.html" (get-core-watcher-url))))
              "Launching Chrome (specific dashboard)" (chrome (list (get-rest-watcher-url))
                                                              :maximized t
                                                              :place :right))))