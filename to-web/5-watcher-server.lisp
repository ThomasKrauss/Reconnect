(in-package :to-web)

(defparameter *nodejs-server-filename* "nodejs-side-server.js")

(defun compute-nodejs-url (port &optional (site-name "site/"))
  "Compute the url to the local NodeJS server."
  (format nil "http://localhost:~s/~a" port site-name))

(defun ps-symb (str)
  "In order to have exactly the right case in the given string as in the output symbol."
  (symb
   (cond
    ((string= str (string-upcase str))
     (string-downcase str))
    ((string= str (string-downcase str))
     (string-upcase str))
    (t str))))

(defmacro dops ((x l) &body body)
  "Interface like dolist but underneath apply ps* over... Well just read the code!."
  `(apply #'ps* (mapcar (lambda (,x) ,@body) ,l)))

(defun nodejs-require (lst)
  "Generate the instructions for adding required modules.
Invalid characters (like .) are ignored in variable names and thus skipped."
  (dops (x lst)
    `(var ,(ps-symb (remove #\. x))
          (require ,x))))

(defun nodejs-global-variables (lst)
  "Generate the instructions for declaring all necessary global variables."
  (dops (x lst)
    (if (atom x)
      `(var ,x)
      `(var ,(first x) ,(second x)))))

(defun nodejs-server-config (server-var attributes methods)
  "Generate the instructions for configuring the static files server."
  (concatenate 'string
               (dops (x attributes)
                 `(setf (@ ,server-var ,(first x))
                        ,(second x)))
               '(#\Newline)
               (dops (x methods)
                 `(chain ,server-var ,x))))

(defmacro with-ps-to-file (with-open-file-spec &body body)
  ""
  (let ((stream (first with-open-file-spec)))
    `(with-open-file ,with-open-file-spec
       ,@(loop for x in body
               for index from 0
               if (eql (first x) '//)
               collect `(progn
                          ,@(when (< 0 index)
                              `((write-char #\Newline ,stream)
                                (write-char #\Newline ,stream)))
                          ,@(when (< 1 (length (rest x)))
                             `((write-string "/*" ,stream)
                               (write-char #\Newline ,stream)))
                          ,@(loop for comment in (normalize-string (rest x) 80)
                                  collect `(write-string (concatenate 'string ,(if (= 1 (length (rest x)))
                                                                                 "// " "   ")
                                                                      ,comment) ,stream)
                                  collect `(write-char #\Newline ,stream))
                          ,@(when (< 1 (length (rest x)))
                             `((write-string "*/" ,stream)
                               (write-char #\Newline ,stream))))
               else
               collect `(write-string ,x ,stream)))))

(defun install-nodejs-library (library-name current-directory)
  (npm "install" (list library-name) :current-directory current-directory))

(defun generate-nodejs-server (site-path)
  (let ((nodejs-file (merge-pathnames *nodejs-server-filename* site-path)))
    (with-ps-to-file (s nodejs-file
                        :direction :output
                        :if-exists :supersede)
      (// "Command-line options are: port &optional system-name"
          ""
          "The port is mandatory. The watcher will listen to requests on it."
          "Only one system at a time can be watched. By default, none is watched on startup."
          "Unless a system-name is given after the port number."
          ""
          "Events sent through sockets have a name and a content."
          ""
          "The name of the event is the name of the resource. So it's the filename if the resource is a single file or the name of the directory holding all other files and directories."
          ""
          "The content is always an array. It holds objects structured the same way: a 'keys' property and a 'value' property. The keys are the name of all directories up to the name of the file. The value depends on the file extension (more on that after examples)."
          ""
          "Examples:"
          "- Single file 'foo': [{'keys': [], 'value': ...}] and the event name is the resource name: 'foo'"
          "- Directory holding one file 'foo/bar.baz': [{'keys': ['bar'], 'value': ...}] and the event name is 'foo'"
          "- Directory 'foo' holding 'bar.baz' and 'bat.baz':"
          "[{'keys': ['bar'], 'value': ...}, {'keys': ['bat'], 'value': ...}] and the event name is 'foo'"
          "- Directory 'foo' holding 'bar.baz' and 'foop/bat.baz':"
          "[{'keys': ['bar'], 'value': ...}, {'keys': ['foop','bat'], 'value': ...}] and the event name is 'foo'"
          ""
          "The value depends on the file extension but there are two important exceptions to the presented mechanism:"
          "- the file was deleted, not created or modified"
          "-> as no data could be transmitted, the value is null and the event name is prepended with 'no-'. Only the 'keys' property follow the rules presented above."
          "- the file extension is 'tmp'"
          "-> no data is even emitted through the sockets: a temporary file is purely ignored."
          ""
          "Other than these two cases, the supported extensions are:"
          "- png: the value is the path to the file"
          "- json: the value is the JSON content of the file")
      (// "Required modules")
      (nodejs-require '("http" "url" "querystring" "path" "fs" "inertia" "socket.io" "watch"))
      (// "Global variables")
      (nodejs-global-variables `(port
                                 initial-system-name
                                 (static-server (chain inertia (create-handler)))
                                 io
                                 app
                                 notifier))
      (// "Parse port from command line")
      (ps*
       '(setf port (when (> (@ process argv length) 2)
                     (* (elt (@ process argv) 2) 1)))
       '(if (string= (typeof port) "number")
          (setf initial-system-name (when (> (@ process argv length) 3)
                                      (elt (@ process argv) 3)))
          (progn
            (chain console (error (concatenate 'string "The port number " port " given is not recognized as a number. Arguments to the watcher server are " (chain *json* (stringify (@ process argv))))))
            (setf port nil))))
      (// "Server config")
      (nodejs-server-config 'static-server
                            (list '(use-cache nil)
                                  '(use-compression t))
                            (list '(compress "css" "html" "js" "json")
                                  '(max-age "css" "html" "js" "json" "png" 0)
                                  '(add-file-handler (regex "/^\\/index\\.html|\\/refresh\\.js$/i"))))
      (// "The notifier has 3 aspects necessary to its job"
          "1 - It pushes notifications through sockets"
          "-> expose methods 'socketId addSocket(socket)' and 'removeSocket(socketId)' to record/unrecord a socket"
          "2 - It proposes to emit notifications about a specific file"
          "-> expose methods 'fileHasChanged(filePath)' and 'fileHasBeenDeleted(filePath)'"
          "3 - It proposes to output the content it normally emits through sockets to a string, such that any browser-side component can ask for some data without having to wait for it to change and still gets it in the very same format used in notifications"
          "-> expose method 'getResources(name)'")
      (ps*
       `(setf notifier
              (let ()
                (var notifier (create))
                (var sockets (array))
                (var watched-system-name)
                (var watched-resources (array))
                (var site-base-path "c:/home/lisp/watcher/")
                (var resource-config-file "c:/home/lisp/watcher/resources.json")
                (var emit (lambda (event-name content)
                            (when (and event-name (or content
                                                      (> (@ content length) 0)))
                              (loop for socket in sockets
                                    when socket
                                    do (chain socket (emit event-name (chain *json* (stringify content))))))))
                (var get-site-path (lambda ()
                                     (concatenate 'string watched-system-name "/")))
                (var get-watch-path (lambda ()
                                      (concatenate 'string site-base-path (get-site-path))))
                (var get-notification-data (lambda (file resource-path file-does-not-exist)
                                             (var extension (chain path (extname file)))
                                             (var event-name)
                                             (var directories)
                                             (var filename)
                                             (var keys)
                                             (var value)
                                             (when (string= (chain extension (char-at 0)) ".")
                                               (setf extension (chain extension (slice 1))))
                                             (when (not (string= extension "tmp"))
                                               (setf directories (chain path
                                                                        (dirname (chain path (relative resource-path file)))
                                                                        (split "\\"))
                                                     filename (elt (chain path (basename file) (split (regex "/\\./"))) 0))
                                               (if (string= (elt directories 0) ".")
                                                 (setf event-name filename
                                                       keys (array))
                                                 (setf event-name (elt directories 0)
                                                       keys (chain directories (slice 1) (concat filename))))
                                               (if file-does-not-exist
                                                 (setf value nil)
                                                 (cond
                                                  ((string= extension "png")
                                                   (setf value (chain path (relative resource-path file)
                                                                      (replace (regex "/\\\\/g") "/"))))
                                                  ((string= extension "json")
                                                   (try
                                                    (setf value (chain *json* (parse
                                                                               (chain fs
                                                                                      (read-file-sync file "utf-8")))))
                                                    (:catch (e)
                                                      (setf value nil)
                                                      (chain console (error (concatenate 'string
                                                                                         "Error while parsing json from "
                                                                                         file
                                                                                         " because "
                                                                                         e)))))))))
                                             (when event-name
                                               (array event-name (array (create "keys" keys "value" value))))))
                (var next-depth-limit (lambda (depth-limit)
                                        (if (string= (typeof depth-limit) "number")
                                          (- depth-limit 1)
                                          false)))
                (var collect-notification-data (lambda (directory-path resource-path depth-limit ignore-limit)
                                                 (var result (array))
                                                 (var stats)
                                                 (var full-path)
                                                 (var data)
                                                 (var resources (chain fs (readdir-sync directory-path)))
                                                 (loop for resource in resources
                                                       do (progn
                                                            (setf full-path
                                                                  (concatenate 'string directory-path "/" resource)
                                                                  stats
                                                                  (chain fs (stat-sync full-path)))
                                                            (cond
                                                             ((chain stats (is-file))
                                                              (when (< ignore-limit 0)
                                                                (setf data (get-notification-data full-path resource-path))
                                                                (when data
                                                                  (setf result (chain result (concat (elt data 1)))))))
                                                             ((chain stats (is-directory))
                                                              (when (or
                                                                     (and (string= (typeof depth-limit) "boolean")
                                                                          (not depth-limit))
                                                                     (< 0 depth-limit))
                                                                (setf result
                                                                      (chain result
                                                                             (concat
                                                                              (collect-notification-data full-path
                                                                                                         resource-path
                                                                                                         (next-depth-limit depth-limit)
                                                                                                         (- ignore-limit 1))))))))))
                                                 result))
                (var clean-socket-store (lambda ()
                                          (let ((pos (@ sockets length)))
                                            (loop while (eql nil (elt sockets pos))
                                                  do (decf pos))
                                            (when (< pos (@ sockets length))
                                              (chain sockets (splice pos))))))
                (var page-has-changed (lambda (file)
                                        (emit "pageHasChanged"
                                              (create "keys" (array)
                                                      "value" (chain path (relative (get-watch-path) file)
                                                                     (replace (regex "/\\\\/g") "/"))))))
                (var file-has-changed (lambda (file resource-path)
                                        (var data (get-notification-data file resource-path))
                                        (when data
                                          (chain emit (apply nil data)))))
                (var file-has-been-deleted (lambda (file resource-path)
                                             (var data (get-notification-data file resource-path true))
                                             (when data
                                               (emit (concatenate 'string "no-" (elt data 0)) (elt data 1)))))
                (var scan-for-resources (lambda (name resource-path depth-limit ignore-limit)
                                          (var result (array))
                                          (var scan-ok false)
                                          (var stats)
                                          (var base-path (concatenate 'string resource-path name))
                                          (var parent-base-path (chain base-path
                                                                       (slice 0 (chain base-path (last-index-of "/")))))
                                          (var i 0)
                                          (var resources)
                                          (var file-name (chain name (slice (+ (chain name (last-index-of "/")) 1))))
                                          (var data)
                                          (var target-file)
                                          (when (chain fs (exists-sync parent-base-path))
                                            (try
                                             (setf resources (chain fs (readdir-sync parent-base-path)))
                                             (:catch (e)
                                               (setf resources nil)
                                               (chain console (log e))))
                                            (when resources
                                              (loop while (and (not target-file)
                                                               (< i (length resources)))
                                                    if (and (string= (chain path (basename (elt resources i)
                                                                                           (chain path (extname (elt resources i)))))
                                                                     file-name)
                                                            (not (string= (chain path (extname (elt resources i))) "")))
                                                    do (setf target-file (concatenate 'string parent-base-path "/" (elt resources i)))
                                                    do (incf i))
                                              (when target-file
                                                (setf data (get-notification-data target-file resource-path))
                                                (when (and data (< ignore-limit 0))
                                                  (setf result (chain result (concat (elt data 1)))
                                                        scan-ok t)))
                                              (when (and (chain fs (exists-sync base-path))
                                                         (or
                                                          (and (string= (typeof depth-limit) "boolean")
                                                               (not depth-limit))
                                                          (< 0 depth-limit)))
                                                (setf result (chain result
                                                                    (concat
                                                                     (collect-notification-data base-path resource-path
                                                                                                (next-depth-limit depth-limit)
                                                                                                (- ignore-limit 1))))
                                                      scan-ok t))))
                                          (if scan-ok result null)))
                (var watch-resource (lambda (resource-path)
                                      (chain watched-resources (push resource-path))
                                      (chain watch (watch-tree resource-path
                                                               (lambda (stat current previous)
                                                                 (unless (and (string= (typeof stat) "object")
                                                                              (null previous)
                                                                              (null current))
                                                                   (cond
                                                                    ((null previous)
                                                                     (file-has-changed stat resource-path))
                                                                    ((= (@ current nlink) 0)
                                                                     (file-has-been-deleted stat resource-path))
                                                                    (t
                                                                     (file-has-changed stat resource-path)))))))))
                (var watch-resources-for (lambda (system-name)
                                           (var config)
                                           (var i)
                                           (var system-config)
                                           (try
                                            (setf config (chain *json*
                                                                (parse (chain fs
                                                                              (read-file-sync resource-config-file
                                                                                              "utf-8")))))
                                            (:catch (e)
                                              (chain console
                                                     (error
                                                      (concatenate 'string
                                                                   "Problem while reading the resources configuration file at "
                                                                   resource-config-file)))))
                                           (when config
                                             (loop for resource in watched-resources
                                                   do (chain watch (unwatch-tree resource)))
                                             (setf i 0)
                                             (loop while (and (< i (@ config length))
                                                              (not system-config))
                                                   when (string= (@ (elt config i) name) system-name)
                                                   do (setf system-config (@ (elt config i) resources))
                                                   do (incf i))
                                             (if system-config
                                               (loop for resource in system-config
                                                     do (chain console (log (concatenate 'string
                                                                                         "Watching resource "
                                                                                         resource)))
                                                     do (watch-resource resource))
                                               (setf watched-resources (array))))))
                (setf (@ notifier get-watched-system-name)
                      (lambda ()
                        watched-system-name)
                      (@ notifier get-resources)
                      (lambda (name depth-limit ignore-limit)
                        (var i 0)
                        (var result)
                        (var _depth-limit (if (string= depth-limit "false")
                                            false
                                            (if (and depth-limit
                                                     (not (is-na-n depth-limit)))
                                              (* depth-limit 1)
                                              0)))
                        (var _ignore-limit (if (and ignore-limit
                                                    (not (is-na-n ignore-limit)))
                                             (* ignore-limit 1)
                                             -1))
                        (loop while (and (< i (@ watched-resources length))
                                         (not result))
                              do (setf result (scan-for-resources name (elt watched-resources i) _depth-limit _ignore-limit))
                              do (incf i))
                        (if result result (array)))
                      (@ notifier notify-action)
                      (lambda (params)
                        (var action-name (@ params name))
                        (when action-name
                          (emit action-name params))
                        (if action-name t false))
                      (@ notifier watch-system)
                      (lambda (system-name)
                        (unless (string= watched-system-name system-name)
                          (when watched-system-name
                            (chain watch (unwatch-tree (get-watch-path)))
                            (chain static-server (pop-dir-handler)))
                          (chain console (log (concatenate 'string "Watching system " system-name)))
                          (setf watched-system-name system-name)
                          (watch-resources-for system-name)
                          (chain watch
                                 (watch-tree (get-watch-path)
                                             (lambda (stat current previous)
                                               (unless
                                                   (and (string= (typeof stat) "object")
                                                        (null previous)
                                                        (null current))
                                                 (when (or (null previous)
                                                           (/= (@ current nlink) 0))
                                                   (page-has-changed stat))))))
                          (chain static-server (add-dir-handler (get-site-path)))
                          (page-has-changed (concatenate 'string (get-watch-path) "index.html"))
                          system-name))
                      (@ notifier add-socket)
                      (lambda (s)
                        (1- (chain sockets (push s))))
                      (@ notifier remove-socket)
                      (lambda (socket-id)
                        (setf (elt sockets socket-id) nil)
                        (clean-socket-store)))
                notifier)))
      (// "Actual server logic, should we..."
          "1 - ... serve the urls of the various servers?"
          "2 - ... serve a resource?"
          "=> At this point, any content to be served must be in JSON format."
          "=> If these special logics have not been activated, we serve static elements."
          "So, should we..."
          "3 - ... override the root page url to actually serve the index.html?"
          "4 - ... try to serve a static element?"
          "=> if everything has failed, we return a 404 error")
      (ps* `(setf app
                  (chain http
                         (create-server
                          (lambda (req res)
                            (var str)
                            (var requested-url (chain url (parse (@ req url) t)))
                            (var watched-system-name (chain notifier (get-watched-system-name)))
                            (var process (lambda (obj callback)
                                           (var str)
                                           (if obj
                                             (progn
                                               (setf str (chain *json* (stringify obj)))
                                               (when callback
                                                 (setf str (concatenate 'string callback "(" str ")")))
                                               (chain res
                                                      (write-head 200
                                                                  (create "Content-Length" (chain *buffer (byte-length str))
                                                                          "Content-Type" "text/json")))
                                               (chain res (end str)))
                                             (progn
                                               (when (and ,@(loop for file in *js-watcher-files*
                                                                  collect `(not (string= (@ requested-url pathname) ,file))))
                                                 (setf (@ req url) "/")
                                                 (when watched-system-name
                                                   (setf (@ req url)
                                                         (concatenate 'string (@ req url) watched-system-name "/")))
                                                 (if (or (string= (@ requested-url pathname) "/")
                                                         (string= (@ requested-url pathname) ""))
                                                   (setf (@ req url)
                                                         (concatenate 'string (@ req url) "index.html"))
                                                   (if (string= (chain (@ requested-url pathname)
                                                                       (char-at 0))
                                                                "/")
                                                     (setf (@ req url)
                                                           (concatenate 'string (@ req url)
                                                                        (chain (@ requested-url pathname)
                                                                               (slice 1))))
                                                     (setf (@ req url)
                                                           (concatenate 'string (@ req url)
                                                                        (@ requested-url pathname)))))
                                                 (setf (@ req url)
                                                       (chain querystring (unescape (@ req url)))))
                                               (when (not (chain static-server (serve req res)))
                                                 (chain res (write-head 404))
                                                 (chain res (end (concatenate 'string
                                                                              "Nothing found at " (@ req url)))))))))
                            (cond
                             ((string= (@ requested-url pathname) "/get-resources")
                              (process (chain notifier (get-resources (@ requested-url query name)
                                                                      (@ requested-url query depth-limit)
                                                                      (@ requested-url query ignore-limit)))
                                       (@ requested-url query callback)))
                             ((string= (@ requested-url pathname) "/watch")
                              (cond
                               ((and (string= (@ req method) "GET")
                                     (@ requested-url query system-name))
                                (process (create "watch" (array (chain notifier
                                                                       (watch-system
                                                                        (@ requested-url query system-name)))))))
                               ((string= (@ req method) "POST")
                                (setf str "")
                                (chain req (on "data" (lambda (chunk)
                                                        (setf str (concatenate 'string str chunk)))))
                                (chain req (on "end" (lambda ()
                                                       (setf str (chain querystring (parse str) system-name))
                                                       (process (create "watch" (array (chain notifier (watch-system str)))))))))
                               (t
                                (process))))
                             ((string= (@ requested-url pathname) "/act")
                              (process (chain notifier (notify-action (@ requested-url query)))
                                       (@ requested-url query callback)))
                             (t
                              (process))))))))
      (// "Using Socket IO to communicate back to the browser"
          "-> When a connection is established, record it to push notifications through it"
          "-> Of course, handle the disconnect event to get rid of the rendered useless socket!"
          ""
          "Monitoring directories from the root directory"
          "No difference is made between file creation and file modification")
      (ps*
       `(when port
          (setf io (chain socketio (listen app)))
          (chain app (listen port))
          (chain console (log (concatenate 'string "Watcher set on port " port)))
          (when initial-system-name
            (chain notifier (watch-system initial-system-name)))
          (chain io sockets (on "connection"
                                (lambda (socket)
                                  (var socket-id (chain notifier (add-socket socket)))
                                  (chain socket (on "disconnect" (lambda ()
                                                                   (chain notifier (remove-socket socket-id)))))))))))
    nodejs-file))

(defun generate-watcher-index (directory)
  (with-html-output-to-file ((merge-pathnames "index.html" directory))
    (html
      (:html :xmlns "http://www.w3.org/1999/xhtml" :|xml:lang| "en" :lang "en"
       (:head
        (:meta :http-equiv "Content-type" :content "text/html;charset=utf-8")
        (:title "Watcher index"))
       (:body
        (:p "Waiting to watch something")
        (:js "/socket.io/socket.io.js")
        (:js "/refresh.js"))))))

(defun generate-refresh-js (directory)
  (let ((refresh-file (merge-pathnames "refresh.js" directory)))
    (with-ps-to-file (s refresh-file
                        :direction :output
                        :if-exists :supersede)
      (ps*
       '(var *end-points)
       '(var make-end-points
             (lambda ()
               (var end-points)
               (var watcher-url)
               (var socket)
               (var shall-handle-data (lambda (expected-keys data-keys)
                                        (var result)
                                        (var i 0)
                                        (setf result (= (@ expected-keys length)
                                                        (@ data-keys length)))
                                        (loop while (and result
                                                         (< i (@ expected-keys length)))
                                              do (setf result (string= (elt expected-keys i)
                                                                       (elt data-keys i)))
                                              do (incf i))
                                        result))
               (var make-listener (lambda (keys event-name callback)
                                    (lambda (json-string)
                                      (var data)
                                      (try
                                       (setf data (chain *json* (parse json-string)))
                                       (:catch (e)
                                         (chain console
                                                (error
                                                 (concatenate 'string
                                                              "Cannot parse JSON on socket communication of event "
                                                              event-name
                                                              ". Error is "
                                                              e)))))
                                      (when data
                                        (chain $ (map data
                                                      (lambda (item)
                                                        (when (shall-handle-data keys (@ item keys))
                                                          (chain callback (apply null (array item)))))))))))
               (var on (lambda (name cb)
                         (var event-name)
                         (var keys)
                         (if (chain $ (is-function cb))
                           (progn
                             (setf keys (chain name (split "/"))
                                   event-name (elt (chain keys (splice 0 1)) 0))
                             (chain socket (on event-name (make-listener keys event-name cb)))
                             (chain socket (on (concatenate 'string "no-" event-name)
                                               (make-listener keys
                                                              (concatenate 'string "no-" event-name)
                                                              cb)))
                             (chain $ (ajax (create "url" (concatenate 'string watcher-url "get-resources")
                                                    "dataType" "jsonp"
                                                    "data" (create "name" name)
                                                    "success" (lambda (data)
                                                                (when data
                                                                  (chain $ (map data
                                                                                (lambda (item)
                                                                                  (chain cb (apply null (array item))))))))
                                                    "error" (lambda (request status error)
                                                              (chain console (error
                                                                              (concatenate 'string
                                                                                           "Cannot initialize data for resource "
                                                                                           name
                                                                                           ". Error is"
                                                                                           (or status error)))))))))
                           (chain console (error
                                           (concatenate 'string
                                                        "Cannot setup listening to resource "
                                                        name
                                                        ": the callback is actually not a function!"))))))
               (var scroll-to-position (lambda (position)
                                         (when (and position
                                                    (elt position 1)
                                                    (elt position 2))
                                           (chain window (scroll-to (parse-int (elt position 1))
                                                                    (parse-int (elt position 2)))))))
               (var handle-page-change (lambda (url)
                                         (let* (x y
                                                  (full-url (chain *json* (parse url) value))
                                                  (extension (chain full-url (slice (+ 1 (chain full-url (last-index-of ".")))))))
                                           (cond
                                            ((string= extension "html")
                                             (if (/= (chain (decode-u-r-i (@ window location href))
                                                            (index-of full-url))
                                                     -1)
                                               (setf x (if (chain document page-x-offset)
                                                         (chain document page-x-offset)
                                                         (chain document body scroll-left))
                                                     y (if (chain document page-y-offset)
                                                         (chain document page-y-offset)
                                                         (chain document body scroll-top))
                                                     (chain window location search) (concatenate 'string "?x=" x "&y=" y))
                                               (setf (chain window location pathname) full-url)))
                                            ((or (string= extension "css")
                                                 (string= extension "js"))
                                             (chain window location (reload t)))))))
               (var get-watcher-url (lambda (pathname)
                                      (if (concatenate 'string watcher-url pathname)
                                        pathname
                                        "")))
               (var reset (lambda ()
                            (var previous-position)
                            (setf end-points (create)
                                  watcher-url (concatenate 'string (@ window location protocol) "//"
                                                           (@ window location host) "/")
                                  previous-position (chain (regex "/\\?x=([0-9]*)&y=([0-9]*)/")
                                                           (exec (@ window location search))))
                            (scroll-to-position previous-position)
                            (setf socket (chain io (connect watcher-url)))
                            (chain socket (on "pageHasChanged" handle-page-change))
                            (setf (@ end-points get-watcher-url) get-watcher-url
                                  (@ end-points on) on)))
               (reset)
               end-points))
       '(setf *end-points (make-end-points))))
    refresh-file))

(defun generate-resources-json (directory)
  (let ((file (merge-pathnames "resources.json" directory)))
    (with-open-file (s file
                       :direction :output
                       :if-exists :supersede)
      (write-string
       (json<-lisp (list '(:name "code-editor" :resources ("c:/home/lisp/work/my-lisp-parsing/caches/"
                                                           "c:/home/lisp/work/code-editor/"))
                         '(:name "overview" :resources ("c:/home/lisp/work/my-lisp-parsing/caches/"))))
       s))))
       