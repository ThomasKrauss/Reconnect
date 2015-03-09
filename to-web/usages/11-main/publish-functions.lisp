("Give couple (function-name module-name) from a given system that you wish to publish to the web. The handler for the server will be automatically generated as well as the Javascript function to perform the AJAX call"
 ("Example"
  (equal (let ((*application-published-functions* nil))
           (publish-functions "to-web" '("get-action-resources" "1-main") '("publish-functions" "1-main")
                              '("get-free-nodejs-port" "3-side-server")))
         '(("to-web" (("get-action-resources" "1-main") ("publish-functions" "1-main")
                      ("get-free-nodejs-port" "3-side-server"))))))
 ("Call to publish-functions with the same system name are NOT cumulative"
  (equal (let ((*application-published-functions* nil))
           (publish-functions "to-web" '("get-action-resources" "1-main") '("publish-functions" "1-main")
                              '("get-free-nodejs-port" "3-side-server"))
           (publish-functions "to-web" '("make-web-handler" "1-main")))
         '(("to-web" (("make-web-handler" "1-main"))))))
 ("No impact calling it with another system name"
  (list= (let ((*application-published-functions* nil))
           (publish-functions "to-web" '("get-action-resources" "1-main") '("publish-functions" "1-main")
                              '("get-free-nodejs-port" "3-side-server"))
           (publish-functions "my-systems" '("system-source" "0-layout") '("system-work" "0-layout")))
         '(("to-web" (("get-action-resources" "1-main") ("publish-functions" "1-main")
                      ("get-free-nodejs-port" "3-side-server")))
           ("my-systems" (("system-source" "0-layout") ("system-work" "0-layout")))))))
