("Generate a web handler for a function <foo> named web-<foo>."
 ("Ensure there is no function bound to web-no-backslashes"
  (not (fboundp 'my-utilities::web-no-backslashes)))
 ("Perform and check the generation. Delete the global binding afterwards."
  (let (generated-function-name
        (*request* nil)
        method-for-nil-request
        (*standard-output* (make-string-output-stream)))
    (unwind-protect
        (progn
          (setf generated-function-name (make-web-handler "no-backslashes" "0-util" "my-utilities")
                method-for-nil-request (defmethod post-parameters ((request null))
                                         '(("str" . "full\\of\\backslashes\\"))))
          (and (eq generated-function-name 'my-utilities::web-no-backslashes)
               (fboundp 'my-utilities::web-no-backslashes)
               (string= (funcall 'my-utilities::web-no-backslashes)
                        "\"full/of/backslashes/\"")))
      (when (fboundp generated-function-name)
        (fmakunbound generated-function-name))
      (remove-method #'post-parameters method-for-nil-request))))
 ("Ensure there is no function bound to web-no-backslashes afterwards. You know, just to be extra sure."
  (not (fboundp 'my-utilities::web-no-backslashes))))