("group-values-by-keys normalizes all values by regrouping them under a canonical key"
 ("Example"
  (equal (group-values-by-keys
          '((:dirs "js/" "css/" "test-ide/")
            (:files "tools/codemirror/lib/codemirror.js"
             "tools/codemirror/lib/codemirror.css"
             "tools/codemirror/mode/commonlisp/commonlisp.js")
            (:files "tools/test-simple/Builder.js" "tools/test-simple/More.js" "tools/test-simple/Harness.js"
             "tools/test-simple/Harness/Browser.js")
            (:prefix web-clear-draft-system web-save-wip web-load-wip web-delete-saved-wip-chunk)
            (:prefix web-load-facts web-save-fact web-delete-fact))
          '(:file :files :dirs)
          '(:prefix))
         '(:PREFIX (web-clear-draft-system web-save-wip web-load-wip web-delete-saved-wip-chunk
                                           web-load-facts web-save-fact web-delete-fact)
           :FILE ("js/" "css/" "test-ide/" "tools/codemirror/lib/codemirror.js"
                        "tools/codemirror/lib/codemirror.css"
                        "tools/codemirror/mode/commonlisp/commonlisp.js"
                        "tools/test-simple/Builder.js" "tools/test-simple/More.js"
                        "tools/test-simple/Harness.js" "tools/test-simple/Harness/Browser.js")))))