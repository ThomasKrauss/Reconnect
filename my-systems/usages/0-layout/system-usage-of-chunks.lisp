("Give the directory about the usages of the various chunks of the given system."
 ("Example"
  (equal (pathname-directory (system-usage-of-chunks "my-systems"))
         '(:absolute "home" "thomas" "dev" "my-systems" "chunks" "usages"))))