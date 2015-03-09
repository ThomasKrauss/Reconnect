("Give the source directory of the given system."
 ("Example"
  (equal (pathname-directory (system-dispatch-of-chunks "my-systems"))
         '(:absolute "home" "thomas" "dev" "my-systems" "chunks" "dispatch"))))