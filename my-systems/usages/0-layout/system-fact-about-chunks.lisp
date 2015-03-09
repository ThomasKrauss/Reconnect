("Give the directory about the facts of the given system."
 ("Example"
  (equal (pathname-directory (system-fact-about-chunks "my-systems"))
         '(:absolute "home" "thomas" "dev" "my-systems" "chunks" "facts"))))