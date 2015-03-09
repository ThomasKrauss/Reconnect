("Give the chunks directory of the given system, for information about the various chunks."
 ("Example"
  (equal (pathname-directory (system-chunks "my-systems"))
         '(:absolute "home" "thomas" "dev" "my-systems" "chunks"))))