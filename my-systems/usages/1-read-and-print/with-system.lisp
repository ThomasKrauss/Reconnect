("Simply ensure to bind *package* to the package associated to the given system. Falls back on the draft system if needs be"
 ("Example"
  (equal (with-system ("my-systems")
                      *package*)
         (system-package "my-systems"))
  (equal (with-system (nil)
                      *package*)
         (find-package "CL-USER"))
  (equal (with-system ("I-am-certainly-not-an-existing-system")
                      *package*)
         (find-package "CL-USER"))))