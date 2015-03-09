("Example"
 (my-equal (system-asd-file "my-systems")
           (merge-pathnames "dev/my-systems/my-systems.asd"
                            *base-dir*)))

