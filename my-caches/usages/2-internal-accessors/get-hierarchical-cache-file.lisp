(""
 ("One name (as for root file)"
  (my-equal (get-hierarchical-cache-file (usage-temporary-directory) '("foo"))
            (merge-pathnames "foo.json" (usage-temporary-directory))))
 ("Basic usage"
  (my-equal (get-hierarchical-cache-file (usage-temporary-directory) '("foo" "bar" "baz"))
            (merge-pathnames "foo/bar/baz.json" (usage-temporary-directory))))
 ("Using a getf-query-key list"
  (my-equal (get-hierarchical-cache-file (usage-temporary-directory) '(:alphas "foo" :betas "bar" :gammas "baz"))
            (merge-pathnames "foo/bar/baz.json" (usage-temporary-directory)))))