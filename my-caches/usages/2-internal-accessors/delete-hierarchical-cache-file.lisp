(""
 ("Deleting a cache file"
  (let ((path (ensure-directories-exist (get-hierarchical-cache-file (usage-temporary-directory) '("foo" "bar" "baz")))))
    (with-open-file (out path
                         :direction :output)
      (write-string "test" out))
    (and (file-exists-p path)
         (progn
           (delete-hierarchical-cache-file (usage-temporary-directory) '("foo" "bar" "baz"))
           (not (file-exists-p path))))))
 ("Deleting a root cache file"
  (let ((path (ensure-directories-exist (get-hierarchical-cache-file (usage-temporary-directory) '("foo")))))
    (with-open-file (out path
                         :direction :output)
      (write-string "test" out))
    (and (file-exists-p path)
         (progn
           (delete-hierarchical-cache-file (usage-temporary-directory) "foo")
           (not (file-exists-p path)))))))