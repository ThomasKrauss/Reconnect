(in-package :my-caches)

; hierarchical cache
; Defined by keys which denote the types of each thing to cache.
; The given order of the keys is the one used for the hierarchy.
; The data is entirely determined by the definitions given, except for one thing: each data must have a name.
;
; => Constraints:
; 1) A :root key must always be specified and associated to it, a way of getting all the names of the top level thing
; 2) There must exist a link from level n to level n+1 where a name of any thing at level n return all the names of the things of level n+1 that are associated to it. It is expected to be an accessible function called <key-level(n)>-<key-level-(n+1)>-names.
; Ex: Let keys = '(:system :module :action)
;     1) There must exist a :root key with a form associated to it that return all the targeted system names.
;     2) There must exist a function system-module-names of one argument, system-name, returning all the module-names of the system of given name. And there must exist a function module-action-names working the same way and linking a module and all its associated actions by name.
;
; A hierarchial cache provides 3 processes for interfacing: get, refresh and delete, and 1 for externalizing: write
;
; => Interfacing processes return events about how the cache changed and based on them, we can select what data is needed to be written to external files.
;
; Externalization: can be associated to any key while defining the hierarchy. But it is not mandatory.
; In such case, the key can stand on its own and no writing method will be associated for the thing at that level.
; For instance, the count of lines and forms is defined at the level of the module but the externalization happens at the system level only. The external files packs all the statistics of a whole system.
; If a writing process needs to be associated to a certain level of the cache, a list must be provided and hold the key denoting that level as its first element, the rest being the various writing processes.
; These processes are functions but for convenience can also just be the true symbol. That special syntax means the data is passed as is through the JSON <- Plist transformers. Otherwise, when a function is given, the data is first preprocessed by it before translation to JSON.
;
; => Constraints:
; 1) The writing processes are only about data transformation, they are not choosing where to write (although it is not enforced per se). Each file will be written in a given directory.
; 2) The organization in that output directory is thusly:
; For a writer at level 1, the output file is: <output-directory>/<name-of-thing-level(1)>.json
; For a writer at level 2, the output file is: <output-directory>/<name-of-thing-level(1)>/<name-of-thing-level(2)>.json
; And so on.
; 3) As several writers can be associated to a given level, several output directories must be provided. Their total number must matched the maximum number of writers to be used among all levels of the cache.
; Ex: If there are 2 writers associated to the :system level, none for the :module level and 3 for the :action level, then 3 output directories must be provided. They will be used in that order, that is the first writers of each level will use the first output directory, the second writers will use the second output directory and so on. Therefore, from the given example, the third output directory will only be used at the :action level, by the third writer associated to that level.
; 4) This previous constraint means that when several writers are using the same output directory, the data they return must be related. The data returned at each upper level is just some sort of summary of the data returned at each lower level.
; 5) As a way to respect that, the list of writers can contain nil to denote that no particular writer is associated to the output directory of same index. While it is not mandatory to specify nil, that is if, for instance, 3 ouput directories need to be defined as per constraint 3 but for a given level, only the first one needs to be written to, the list of writer can just be (:key writer) instead of (:key writer nil nil). This syntax is provided as a way to better associate the appropriate writers to the appropriate directories.
; Ex: Say the configuration of writers is like this:
; - 2 for :system, 2 for :module, 3 for :action
; - :system writerA <-> :action writerA & :system writerB <-> :action writerB
;
; At this point, you would list the output directories to match the order of the :action :writers, (writerA writerB writerC) and then the :system writerA should be listed first too, and the :system writerB should come in the second place
;
; However, you have that further constraint:
; - :module writerA <-> :action writerA & :module writerB <-> :action writerC
;
; The list of output directory can still have the first element as before, since all writerA are linked together. However, for the last 2, there is a mismatch. The writerB of :system and :module share nothing together.
;
; Hence allowing nil in the list of writers to allow the two following configurations that solved this situation:
; (directoryA directoryB directoryC) (:system writerA writerB) (:module writerA nil writerB) (:action writerA writerB writerC)
; (directoryA directoryC directoryB) (:system writerA nil writerB) (:module writerA writerB) (:action writerA writerC writerB)
;
; Querying:
; A query is: key-or-name*.
; The result of a query is either a named plist, that is a plist with a property :name, if you have asked for a specific data at a given level. Additionally, you also have a property named <key-next-level> (in plural form) holding the data of all the related thing of the next level. And then, all the other properties you wanted for that specific thing.
; Or it's a list if you have asked for the whole data of a level.
;
; Ex:
; Let keys = '(:system :module :action)
;
; Searching for the data (specific data + data of all related modules) of the system "foo"
; query = '("foo")
; => system-data = '(:name "foo" :modules <module-data>* &rest <specific-system-data>)
;
; Searching for the data of the module "bar" of system "foo"
; query = '("foo" "bar")
; => module-data = '(:name "bar" :actions <action-data>* &rest <specific-module-data>)
;
; Searching for all modules of system "foo"
; query = '("foo" :modules)
; => <module-data>* of system "foo"
;
; Searching for the specific data :test of system "foo"
; query = '("foo" :test)
; => value of property :test in the system-data of system "foo"
;
; Searching for all systems
; query = '(:systems)
; => <system-data>*
;
;
; Refreshing and deleting:
; Only named data.
;
; Ex: query = '("foo" "bar") or '("foo")
; Never '("foo" :modules) or (:systems).

(defun key-hierarchy (key key-list &key (test 'eql))
  "Get the keys of the cache of given name up to the given key."
  (awhen (position key key-list :test (if (functionp test) test (symbol-function test)))
    (subseq key-list 0 (1+ it))))

(defun key-getf-hierarchy (key key-list)
  (loop for previous-key in (key-hierarchy key key-list)
        collect :in
        collect (key-name previous-key)))

(defun append-as-list (&rest args)
  "Utility for list manipulation."
  (append (first args)
          (loop for lst in (rest args)
                when lst
                collect lst)))

(defun key-name-hierarchy (key key-list)
  "Get the key names of the hierarchy of keys."
  (mapcar #'key-name (key-hierarchy key key-list)))

(defun to-getf-keys (names keys)
  (declare (ignore keys))
  (loop for name in names
        collect :in
        collect name))

(defun complete-query (query)
  (let ((last-element (let-a (first (last query))
                        (when (keywordp it) it))))
    (aif (append (loop for name-or-keyword in (if last-element (butlast query) query)
                       collect :in
                       collect name-or-keyword)
                 (awhen last-element (list it)))
         it
         (list :in))))