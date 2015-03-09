(in-package :cl)

(defpackage :my-caches (:use :cl :cl-fad :my-utilities :my-systems)
  (:export
   :define-hierarchical-cache
   :update-hierarchical-cache
   :refresh-hierarchical-cache
   :get-from-hierarchical-cache
   :delete-in-hierarchical-cache
   :write-hierarchical-cache
   :update-upper-levels
   :with-hierarchical-cache
   :with-hierarchical-caches
   :get-events
   :get
   :update
   :refresh
   :delete))