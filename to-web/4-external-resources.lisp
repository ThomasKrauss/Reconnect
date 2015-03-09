(in-package :to-web)

(defparameter *external-resources*
  '(("cl-format.js" "formatter")))

(defun find-external-resource (resource-name)
  (awhen (find resource-name *external-resources* :test #'string= :key #'first)
    (system-resource resource-name (second it))))
