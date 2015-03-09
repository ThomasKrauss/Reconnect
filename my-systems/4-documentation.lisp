(in-package :my-systems)

(defun get-symbol-documentation (symbol)
  "Return the documentation of the symbol of given name,
defined in the given system, by default the currently edited one."
  (multiple-value-bind
      (symbol accessibility)
      (find-symbol (string-upcase (symbol-name symbol)) (symbol-package symbol))
    (when (and symbol accessibility
               (not (eq :inherited accessibility)))
      (if (fboundp symbol)
        (documentation symbol 'function)
        (documentation symbol 'variable)))))

(defun set-symbol-documentation (symbol doc-string)
  "Set the documentation string of the symbol of given name to the given doc-string.
The symbol's name must be in uppercase (it will be transformed that way)."
  (multiple-value-bind
      (symbol accessibility)
      (find-symbol (string-upcase (symbol-name symbol)) (symbol-package symbol))
    (when (and symbol accessibility
               (not (eq :inherited accessibility)))
      (if (fboundp symbol)
        (setf (documentation symbol 'function) doc-string)
        (setf (documentation symbol 'variable) doc-string)))))