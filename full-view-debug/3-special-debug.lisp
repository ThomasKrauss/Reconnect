(in-package :full-view-debug)

(define-debugging when
  "The macro when is directly debugged with its macroexpansion since it is so simple."
  (debug-perimeter (macroexpand perimeter) environment))

(define-debugging unless
  "The macro unless is directly debugged with its macroexpansion since it is so simple."
  (debug-perimeter (macroexpand perimeter) environment))

(define-debugging every
  "A call to mapcar is ruthlessly inserted.
Indeed (every #'predicate lst) is equivalent to (every #'not-null (mapcar #'predicate lst)) except that the second one allows us to see the result of having applied the predicate on the list before shrinking all to one boolean through the use of every."
  (if (equal (second perimeter)
             '(function not-null))
    (debug-function-call perimeter environment)
    (debug-perimeter `(every #'not-null
                             (mapcar ,@(rest perimeter)))
                     environment)))