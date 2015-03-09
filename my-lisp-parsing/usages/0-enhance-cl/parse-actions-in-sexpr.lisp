(""
 ("Parse special operators"
  ("block"
   (multiple-value-bind
       (run-actions install-actions)
       (parse-actions-in-sexpr '(block empty))
     (and (list= '(block) run-actions)
          (null install-actions)))
   (multiple-value-bind
       (run-actions install-actions)
       (parse-actions-in-sexpr '(block whocares (values 1 2) (values 3 4)))
     (and (list= '(block values values) run-actions)
          (null install-actions))))
  ("catch"
   (multiple-value-bind
       (run-actions install-actions)
       (parse-actions-in-sexpr '(catch 'dummy-tag 1 2 (+ 3 3) 4 (* 5 5)))
     (and (list= '(catch quote + *) run-actions)
          (null install-actions))))
  ("eval-when"
   (multiple-value-bind
       (run-actions install-actions)
       (parse-actions-in-sexpr '(eval-when (:compile-toplevel) 
                       (eval-when (:execute)
                         (print 'foo5))))
     (and (list= '(eval-when eval-when quote print) run-actions)
          (null install-actions))))
  ("flet"
   (multiple-value-bind
       (run-actions install-actions)
       (parse-actions-in-sexpr '(flet ((dummy-function () 'shadow))
                       (+ 1 1)
                       (dummy-function)))
     (and (list= '(flet + quote) run-actions)
          (null install-actions)))
   (multiple-value-bind
       (run-actions install-actions)
       (parse-actions-in-sexpr '(flet ((flet1 (n) (+ n n)))
                       (flet ((flet1 (n) (* 2 (flet1 n))))
                         (flet1 2))))
     (and (list= '(flet flet + *) run-actions)
          (null install-actions)))
   (multiple-value-bind
       (run-actions install-actions)
       (parse-actions-in-sexpr '(flet ((my-add (m n) (+ m n))
                            (my-multiply (m n) (* m n)))
                       (my-add 1 (my-multiply 2 3))))
     (and (list= '(flet + *) run-actions)
          (null install-actions))))
  ("function"
   (multiple-value-bind
       (run-actions install-actions)
       (parse-actions-in-sexpr '(function +))
     (and (list= '(function +) run-actions)
          (null install-actions))))
  ("go"
   (multiple-value-bind
       (run-actions install-actions)
       (parse-actions-in-sexpr '(go tag))
     (and (list= '(go) run-actions)
          (null install-actions))))
  ("if"
   (multiple-value-bind
       (run-actions install-actions)
       (parse-actions-in-sexpr '(if (= 1 (/ 2 2)) (print 'true) (format t "~s" 'false)))
     (and (list= '(if = / print quote quote format) run-actions)
          (null install-actions))))
  ("labels"
   (multiple-value-bind
       (run-actions install-actions)
       (parse-actions-in-sexpr '(labels ((dummy-function () 'shadow))
                       (+ 1 1)
                       (dummy-function)))
     (and (list= '(labels + quote) run-actions)
          (null install-actions)))
   (multiple-value-bind
       (run-actions install-actions)
       (parse-actions-in-sexpr '(labels ((flet1 (n) (+ n n)))
                       (labels ((flet1 (n) (* 2 (flet1 n))))
                         (flet1 2))))
     (and (list= '(labels labels + *) run-actions)
          (null install-actions)))
   (multiple-value-bind
       (run-actions install-actions)
       (parse-actions-in-sexpr '(labels ((my-add (m n) (+ m n))
                              (my-multiply (m n) (* m n)))
                       (my-add 1 (my-multiply 2 3))))
     (and (list= '(labels + *) run-actions)
          (null install-actions)))
   (multiple-value-bind
       (run-actions install-actions)
       (parse-actions-in-sexpr '(labels ((flet1 (n) (+ n n))
                              (flet2 (n) (* 2 (flet1 n))))
                       (flet2 2)))
     (and (list= '(labels + *) run-actions)
          (null install-actions))))
  ("let"
   (multiple-value-bind
       (run-actions install-actions)
       (parse-actions-in-sexpr '(let ((a (+ m n))
                           b
                           (c)
                           (d (* m n)))
                       (max 1 1)
                       (/ a d)))
     (and (list= '(let + * / max) run-actions)
          (null install-actions))))
  ("let*"
   (multiple-value-bind
       (run-actions install-actions)
       (parse-actions-in-sexpr '(let ((a (+ m n))
                           b
                           (c)
                           (d (* m n)))
                       (max 1 1)
                       (/ a d)))
     (and (list= '(let + * / max) run-actions)
          (null install-actions))))
  ("load-time-value"
   (multiple-value-bind
       (run-actions install-actions)
       (parse-actions-in-sexpr '(load-time-value (random 17) (null foo)))
     (and (list= '(load-time-value random null) run-actions)
          (null install-actions))))
  ("locally"
   (multiple-value-bind
       (run-actions install-actions)
       (parse-actions-in-sexpr '(locally
                       (declare (inline floor) (notinline car cdr))
                       (declare (optimize space))
                       (+ 1 1)
                       (floor (car x) (cdr y))))
     (and (list= '(locally + floor car cdr) run-actions)
          (null install-actions))))
  ("macrolet"
   (multiple-value-bind
       (run-actions install-actions)
       (parse-actions-in-sexpr '(macrolet ((dummy-macro () 'shadow))
                       (+ 1 1)
                       (dummy-macro)))
     (and (list= '(macrolet + quote) run-actions)
          (null install-actions)))
   (multiple-value-bind
       (run-actions install-actions)
       (parse-actions-in-sexpr '(macrolet ((macrolet1 (n) `(+ ,n ,n)))
                       (macrolet1 2)))
     (and (list= '(macrolet + list quote) run-actions)
          (null install-actions)))
   (multiple-value-bind
       (run-actions install-actions)
       (parse-actions-in-sexpr '(macrolet ((my-add (m n) `(+ ,m ,n))
                                (my-multiply (m n) `(* ,m ,n)))
                       (my-add 1 (my-multiply 2 3))))
     (and (list= '(macrolet + * list quote list quote) run-actions)
          (null install-actions)))
   (multiple-value-bind
       (run-actions install-actions)
       (parse-actions-in-sexpr '(macrolet ((macrolet1 (n) (if (and (numberp n)
                                                        (= 0 n))
                                                 0
                                                 `(+ ,n ,n))))
                       (macrolet1 2)))
     (and (list= '(macrolet list if if quote = numberp +) run-actions)
          (list= '(and and) install-actions)))
   (multiple-value-bind
       (run-actions install-actions)
       (parse-actions-in-sexpr '(macrolet ((macrolet1 (n) (if (and (numberp n)
                                                        (= 0 n))
                                                 0
                                                 `(+ ,n ,n))))
                       (macrolet1 0)))
     (and (list= '(macrolet list if if quote = numberp) run-actions)
          (list= '(and and) install-actions)))
   (multiple-value-bind
       (run-actions install-actions)
       (parse-actions-in-sexpr '(macrolet ((macrolet1 (n) `(+ ,n ,n)))
                       (macrolet ((macrolet1 (n) `(* ,n ,(macrolet1 n))))
                         (macrolet1 2))))
     (and (list= '(macrolet + + * list quote list quote) run-actions)
          (null install-actions))))
  ("multiple-value-call"
   (multiple-value-bind
       (run-actions install-actions)
       (parse-actions-in-sexpr '(multiple-value-call #'+ (floor 5 3) (ceiling 19 4)))
     (and (list= '(multiple-value-call function + floor ceiling) run-actions)
          (null install-actions))))
  ("multiple-value-prog1"
   (multiple-value-bind
       (run-actions install-actions)
       (parse-actions-in-sexpr '(multiple-value-prog1
                         (values-list temp)
                       (+ 1 1)
                       (null temp)))
     (and (list= '(multiple-value-prog1 values-list + null) run-actions)
          (null install-actions))))
  ("progn"
   (multiple-value-bind
       (run-actions install-actions)
       (parse-actions-in-sexpr '(progn
                       (+ 1 1)
                       (floor 5 3)
                       (ceiling 19 4)))
     (and (list= '(progn + floor ceiling) run-actions)
          (null install-actions))))
  ("progv"
   (multiple-value-bind
       (run-actions install-actions)
       (parse-actions-in-sexpr '(progv (list '*x* (make-symbol "test")) (list (1+ 3) (1- 3))
                       (list *x* (symbol-value '*x*) (* 2 test))))
     (and (list= '(progv list list list make-symbol 1+ 1- symbol-value * quote quote) run-actions)
          (null install-actions))))
  ("quote"
   (multiple-value-bind
       (run-actions install-actions)
       (parse-actions-in-sexpr '(quote (+)))
     (and (list= '(quote) run-actions)
          (null install-actions))))
  ("return-from"
   (multiple-value-bind
       (run-actions install-actions)
       (parse-actions-in-sexpr '(return-from alpha (values 1 2)))
     (and (list= '(return-from values) run-actions)
          (null install-actions))))
  ("setq"
   (multiple-value-bind
       (run-actions install-actions)
       (parse-actions-in-sexpr '(setq a (1+ b) b (* a 2) c (+ a b)))
     (and (list= '(setq 1+ * +) run-actions)
          (null install-actions))))
  ("symbol-macrolet"
   (multiple-value-bind
       (run-actions install-actions)
       (parse-actions-in-sexpr '(symbol-macrolet ((dummy-macro 'shadow))
                       (+ 1 1)
                       dummy-macro))
     (and (list= '(symbol-macrolet + quote) run-actions)
          (null install-actions)))
   (multiple-value-bind
       (run-actions install-actions)
       (parse-actions-in-sexpr '(symbol-macrolet ((macrolet1 (+ 1 2)))
                       macrolet1))
     (and (list= '(symbol-macrolet +) run-actions)
          (null install-actions)))
   (multiple-value-bind
       (run-actions install-actions)
       (parse-actions-in-sexpr '(symbol-macrolet ((my-add (+ 1 2))
                                       (my-multiply (* 1 2)))
                       my-add
                       my-multiply))
     (and (list= '(symbol-macrolet + *) run-actions)
          (null install-actions)))
   (multiple-value-bind
       (run-actions install-actions)
       (parse-actions-in-sexpr '(symbol-macrolet ((macrolet1 (if (and (numberp 1)
                                                           (= 0 1))
                                                    0
                                                    (+ 1 1))))
                       macrolet1))
     (and (list= '(symbol-macrolet if if = numberp +) run-actions)
          (list= '(and and) install-actions)))
   (multiple-value-bind
       (run-actions install-actions)
       (parse-actions-in-sexpr '(symbol-macrolet ((macrolet1 (if (and (numberp 0)
                                                           (= 0 0))
                                                    0
                                                    (+ 0 0))))
                       macrolet1))
     (and (list= '(symbol-macrolet if if = numberp +) run-actions)
          (list= '(and and) install-actions)))
   (multiple-value-bind
       (run-actions install-actions)
       (parse-actions-in-sexpr '(symbol-macrolet ((macrolet1 (+ 1 1)))
                       (symbol-macrolet ((macrolet2 (* 2 macrolet1)))
                         macrolet2)))
     (and (list= '(symbol-macrolet symbol-macrolet + *) run-actions)
          (null install-actions))))
  ("tagbody"
   (multiple-value-bind
       (run-actions install-actions)
       (parse-actions-in-sexpr '(tagbody
                      (+ val 1)
                      (go point-a)
                      (* val 16)
                      point-c
                      (/ val 04)
                      (go point-b)
                      (floor val 32)
                      point-a
                      (ceiling val 02)
                      (go point-c)
                      (1+ val 64)
                      point-b
                      (1- val 08)))
     (and (list= '(tagbody + go go go * / floor ceiling 1+ 1-) run-actions)
          (null install-actions))))
  ("the"
   (multiple-value-bind
       (run-actions install-actions)
       (parse-actions-in-sexpr '(the (values) (truncate 3.2 2)))
     (and (list= '(the truncate) run-actions)
          (null install-actions))))
  ("throw"
   (multiple-value-bind
       (run-actions install-actions)
       (parse-actions-in-sexpr '(throw (make-symbol "result") (values i j)))
     (and (list= '(throw make-symbol values) run-actions)
          (null install-actions))))
  ("unwind-protect"
   (multiple-value-bind
       (run-actions install-actions)
       (parse-actions-in-sexpr '(unwind-protect (+ 1 1) (* 2 2) (/ 3 3) (- 4 4)))
     (and (list= '(unwind-protect + * / -) run-actions)
          (null install-actions)))))
 ("Shadowing functions and macros"
  (multiple-value-bind
      (run-actions install-actions)
      (parse-actions-in-sexpr '(flet ((+ (x y)
                             (* x y)))
                      (+ 2 3)))
    (and (list= '(flet *) run-actions)
         (null install-actions)))
  (multiple-value-bind
      (run-actions install-actions)
      (parse-actions-in-sexpr '(labels ((+ (x y)
                               (* x y)))
                      (+ 2 3)))
    (and (list= '(labels *) run-actions)
         (null install-actions)))
  (multiple-value-bind
      (run-actions install-actions)
      (parse-actions-in-sexpr '(macrolet ((+ (x y)
                                 `(* ,x ,y)))
                      (+ 2 3)))
    (and (list= '(macrolet quote list *) run-actions)
         (null install-actions)))
  ("Precedence between flet, labels and macrolet"
   (multiple-value-bind
       (run-actions install-actions)
       (parse-actions-in-sexpr '(flet ((+ (x y)
                              (* x y)))
                       (macrolet ((+ (x y)
                                    `(- ,x ,y)))
                         (+ 2 3))))
     (and (list= '(flet macrolet quote list * -) run-actions)
          (null install-actions)))
   (multiple-value-bind
       (run-actions install-actions)
       (parse-actions-in-sexpr '(macrolet ((+ (x y)
                                  `(- ,x ,y)))
                       (flet ((+ (x y)
                                (* x y)))
                         (+ 2 3))))
     (and (list= '(flet quote list macrolet *) run-actions)
          (null install-actions)))
   (multiple-value-bind
       (run-actions install-actions)
       (parse-actions-in-sexpr '(labels ((+ (x y)
                                (* x y)))
                       (macrolet ((+ (x y)
                                    `(- ,x ,y)))
                         (+ 2 3))))
     (and (list= '(labels quote list macrolet * -) run-actions)
          (null install-actions)))
   (multiple-value-bind
       (run-actions install-actions)
       (parse-actions-in-sexpr '(macrolet ((+ (x y)
                                  `(- ,x ,y)))
                       (labels ((+ (x y)
                                  (* x y)))
                         (+ 2 3))))
     (and (list= '(labels quote list macrolet *) run-actions)
          (null install-actions))))
  ("No infinite loop using the whole parameter in macrolet"
   (multiple-value-bind
       (run-actions install-actions)
       (parse-actions-in-sexpr '(macrolet ((rec (&whole whole &body body)
                                  `(macrolet ((rec (&body body)
                                                `(+ ,@body)))
                                     ,whole)))
                       (rec 1 2 3 4 5)))
     (and (list= '(macrolet macrolet quote quote quote list cons +) run-actions)
          (null install-actions))))))