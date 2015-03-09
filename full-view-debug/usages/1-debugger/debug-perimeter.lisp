("debug-perimeter, the environment is always returned. In case of some errors occurred, the state of the bindings at the time of that error are returned."
 ("Atoms"
  ("Verbatim values (which evaluate to themselves)"
   (values-equal (debug-perimeter nil 'env)
                 '(nil) 'env nil)
   (values-equal (debug-perimeter t 'env)
                 '(t) 'env nil)
   (values-equal (debug-perimeter :test 'env)
                 '(:test) 'env nil)
   (values-equal (debug-perimeter 1 'env)
                 '(1) 'env nil)
   (values-equal (debug-perimeter #\a 'env)
                 '(#\a) 'env nil)
   (values-equal (debug-perimeter "test" 'env)
                 '("test") 'env nil))
  ("Symbols: quote operator handled specially"
   (values-equal (debug-perimeter ''test 'env)
                 '(test) 'env nil))
  ("Function: like a verbatim value"
   (values-equal (debug-perimeter (function +) 'env)
                 (list #'+) 'env nil))
  ("Variable"
   (values-equal (debug-perimeter 'test '((let ((test 1)))))
                 '(1 test) '((let ((test 1)))) nil)
   ("Side-effect"
    ("With special operator setq"
     (values-equal (debug-perimeter '(setq test 5) '((let ((test 0)))))
                   '(5 (setq ((test (5)))))
                   '((let ((test 5)))) nil))
    ("With a macro"
     (values-equal (debug-perimeter '(incf test) '((let ((test 0)))))
                   '(1 nil ((incf test)))
                   '((let ((test 1)))) nil)))))
 ("List: non-quoted as result"
  (values-equal (debug-perimeter ''(1 2) 'env)
                '((1 2)) 'env nil)
  (values-equal (debug-perimeter '(list 1 2) '((let ((x 1)))))
                '((1 2) list (1) (2)) '((let ((x 1)))) nil))
 ("Values: wrapped in a call to values-list"
  (values-equal (debug-perimeter '(values 1 2) '((let ((x 1)))))
                '((VALUES-LIST (QUOTE (1 2))) VALUES (1) (2))
                '((let ((x 1)))) nil))
 ("Function calls"
  ("Simple call"
   (values-equal (debug-perimeter '(substitute #\/ #\\ "test/pour\\voir") '((let ((x 1)))))
                 '("test/pour/voir" substitute (#\/) (#\\) ("test/pour\\voir"))
                 '((let ((x 1)))) nil)
   ("No argument: nil explicitely written to differentiate such a function call from a binding"
    (values-equal (debug-perimeter '(+) nil)
                  '(0 + nil)
                  nil nil))
   ("Function values ditched from the arguments"
    (values-equal (debug-perimeter '(find "test" nil :test #'string= :key (lambda (x) (first (second x)))) nil)
                  '(nil find ("test") (nil)
                        (:test) ((:function string=))
                        (:key) (:anonymous-function nil ((lambda (x) (first (second x))))))
                  nil nil))
   ("Side-effect"
    (values-equal (debug-perimeter '(+ (incf test) 1) '((let ((test 0)))))
                  '(2 + (1 nil ((incf test))) (1))
                  '((let ((test 1)))) nil))
   ("Evaluate arguments from left to right"
    (values-equal (debug-perimeter '(+ (incf x y) (incf y x)) '((let ((x 1) (y 10)))))
                  '(32 + (11 NIL ((INCF X Y))) (21 NIL ((INCF Y X))))
                  '((let ((X 11) (Y 21)))) nil))
   ("No multiple evaluations"
    (values-equal (debug-perimeter '(+ (incf test) (incf test)) '((let ((test 0)))))
                  '(3 + (1 NIL ((INCF TEST))) (2 NIL ((INCF TEST))))
                  '((let ((test 2)))) nil))
   ("Forward multiple values"
    (values-equal (debug-perimeter '(floor (/ 2 3)) '((let ((x 1)))))
                  '((VALUES-LIST (QUOTE (0 2/3))) FLOOR (2/3 / (2) (3)))
                  '((let ((x 1)))) nil)))
  ("Chained calls in simple perimeters"
   (values-equal (debug-perimeter '(+ 1 (* 2 (- 3 4))) '((let ((x 1)))))
                 '(-1 + (1) (-2 * (2) (-1 - (3) (4))))
                 '((let ((x 1)))) nil)
   ("Preserve list values"
    (values-equal (debug-perimeter '(list (list 1 2) 3 4) '((let ((x 1)))))
                  '(((1 2) 3 4) LIST ((1 2) LIST (1) (2)) (3) (4))
                  '((let ((x 1)))) nil))
   ("Use automatically the first value in multiple values"
    (values-equal (debug-perimeter '(+ 1 (floor (/ 2 3))) nil)
                  '(1 + (1) ((VALUES-LIST (QUOTE (0 2/3))) FLOOR (2/3 / (2) (3))))
                  nil nil)))
  ("Chained calls in general perimeters"
   (values-equal (debug-perimeter '(= (+ 1 (* 2 3)) (+ 3 (* 2 2))) '((let ((x 1)))))
                 '(t =
                     (7 + (1)
                        (6 * (2) (3)))
                     (7 + (3)
                        (4 * (2) (2))))
                 '((let ((x 1)))) nil))
  ("Destructive function"
   (values-equal (debug-perimeter '(nreverse test) '((let ((test '(1 2 3))))))
                 '((3 2 1) nreverse ((1 2 3) test))
                 '((let ((test '(1)))))
                 nil))
  ("Function value"
   (values-equal (debug-perimeter '(mapcar #'1+ (list 1 2 3)) '((let ((test '(1 2 3))))))
                 `((2 3 4) mapcar ((:function 1+)) ((1 2 3) list (1) (2) (3)))
                 '((let ((test '(1 2 3)))))
                 nil)))
 ("Function calls targeting local functions defined in the environment"
  ("With flet"
   (values-equal (debug-perimeter '(+ (foo 2) (bar 2)) '((flet ((foo (x) (* x 2)) (bar (x) (* x 3))))))
                 '(10 + (4 foo (2)) (6 bar (2)))
                 '((flet ((foo (x) (* x 2)) (bar (x) (* x 3)))))
                 nil))
  ("With labels"
   (values-equal (debug-perimeter '(+ (baz 2) (bulu 2)) '((labels ((baz (x) (+ x 2))
                                                                   (bulu (x) (* (baz x)
                                                                                (if (< x 10)
                                                                                  (bulu (+ 10 x))
                                                                                  3)))))))
                 '(172 + (4 BAZ (2)) (168 BULU (2)))
                 '((labels ((baz (x) (+ x 2))
                            (bulu (x) (* (baz x)
                                         (if (< x 10)
                                           (bulu (+ 10 x))
                                           3))))))
                 nil))
  ("With both"
   (values-equal (debug-perimeter '(+ (foo 2) (bar 2) (baz 2) (bulu 2))
                                  '((flet ((foo (x) (* x 2)) (bar (x) (* x 3))))
                                    (labels ((baz (x) (+ x 2))
                                             (bulu (x) (* (baz x)
                                                          (if (< x 10)
                                                            (bulu (+ 10 x))
                                                            3)))))))
                 '(182 + (4 foo (2)) (6 bar (2)) (4 baz (2)) (168 bulu (2)))
                 '((flet ((foo (x) (* x 2)) (bar (x) (* x 3))))
                                    (labels ((baz (x) (+ x 2))
                                             (bulu (x) (* (baz x)
                                                          (if (< x 10)
                                                            (bulu (+ 10 x))
                                                            3))))))
                 nil))
  ("With no local function defined"
   (values-equal (debug-perimeter '(foo 2) nil)
                 '(nil :error foo (2))
                 nil
                 '("Undefined operator FOO in form (FOO 2)."))))
 ("Lambda in place of function name"
  (values-equal (debug-perimeter '((lambda (x) (+ x 3)) 5) '((let ((y 1)))))
                '(8 ((lambda (x) (+ x 3)) ((x 5)) (8 + (5 x) (3))) (5))
                '((let ((y 1)))) nil)
  ("Side-effect"
   ("From lambda arguments"
    (values-equal (debug-perimeter '((lambda (x) (+ x 3)) (incf test)) '((let ((test 1)))))
                  '(5 ((LAMBDA (X) (+ X 3)) ((X 2)) (5 + (2 X) (3))) (2 NIL ((INCF TEST))))
                  '((let ((TEST 2)))) nil))
   ("From lambda body"
    (values-equal (debug-perimeter '((lambda (x) (incf test x)) 3) '((let ((test 1)))))
                  '(4 ((LAMBDA (X) (INCF TEST X)) ((X 3)) (4 NIL ((INCF TEST X)))) (3))
                  '((let ((TEST 4)))) nil))
   ("Evaluate arguments from left to right"
    (values-equal (debug-perimeter '((lambda (a b) (+ a b)) (incf x y) (incf y x)) '((let ((x 1) (y 10)))))
                  '(32 ((lambda (a b) (+ a b)) ((a 11) (b 21)) (32 + (11 a) (21 b)))
                       (11 NIL ((INCF X Y))) (21 NIL ((INCF Y X))))
                  '((let ((X 11) (Y 21)))) nil))
   ("No multiple evaluations"
    (values-equal (debug-perimeter '((lambda (a b) (+ a b)) (incf test) (incf test)) '((let ((test 0)))))
                  '(3 ((lambda (a b) (+ a b)) ((a 1) (b 2)) (3 + (1 a) (2 b)))
                      (1 NIL ((INCF TEST))) (2 NIL ((INCF TEST))))
                  '((let ((test 2)))) nil)))
  ("Preserve list values"
   (values-equal (debug-perimeter '((lambda (x) (list x 3 4)) (list 1 2)) '((let ((x 1)))))
                 '(((1 2) 3 4)
                   ((lambda (x) (list x 3 4)) ((x (quote (1 2)))) (((1 2) 3 4) LIST ((1 2) x) (3) (4)))
                   ((1 2) list (1) (2)))
                 '((let ((x 1)))) nil))
  ("Forward multiple values"
   (values-equal (debug-perimeter '((lambda (x) (floor x)) (/ 2 3)) '((let ((x 1)))))
                 '((VALUES-LIST (QUOTE (0 2/3)))
                   ((lambda (x) (floor x)) ((x 2/3)) ((VALUES-LIST (QUOTE (0 2/3))) floor (2/3 x)))
                   (2/3 / (2) (3)))
                 '((let ((x 1)))) nil))
  ("Function values ditched from the arguments"
   (values-equal (debug-perimeter '((lambda (x y) (funcall x "test" (funcall y '(1 ("test" 2)))))
                                    #'string= (lambda (x) (first (second x))))
                                  nil)
                 '(t
                   ((lambda (x y) (funcall x "test" (funcall y '(1 ("test" 2)))))
                    ((x (:function string=))
                     (y :anonymous-function))
                    (t funcall ((:function string=) x)
                       ("test")
                       ("test" funcall (:anonymous-function y) ((1 ("test" 2))))))
                   ((:function string=))
                   (:anonymous-function nil ((lambda (x) (first (second x))))))
                 nil nil))
  ("Implicit progn in lambda form"
   (values-equal (debug-perimeter '((lambda (x y) (- x y) (+ x y)) 10 11) '((let ((x 1)))))
                 '(21 ((LAMBDA (X Y) (- X Y) (+ X Y))
                       ((X 10) (Y 11))
                       (21 PROGN (-1 - (10 X) (11 Y)) (21 + (10 X) (11 Y)))) (10) (11))
                 '((let ((x 1)))) nil))
  ("Shadowing"
   (values-equal (debug-perimeter '((lambda (x test) (+ x test)) 10 11) '((let ((test 1)))))
                 '(21 ((LAMBDA (X test) (+ X test)) ((X 10) (test 11)) (21 + (10 X) (11 test))) (10) (11))
                 '((let ((TEST 1)))) nil)
   (values-equal (debug-perimeter '((lambda (x test) (incf test) (+ x test)) 10 11) '((let ((test 1)))))
                 '(22 ((LAMBDA (X TEST) (INCF TEST) (+ X TEST))
                       ((X 10) (TEST 11))
                       (22 PROGN (12 NIL ((INCF TEST))) (22 + (10 X) (12 TEST)))) (10) (11))
                 '((let ((TEST 1)))) nil)))
 ("Special operators"
  ("Directly evaluated: catch eval-when go load-time-value locally multiple-value-call multiple-value-prog1 progv return-from tagbody the throw unwind-protect"
   ("block"
    (values-equal (debug-perimeter '(block test (if (= 1 2) 7 (return-from test))) '((let ((x 1)))))
                  '(nil nil ((block test (if (= 1 2) 7 (return-from test)))))
                  '((let ((x 1)))) nil)))
  ("quote"
   (values-equal (debug-perimeter ''test 'env)
                 '(test) 'env nil)
   (values-equal (debug-perimeter ''(1 2) 'env)
                 '((1 2)) 'env nil)
   (values-equal (debug-perimeter '(eq 'test 'test) '((let ((x 1)))))
                 '(t eq (test) (test)) '((let ((x 1)))) nil)
   (values-equal (debug-perimeter '(let ((x 'test)) (symbol-name x)) '((let ((x 1)))))
                 '("TEST" (let ((x (test)))
                            ("TEST" symbol-name (test x))))
                 '((let ((x 1)))) nil))
  ("function"
   (values-equal (debug-perimeter '(function +) '((let ((x 1)))))
                 (list (function +)) '((let ((x 1)))) nil))
  ("progn"
   (values-equal (debug-perimeter '(progn (+ 1 2) (* 3 4)) '((let ((x 1)))))
                 '(12 progn (3 + (1) (2)) (12 * (3) (4))) '((let ((x 1)))) nil)
   ("Forward multiple values"
    (values-equal (debug-perimeter '(progn (+ 1 2) (floor (/ 2 3))) '((let ((x 1)))))
                  '((VALUES-LIST (QUOTE (0 2/3))) progn
                    (3 + (1) (2))
                    ((VALUES-LIST (QUOTE (0 2/3))) FLOOR (2/3 / (2) (3))))
                  '((let ((x 1)))) nil))
   ("Ditch all function values from the body of progn"
    (values-equal (debug-perimeter '(progn (lambda (x) (first (second x))) #'string=) nil)
                  `(,#'string= progn
                               (:anonymous-function nil ((lambda (x) (first (second x)))))
                               ((:function string=)))
                  nil nil)))
  ("if"
   (values-equal (debug-perimeter '(if (eq 'test 'test) (+ 1 1) (+ 2 2)) '((let ((x 1)))))
                 '(2 IF (T EQ (TEST) (TEST)) (2 + (1) (1)))
                 '((let ((x 1)))) nil)
   (values-equal (debug-perimeter '(if (eq 'test 'toto) (+ 1 1) (+ 2 2)) '((let ((x 1)))))
                 '(4 IF (nil EQ (TEST) (TOTO)) (4 + (2) (2)))
                 '((let ((x 1)))) nil)
   ("Forward multiple values"
    (values-equal (debug-perimeter '(if (eq 'test 'test) (values 1 1) (+ 2 2)) '((let ((x 1)))))
                 '((VALUES-LIST (QUOTE (1 1))) IF (T EQ (TEST) (TEST)) ((VALUES-LIST (QUOTE (1 1))) VALUES (1) (1)))
                 '((let ((x 1)))) nil))
   ("Use automatically the first value of multiple values"
    (values-equal (debug-perimeter '(if (values (eq 'test 'test) nil) (+ 1 1) (+ 2 2)) '((let ((x 1)))))
                 '(2 IF ((VALUES-LIST (QUOTE (T NIL))) VALUES (T EQ (TEST) (TEST)) (NIL)) (2 + (1) (1)))
                 '((let ((x 1)))) nil))
   ("Ditch function values for the test clause and the executed clause"
    (values-equal (debug-perimeter '(if (lambda (x) (first (second x))) #'string= (+ 1 2)) nil)
                  `(,#'string= if (:anonymous-function nil ((lambda (x) (first (second x)))))
                               ((:function string=)))
                  nil nil)))
  ("setq"
   (values-equal (debug-perimeter '(setq a 1 b 2 c 3)
                                  '((let (a b c))))
                 '(3 (SETQ ((A (1)) (B (2)) (C (3)))))
                 '((let ((A 1) (B 2) (C 3)))) nil)
   (values-equal (debug-perimeter '(setq y (incf x) z (incf x))
                                  '((let ((x 1) (y 0) z))))
                 '(3 (setq ((Y (2 NIL ((INCF X))))
                            (Z (3 NIL ((INCF X)))))))
                 '((let ((x 3) (y 2) (z 3)))) nil)
   (values-equal (debug-perimeter '(setq y (incf x)
                                         z (+ (incf y) (incf x))
                                         a (+ (incf z) (incf y) (incf x)))
                                  '((let ((x 1) y z a))))
                 '(15 (setq ((Y (2 NIL ((INCF X))))
                             (Z (6 + (3 nil ((incf y))) (3 NIL ((INCF X)))))
                             (a (15 + (7 nil ((incf z))) (4 nil ((incf y))) (4 nil ((incf x))))))))
                 '((let ((x 4) (y 4) (z 7) (a 15)))) nil)
   ("Ditch function values"
    (let ((fn (lambda (x) (first (second x)))))
      (values-equal (debug-perimeter '(setq a external
                                            b 2
                                            c #'string=)
                                     `((let ((external ,fn) a b c))))
                    `(,#'string= (setq ((a (:anonymous-function external))
                                        (b (2))
                                        (c ((:function string=))))))
                    `((let ((external ,fn) (a ,fn) (b 2) (c ,#'string=))))
                    nil))))
  ("let"
   ("full bindings"
    (values-equal (debug-perimeter '(let ((x (+ 1 2)) (y (* 3 4))) (+ x y)) '((let ((z 1)))))
                  '(15 (let ((x (3 + (1) (2))) (y (12 * (3) (4)))) (15 + (3 x) (12 y))))
                  '((let ((z 1)))) nil))
   ("Preserve list values"
    (values-equal (debug-perimeter '(let ((x (list 1 2))) (list x 3 4)) '((let ((z 1)))))
                  '(((1 2) 3 4)
                    (let ((x ((1 2) list (1) (2)))) (((1 2) 3 4) list ((1 2) x) (3) (4))))
                  '((let ((z 1)))) nil))
   ("Forward multiple values"
    (values-equal (debug-perimeter '(let ((x (+ 1 2)) (y (* 3 4))) (values x y)) '((let ((z 1)))))
                  '((values-list (quote (3 12)))
                    (let ((x (3 + (1) (2))) (y (12 * (3) (4))))
                      ((values-list (quote (3 12))) values (3 x) (12 y))))
                  '((let ((z 1)))) nil))
   ("Ditch function values in the bindings"
    (values-equal (debug-perimeter '(let ((x #'string=) (y (lambda (x) (first (second x)))))
                                      (funcall x "test" (funcall y '(1 ("test" 2)))))
                                   nil)
                  '(t (let ((x ((:function string=))) (y (:anonymous-function nil ((lambda (x) (first (second x)))))))
                        (t funcall ((:function string=) x)
                           ("test")
                           ("test" funcall (:anonymous-function y) ((1 ("test" 2)))))))
                  nil nil))
   ("Bind automatically the first value of multiple values"
     (values-equal (debug-perimeter '(let ((x (values 1 2 3))) (+ x 1)) nil)
                   '(2 (LET ((X ((VALUES-LIST (QUOTE (1 2 3))) VALUES (1) (2) (3)))) (2 + (1 X) (1))))
                   nil nil))
   ("single bindings (the value is nil)"
    (values-equal (debug-perimeter '(let ((x (list 1 2)) y) (append x y)) '((let ((z 1)))))
                  '((1 2) (let ((x ((1 2) list (1) (2))) (y (nil))) ((1 2) append ((1 2) x) (nil y))))
                  '((let ((z 1)))) nil))
   ("Side-effect"
    (values-equal (debug-perimeter '(let ((x 1) (y 2)) (incf x) (+ x y)) '((let ((z 1)))))
                  '(4 (let ((x (1)) (y (2)))
                        (4 progn
                           (2 nil ((incf x)))
                           (4 + (2 x) (2 y)))))
                  '((let ((z 1)))) nil)
    (values-equal (debug-perimeter '(let ((y (incf x))
                                          (z (incf x)))
                                      (values x y z))
                                   '((let ((x 1)))))
                  '((VALUES-LIST (QUOTE (3 2 3)))
                    (LET ((Y (2 NIL ((INCF X))))
                          (Z (3 NIL ((INCF X)))))
                      ((VALUES-LIST (QUOTE (3 2 3))) VALUES (3 X) (2 Y) (3 Z))))
                  '((let ((x 3)))) nil))
   ("Shadowing"
    (values-equal (debug-perimeter '(let ((x -1) (y 2)) (* x (let ((x 1)) (+ x y)))) '((let ((z 1)))))
                  '(-3 (let ((x (-1)) (y (2)))
                         (-3 *
                             (-1 x)
                             (3 (let ((x (1)))
                                  (3 + (1 x) (2 y)))))))
                  '((let ((z 1)))) nil)
    (values-equal (debug-perimeter '(let ((x -1) (y 2)) (let ((x 1))) (+ x y)) '((let ((z 1)))))
                  '(1 (let ((x (-1)) (y (2)))
                        (1 progn
                           (nil (let ((x (1))) (nil progn nil)))
                           (1 + (-1 x) (2 y)))))
                  '((let ((z 1)))) nil))
   ("Shadowing and side-effect"
    (values-equal (debug-perimeter '(let ((x -1) (y 2)) (* x (let ((x 1)) (incf x) (+ x y))))
                                   '((let ((z 1)))))
                  '(-4 (let ((x (-1)) (y (2)))
                         (-4 *
                             (-1 x)
                             (4 (let ((x (1)))
                                  (4 progn
                                     (2 nil ((incf x)))
                                     (4 + (2 x) (2 y))))))))
                  '((let ((z 1)))) nil)))
  ("let*"
   ("Same features as let"
    ("full bindings"
     (values-equal (debug-perimeter '(let* ((x (+ 1 2)) (y (* 3 4))) (+ x y)) '((let ((z 1)))))
                   '(15 (let* ((x (3 + (1) (2))) (y (12 * (3) (4)))) (15 + (3 x) (12 y))))
                   '((let ((z 1)))) nil))
    ("Preserve list values"
     (values-equal (debug-perimeter '(let* ((x (list 1 2))) (list x 3 4)) '((let ((z 1)))))
                   '(((1 2) 3 4)
                     (let* ((x ((1 2) list (1) (2)))) (((1 2) 3 4) list ((1 2) x) (3) (4))))
                   '((let ((z 1)))) nil))
    ("Forward multiple values"
     (values-equal (debug-perimeter '(let* ((x (+ 1 2)) (y (* 3 4))) (values x y)) '((let ((z 1)))))
                   '((values-list (quote (3 12)))
                     (let* ((x (3 + (1) (2))) (y (12 * (3) (4))))
                       ((values-list (quote (3 12))) values (3 x) (12 y))))
                   '((let ((z 1)))) nil))
    ("Ditch function values in the bindings"
     (values-equal (debug-perimeter '(let* ((x #'string=) (y (lambda (x) (first (second x)))))
                                       (funcall x "test" (funcall y '(1 ("test" 2)))))
                                    nil)
                   '(t (let* ((x ((:function string=))) (y (:anonymous-function nil ((lambda (x) (first (second x)))))))
                         (t funcall ((:function string=) x)
                            ("test")
                            ("test" funcall (:anonymous-function y) ((1 ("test" 2)))))))
                   nil nil))
    ("Bind automatically the first value of multiple values"
     (values-equal (debug-perimeter '(let* ((x (values 1 2 3))) (+ x 1)) nil)
                   '(2 (LET* ((X ((VALUES-LIST (QUOTE (1 2 3))) VALUES (1) (2) (3)))) (2 + (1 X) (1))))
                   nil nil))
    ("single bindings (the value is nil)"
     (values-equal (debug-perimeter '(let* ((x (list 1 2)) y) (append x y)) '((let ((z 1)))))
                   '((1 2) (let* ((x ((1 2) list (1) (2))) (y (nil))) ((1 2) append ((1 2) x) (nil y))))
                   '((let ((z 1)))) nil))
    ("Side-effect"
     (values-equal (debug-perimeter '(let* ((x 1) (y 2)) (incf x) (+ x y)) '((let ((z 1)))))
                   '(4 (let* ((x (1)) (y (2)))
                         (4 progn
                            (2 nil ((incf x)))
                            (4 + (2 x) (2 y)))))
                   '((let ((z 1)))) nil)
     (values-equal (debug-perimeter '(let* ((y (incf x))
                                            (z (+ y (incf x))))
                                       (values x y z))
                                    '((let ((x 1)))))
                   '((VALUES-LIST (QUOTE (3 2 5)))
                     (LET* ((Y (2 NIL ((INCF X))))
                            (Z (5 + (2 y) (3 NIL ((INCF X))))))
                       ((VALUES-LIST (QUOTE (3 2 5))) VALUES (3 X) (2 Y) (5 Z))))
                   '((let ((x 3)))) nil)
     (values-equal (debug-perimeter '(let* ((y (incf x))
                                            (z (+ (incf y) (incf x)))
                                            (a (+ (incf z) (incf y) (incf x))))
                                       (values x y z a))
                                    '((let ((x 1)))))
                   '((VALUES-LIST (QUOTE (4 4 7 15)))
                     (LET* ((Y (2 NIL ((INCF X))))
                            (Z (6 + (3 nil ((incf y))) (3 NIL ((INCF X)))))
                            (a (15 + (7 nil ((incf z))) (4 nil ((incf y))) (4 nil ((incf x))))))
                       ((VALUES-LIST (QUOTE (4 4 7 15))) VALUES (4 X) (4 Y) (7 Z) (15 a))))
                   '((let ((x 4)))) nil))
    ("Shadowing"
     (values-equal (debug-perimeter '(let* ((x -1) (y 2)) (* x (let* ((x 1)) (+ x y)))) '((let ((z 1)))))
                   '(-3 (let* ((x (-1)) (y (2)))
                          (-3 *
                              (-1 x)
                              (3 (let* ((x (1)))
                                   (3 + (1 x) (2 y)))))))
                   '((let ((z 1)))) nil)
     (values-equal (debug-perimeter '(let* ((x -1) (y 2)) (let* ((x 1))) (+ x y)) '((let ((z 1)))))
                   '(1 (let* ((x (-1)) (y (2)))
                         (1 progn
                            (nil (let* ((x (1))) (nil progn nil)))
                            (1 + (-1 x) (2 y)))))
                   '((let ((z 1)))) nil))
    ("Shadowing and side-effect"
     (values-equal (debug-perimeter '(let* ((x -1) (y 2)) (* x (let* ((x 1)) (incf x) (+ x y)))) '((let ((z 1)))))
                   '(-4 (let* ((x (-1)) (y (2)))
                          (-4 *
                              (-1 x)
                              (4 (let* ((x (1)))
                                   (4 progn
                                      (2 nil ((incf x)))
                                      (4 + (2 x) (2 y))))))))
                   '((let ((z 1)))) nil)))
   ("Previous bindings available"
    (values-equal (debug-perimeter '(let* ((x 1) (y (+ 1 x))) (+ x y)) '((let ((z 1)))))
                  '(3 (let* ((x (1)) (y (2 + (1) (1 x))))
                        (3 + (1 x) (2 y))))
                  '((let ((z 1)))) nil))
   ("Previous bindings shadow upper bindings"
    (values-equal (debug-perimeter '(let ((x -1)) (let* ((x 1) (y (+ 1 x))) (+ x y))) '((let ((z 1)))))
                  '(3 (let ((x (-1)))
                        (3 (let* ((x (1)) (y (2 + (1) (1 x))))
                             (3 + (1 x) (2 y))))))
                  '((let ((z 1)))) nil))))
 ("Handling the various arguments in a lambda-list "
  ("Optional arguments"
   (values-equal (debug-perimeter '((lambda (x &optional (y 10)) (+ x y)) 10 11) nil)
                 '(21 ((lambda (x &optional (y 10)) (+ x y)) ((x 10) (y 11)) (21 + (10 x) (11 y))) (10) (11))
                 nil nil)
   (values-equal (debug-perimeter '((lambda (x &optional (y 10)) (+ x y)) 10) nil)
                 '(20 ((lambda (x &optional (y 10)) (+ x y)) ((x 10) (y 10)) (20 + (10 x) (10 y))) (10))
                 nil nil)
   (values-equal (debug-perimeter '((lambda (x &optional (y 10) (z 20)) (+ x y z)) 10 11) nil)
                 '(41 ((lambda (x &optional (y 10) (z 20)) (+ x y z)) ((x 10) (y 11) (z 20)) (41 + (10 x) (11 y) (20 z))) (10) (11))
                 nil nil)
   (values-equal (debug-perimeter '((lambda (x &optional (y 10) (z 20)) (+ x y z)) 10 11 12) nil)
                 '(33 ((lambda (x &optional (y 10) (z 20)) (+ x y z)) ((x 10) (y 11) (z 12)) (33 + (10 x) (11 y) (12 z))) (10) (11) (12))
                 nil nil)
   (values-equal (debug-perimeter '((lambda (x &optional (y 10 y-supplied?)) (+ x y (if y-supplied? 10 1))) 10) nil)
                 '(21 ((lambda (x &optional (y 10 y-supplied?)) (+ x y (if y-supplied? 10 1)))
                       ((x 10) (y 10) (y-supplied? nil)) (21 + (10 x) (10 y) (1 if (nil y-supplied?) (1)))) (10))
                 nil nil)
   (values-equal (debug-perimeter '((lambda (x &optional (y 10 y-supplied?)) (+ x y (if y-supplied? 10 1))) 10 11) nil)
                 '(31 ((lambda (x &optional (y 10 y-supplied?)) (+ x y (if y-supplied? 10 1)))
                       ((x 10) (y 11) (y-supplied? t)) (31 + (10 x) (11 y) (10 if (t y-supplied?) (10)))) (10) (11))
                 nil nil))
  ("Rest argument"
   (values-equal (debug-perimeter '((lambda (x &rest lst) (+ x (length lst))) 10) nil)
                 '(10 ((lambda (x &rest lst) (+ x (length lst))) ((x 10) (lst nil)) (10 + (10 x) (0 length (nil lst)))) (10))
                 nil nil)
   (values-equal (debug-perimeter '((lambda (x &rest lst) (+ x (length lst))) 10 1) nil)
                 '(11 ((lambda (x &rest lst) (+ x (length lst))) ((x 10) (lst '(1))) (11 + (10 x) (1 length ((1) lst)))) (10) (1))
                 nil nil)
   (values-equal (debug-perimeter '((lambda (x &rest lst) (+ x (length lst))) 10 1 2 3) nil)
                 '(13 ((lambda (x &rest lst) (+ x (length lst))) ((x 10) (lst '(1 2 3))) (13 + (10 x) (3 length ((1 2 3) lst)))) (10) (1) (2) (3))
                 nil nil))
  ("Keys arguments"
   (values-equal (debug-perimeter '((lambda (x &key (y 10)) (+ x y)) 10 :y 11) nil)
                 '(21 ((lambda (x &key (y 10)) (+ x y)) ((x 10) (y 11)) (21 + (10 x) (11 y))) (10) (:y) (11))
                 nil nil)
   (values-equal (debug-perimeter '((lambda (x &key (y 10)) (+ x y)) 10) nil)
                 '(20 ((lambda (x &key (y 10)) (+ x y)) ((x 10) (y 10)) (20 + (10 x) (10 y))) (10))
                 nil nil)
   (values-equal (debug-perimeter '((lambda (x &key (y 10) (z 20)) (+ x y z)) 10 :z 12) nil)
                 '(32 ((lambda (x &key (y 10) (z 20)) (+ x y z)) ((x 10) (y 10) (z 12)) (32 + (10 x) (10 y) (12 z))) (10) (:z) (12))
                 nil nil)
   (values-equal (debug-perimeter '((lambda (x &key (y 10) (z 20)) (+ x y z)) 10 :z 12 :y 11) nil)
                 '(33 ((lambda (x &key (y 10) (z 20)) (+ x y z)) ((x 10) (y 11) (z 12)) (33 + (10 x) (11 y) (12 z))) (10) (:z) (12) (:y) (11))
                 nil nil)
   (values-equal (debug-perimeter '((lambda (x &key (y 10 y-supplied?)) (+ x y (if y-supplied? 10 1))) 10) nil)
                 '(21 ((lambda (x &key (y 10 y-supplied?)) (+ x y (if y-supplied? 10 1)))
                       ((x 10) (y 10) (y-supplied? nil)) (21 + (10 x) (10 y) (1 if (nil y-supplied?) (1)))) (10))
                 nil nil)
   (values-equal (debug-perimeter '((lambda (x &key (y 10 y-supplied?)) (+ x y (if y-supplied? 10 1))) 10 :y 11) nil)
                 '(31 ((lambda (x &key (y 10 y-supplied?)) (+ x y (if y-supplied? 10 1)))
                       ((x 10) (y 11) (y-supplied? t)) (31 + (10 x) (11 y) (10 if (t y-supplied?) (10)))) (10) (:y) (11))
                 nil nil)))
 ("Errors"
  ("Unbound variable"
   (values-equal (debug-perimeter 'test '((let ((x 1)))))
                 '(nil :error test)
                 '((let ((x 1))))
                 '("The variable TEST is unbound.")))
  ("Function calls"
   ("Direct error"
    (values-equal (debug-perimeter '(+ 1 nil)'((let ((x 1)))))
                  '(nil :error + (1) (nil))
                  '((let ((x 1))))
                  '("In + of (1 NIL) arguments should be of type NUMBER.")))
   ("One error in the arguments"
    (values-equal (debug-perimeter '(* 2 (+ 1 nil)) '((let ((x 1)))))
                  '(NIL * (2) (NIL :error + (1) (NIL)))
                  '((let ((x 1))))
                  '("In + of (1 NIL) arguments should be of type NUMBER.")))
   ("Two errors in the arguments"
    (values-equal (debug-perimeter '(* (+ 2 nil) (+ 1 nil)) '((let ((x 1)))))
                  '(NIL * (NIL :error + (2) (NIL)) (NIL :error + (1) (NIL)))
                  '((let ((x 1))))
                  '("In + of (2 NIL) arguments should be of type NUMBER."
                    "In + of (1 NIL) arguments should be of type NUMBER."))))
  ("Special operators"
   ("function"
    (values-equal (debug-perimeter '(function foo) '((let ((x 1)))))
                  '(NIL :error function (foo))
                  '((let ((x 1))))
                  '("Undefined function FOO in form (FUNCTION FOO).")))
   ("progn"
    (values-equal (debug-perimeter '(progn (+ 1 nil) (* 2 2)) '((let ((x 1)))))
                  '(NIL PROGN (NIL :error + (1) (NIL)) (4 * (2) (2)))
                  '((let ((x 1))))
                  '("In + of (1 NIL) arguments should be of type NUMBER.")))
   ("if"
    ("Error in the condition"
     (values-equal (debug-perimeter '(if (+ 1 nil) (+ 2 nil) (+ 3 3)) '((let ((x 1)))))
                   '(NIL IF (NIL :error + (1) (NIL)) (NIL))
                   '((let ((x 1))))
                   '("In + of (1 NIL) arguments should be of type NUMBER.")))
    ("Error in the activated branch"
     (values-equal (debug-perimeter '(if (+ 1 1) (+ 2 nil) (+ 3 3)) '((let ((x 1)))))
                   '(NIL IF (2 + (1) (1)) (NIL :error + (2) (NIL)))
                   '((let ((x 1))))
                   '("In + of (2 NIL) arguments should be of type NUMBER."))))
   ("setq"
    ("An error in one computed values"
     (values-equal (debug-perimeter '(setq a (+ 1 nil) b 2 c 3) '((let (a b c))))
                   '(nil :error (SETQ ((A (nil :error + (1) (nil))) (B (2)) (C (3)))))
                   '((let ((a nil) (b 2) (c 3))))
                   '("In + of (1 NIL) arguments should be of type NUMBER.")))
    ("Two errors, in two computed values"
     (values-equal (debug-perimeter '(setq a (+ 1 nil) b 2 c (+ 3 nil)) '((let (a b c))))
                   '(nil :error (SETQ ((A (nil :error + (1) (nil))) (B (2)) (C (nil :error + (3) (nil))))))
                   '((let ((a nil) (b 2) (c nil))))
                   '("In + of (1 NIL) arguments should be of type NUMBER."
                     "In + of (3 NIL) arguments should be of type NUMBER.")))
    ("One error in the syntax of setq"
     (values-equal (debug-perimeter '(setq (+ 1 1) (+ 1 1) b 2 c 3) '((let (a b c))))
                   '(nil :error (SETQ (((+ 1 1) (2  + (1) (1))) (B (2)) (C (3)))))
                   '((let ((a nil) (b 2) (c 3))))
                   '("Cannot setq (+ 1 1) -- not a symbol.")))
    ("Two errors in the syntax of setq"
     (values-equal (debug-perimeter '(setq (+ 1 1) (+ 1 1) (+ 2 2) 2 c 3) '((let (a b c))))
                   '(nil :error (SETQ (((+ 1 1) (2  + (1) (1))) ((+ 2 2) (2)) (C (3)))))
                   '((let ((a nil) (b nil) (c 3))))
                   '("Cannot setq (+ 1 1) -- not a symbol."
                     "Cannot setq (+ 2 2) -- not a symbol.")))
    ("Several errors in the syntax and in the computed values"
     (values-equal (debug-perimeter '(setq (+ 1 1) (+ 1 nil) (+ 2 2) 2 c (+ 3 nil)) '((let (a b c))))
                   '(nil :error (SETQ (((+ 1 1) (nil :error + (1) (nil))) ((+ 2 2) (2)) (C (nil :error + (3) (nil))))))
                   '((let ((a nil) (b nil) (c nil))))
                   '("Cannot setq (+ 1 1) -- not a symbol."
                     "Cannot setq (+ 2 2) -- not a symbol."
                     "In + of (1 NIL) arguments should be of type NUMBER."
                     "In + of (3 NIL) arguments should be of type NUMBER."))))
   ("let"
    ("Error in the body"
     (values-equal (debug-perimeter '(let ((x (+ 1 1)) y)
                                       (+ x y))
                                    '((let ((test 1)))))
                   '(nil (let ((x (2 + (1) (1))) (y (nil)))
                           (nil :error + (2 x) (nil y))))
                   '((let ((test 1))))
                   '("In + of (2 NIL) arguments should be of type NUMBER.")))
    ("One error in the bindings"
     (values-equal (debug-perimeter '(let ((x (+ 1 nil)) (y (+ 2 2)))
                                       (+ x y))
                                    '((let ((test 1)))))
                   '(nil (let ((x (nil :error + (1) (nil)))
                               (y (4 + (2) (2))))
                           nil))
                   '((let ((test 1))))
                   '("In + of (1 NIL) arguments should be of type NUMBER.")))
    ("Two errors in the bindings"
     (values-equal (debug-perimeter '(let ((x (+ 1 nil)) (y (+ 2 nil)))
                                       (+ x y))
                                    '((let ((test 1)))))
                   '(nil (let ((x (nil :error + (1) (nil)))
                               (y (nil :error + (2) (nil))))
                           nil))
                   '((let ((test 1))))
                   '("In + of (1 NIL) arguments should be of type NUMBER."
                     "In + of (2 NIL) arguments should be of type NUMBER."))))
   ("let*"
    ("Error in the body"
     (values-equal (debug-perimeter '(let* ((x (+ 1 1)) y)
                                       (+ x y))
                                    '((let ((test 1)))))
                   '(nil (let* ((x (2 + (1) (1))) (y (nil)))
                           (nil :error + (2 x) (nil y))))
                   '((let ((test 1))))
                   '("In + of (2 NIL) arguments should be of type NUMBER.")))
    ("One error in the bindings"
     (values-equal (debug-perimeter '(let* ((x (+ 1 nil)) (y (+ 2 2)))
                                       (+ x y))
                                    '((let ((test 1)))))
                   '(nil (let* ((x (nil :error + (1) (nil)))
                                (y (4 + (2) (2))))
                           nil))
                   '((let ((test 1))))
                   '("In + of (1 NIL) arguments should be of type NUMBER.")))
    ("Two errors in the bindings"
     (values-equal (debug-perimeter '(let* ((x (+ 1 nil)) (y (+ 2 nil)))
                                       (+ x y))
                                    '((let ((test 1)))))
                   '(nil (let* ((x (nil :error + (1) (nil)))
                                (y (nil :error + (2) (nil))))
                           nil))
                   '((let ((test 1))))
                   '("In + of (1 NIL) arguments should be of type NUMBER."
                     "In + of (2 NIL) arguments should be of type NUMBER."))))
  ("Lambda form"
   ("Error in the body"
    (values-equal (debug-perimeter '((lambda (x) (+ x nil)) 5)
                                   '((let ((test 1)))))
                  '(nil ((lambda (x) (+ x nil)) ((x 5)) (nil :error + (5 x) (nil))) (5))
                  '((let ((test 1))))
                  '("In + of (5 NIL) arguments should be of type NUMBER.")))
   ("One error in the bindings"
    (values-equal (debug-perimeter '((lambda (x) (+ x 3)) (+ 5 nil))
                                   '((let ((test 1)))))
                  '(nil ((lambda (x) (+ x 3)) ((x nil)) nil) (nil :error + (5) (nil)))
                  '((let ((test 1))))
                  '("In + of (5 NIL) arguments should be of type NUMBER.")))
   ("Two errors in the bindings"
    (values-equal (debug-perimeter '((lambda (x y) (+ x y)) (+ 5 nil) (+ 6 nil))
                                   '((let ((test 1)))))
                  '(nil ((lambda (x y) (+ x y)) ((x nil) (y nil)) nil)
                        (nil :error + (5) (nil))
                        (nil :error + (6) (nil)))
                  '((let ((test 1))))
                  '("In + of (5 NIL) arguments should be of type NUMBER."
                    "In + of (6 NIL) arguments should be of type NUMBER."))))
   )))