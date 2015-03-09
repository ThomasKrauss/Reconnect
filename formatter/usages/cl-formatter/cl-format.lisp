("simple tests"
 ("on nil" (string= (cl-format nil) ""))
 ("on empty string" (string= (cl-format "") ""))
 ("one-liner function"
  (string= (cl-format "(+ 1 2 (* 3 4))") "(+ 1 2 (* 3 4))"))
 ("'regular' two-liner function"
  (string= (cl-format
            "(+ 1 2
(* 3 4))")
           "(+ 1 2
   (* 3 4))"))) 

("syntax exceptions"
 ("quote with a newline after the first element"
  (string= (cl-format "(find 2 '(1
2 3))")
           "(find 2 '(1
          2 3))"))
 ("quote with a newline after the second element"
  (string= (cl-format "(find 2 '(1 2
3))")
           "(find 2 '(1 2
            3))"))
 ("quote with a newline after the fourth element"
  (string= (cl-format "(find 2 '(1 2 3 4
5 6))")
           "(find 2 '(1 2 3 4
            5 6))"))
 ("appropriate indentation of an element after a quoted element"
  (string= (cl-format "(find 2 '((1 \"bar\") (2 \"baz\") (3 \"foo\") (2 \"bulu\"))
:key #'first)")
           "(find 2 '((1 \"bar\") (2 \"baz\") (3 \"foo\") (2 \"bulu\"))
      :key #'first)")))

("spaces and newlines in uncommon places"
 ("'twisted' two-liner function"
  (string= (cl-format
            "
(+ 1 2 (* 3 4))")
           "(+ 1 2 (* 3 4))"))
 ("another 'twisted' two-liner function"
  (string= (cl-format
            "(
+ 1 2 (* 3 4))")
           "(+ 1 2 (* 3 4))"))
 ("some additional spaces in front of the form. There should be no impact."
  (string= (cl-format
            "   (+ 1 2
(* 3 4))")
           "(+ 1 2
   (* 3 4))"))
 ("some additional spaces between the first parenthesis and the action-name. No impact expected."
  (string= (cl-format
            "(     + 1 2
(* 3 4))")
           "(+ 1 2
   (* 3 4))"))
 ("some additional spaces between arguments. No impact expected."
  (string= (cl-format
            "(+    1     2
(* 3 4))")
           "(+ 1 2
   (* 3 4))"))
 ("some whitespace before the newline. No impact expected."
  (string= (cl-format
            "(+ 1 2    
(* 3 4))")
           "(+ 1 2
   (* 3 4))"))
 ("some additional spaces in front of the newline. No impact expected."
  (string= (cl-format
            "(+ 1 2
                  (* 3 4))")
           "(+ 1 2
   (* 3 4))"))
 ("some additional spaces between two closing parentheses. No impact expected."
  (string= (cl-format
            "(+ 1 2
(* 3 4)          )")
           "(+ 1 2
   (* 3 4))"))
 ("some additional spaces at the end of the form. No impact expected."
  (string= (cl-format
            "(+ 1 2
(* 3 4))                 ")
           "(+ 1 2
   (* 3 4))"))) 


("test recursive indentation"
 ("(Is it that complete?)"
  (string= (cl-format
            "(+ 1 2 (* 3
4)
(/ 50 (- 6
7 (* 80
90))
10 11))")
           "(+ 1 2 (* 3
          4)
   (/ 50 (- 6
            7 (* 80
                 90))
      10 11))")))

; def*
("Body indentation"
 ("defun"
  (string= (cl-format
            "(defun test (x)
(+ x 2)
(+ x 3))")
           "(defun test (x)
    (+ x 2)
    (+ x 3))"))
 ("lambda"
  (string= (cl-format
            "(lambda (x)
(+ x 2)
(+ x 3))")
           "(lambda (x)
    (+ x 2)
    (+ x 3))"))
 ("with-open-file"
  (string= (cl-format
            "(with-open-file (s file)
(write-char #\! s))")
           "(with-open-file (s file)
    (write-char #\! s))"))
 ("let"
  (string= (cl-format
            "(let ((test 1))
(+ test foo))")
           "(let ((test 1))
    (+ test foo))"))
 ("progn"
  (string= (cl-format "(progn
(+ 1 2)
(+ 3 4))")
           "(progn
    (+ 1 2)
    (+ 3 4))")))

; with-open-file, unwind-protect
("Complex indentation"
 ("multiple-value-bind"
  (string= (cl-format
            "(multiple-value-bind
(first second)
(if (= 0 x)
(values 1 2)
(values 3 4))
(incf first)
(list first
(* 2 second)))")
           "(multiple-value-bind
        (first second)
        (if (= 0 x)
            (values 1 2)
            (values 3 4))
    (incf first)
    (list first
          (* 2 second)))"))
 ("unwind-protect"
  (string= (cl-format "(unwind-protect
(setf my-stream (open file))
(close my-stream))")
           "(unwind-protect
        (setf my-stream (open file))
    (close my-stream))")))


("Indent size argument"
 ("validate the indent-size argument with multiple-value-bind"
  (string= (cl-format
            "(multiple-value-bind
(first second)
(if (= 0 x)
(values 1 2)
(values 3 4))
(list first
(* 2 second)))"
            2)
           "(multiple-value-bind
    (first second)
    (if (= 0 x)
        (values 1 2)
        (values 3 4))
  (list first
        (* 2 second)))"))) 


("Strings"
 ("list of strings, expect one whitespace character as separator"
	(string= (cl-format "(\"Lisp induced resentment\" \"bellevue\" \"test\")")
		"(\"Lisp induced resentment\" \"bellevue\" \"test\")"))
 ("on a newline, single line, no parentheses"
  (string= (cl-format
            "(act test (x)
\"This is a small documentation string.\"
(+ x 2))")
           "(act test (x)
     \"This is a small documentation string.\"
     (+ x 2))"))
 ("on a newline, single line, parentheses"
  (string= (cl-format
            "(act test (x)
\"(test 4) gives 6.\"
(+ x 2))")
           "(act test (x)
     \"(test 4) gives 6.\"
     (+ x 2))"))
 ("on a newline, single line, an escaped double-quote"
  (string= (cl-format
            "(act test (x)
\"This is a \\\"small\\\" documentation string.\"
(+ x 2))")
           "(act test (x)
     \"This is a \\\"small\\\" documentation string.\"
     (+ x 2))"))
 ("as an argument, single line"
  (string= (cl-format
            "(concatenate 'string \"test\"
\"!!!\")")
           "(concatenate 'string \"test\"
             \"!!!\")"))
 ("multiple lines, no parentheses"
  (string= (cl-format
            "(act test (x)
\"This is a bigger documentation string.
I want to say much more things here!\"
(+ x 2))")
           "(act test (x)
     \"This is a bigger documentation string.
I want to say much more things here!\"
     (+ x 2))"))
 ("multiple lines, parentheses"
  (string= (cl-format
            "(act test (x)
\"This is a bigger documentation string.
I want to say much more things here!
Like (test 4) gives 6.\"
(+ x 2))")
           "(act test (x)
     \"This is a bigger documentation string.
I want to say much more things here!
Like (test 4) gives 6.\"
     (+ x 2))"))) 


("Single line comment"
 ("between lines of code, no parentheses"
  (string= (cl-format
            "(act test (x)
; This is a small comment
(+ x 2))")
           "(act test (x)
     ; This is a small comment
     (+ x 2))"))
 ("between lines of code, parentheses"
  (string= (cl-format
            "(act test (x)
; (test 4) gives 6.
(+ x 2))")
           "(act test (x)
     ; (test 4) gives 6.
     (+ x 2))"))
 ("at the end of a line, no parentheses"
  (string= (cl-format
            "(act test (x) ; This is a end-of-line comment
(+ x 2))")
           "(act test (x) ; This is a end-of-line comment
     (+ x 2))"))
 ("at the end of a line, parentheses"
  (string= (cl-format
            "(act test (x) ; (test 4) gives 6.
(+ x 2))")
           "(act test (x) ; (test 4) gives 6.
     (+ x 2))"))) 

("Lists"
 ("Plain list"
  (string= (cl-format "(1 2 3)") "(1 2 3)"))
 ("List with newline after the first element"
  (string= (cl-format "(1
2 3)")
           "(1
 2 3)"))
 ("List with newline after the first element and further newlines between elements afterwards"
  (string= (cl-format "(1
2 3
4 5)")
           "(1
 2 3
 4 5)"))
 ("List with newline after the second element"
  (string= (cl-format "(1 2
3 4)")
           "(1 2
   3 4)"))
 ("List with newline after the second element and further newlines between elements afterwards"
  (string= (cl-format "(1 2
3 4
5 6)")
           "(1 2
   3 4
   5 6)"))
 ("List with newline after the fourth element"
  (string= (cl-format "(1 2 3 4
5 6)")
           "(1 2 3 4
   5 6)"))
 ("List with newline after the fourth element and further newlines between elements afterwards"
  (string= (cl-format "(1 2 3 4
5 6
7 8)")
           "(1 2 3 4
   5 6
   7 8)"))
 ("List of one list, expect no whitespace character between the first two parentheses"
  (string= (cl-format "((1 2 3))")
           "((1 2 3))"))
 ("List of several lists, on one line, expect one whitespace character as separator"
  (string= (cl-format "((1 2 3) (4 5 6) (7 8 9))")
           "((1 2 3) (4 5 6) (7 8 9))"))
 ("List of several lists, on several lines, expect one whitespace character as separator"
  (string= (cl-format "((1 2
3)
(4
5 6)
(7 8 9))")
           "((1 2
    3)
 (4
  5 6)
 (7 8 9))"))
 ("Deeply nested lists n°1"
  (string= (cl-format "((((1 2
3) (11 21
31)
(41
51 61 (71 81
91)))
((4 5
6)
(7
8
9)))
(10 11
12))")
           "((((1 2
      3) (11 21
             31)
         (41
          51 61 (71 81
                    91)))
  ((4 5
      6)
   (7
    8
    9)))
 (10 11
     12))"))
  ("Deeply nested lists n°2"
  (string= (cl-format "((((1 2
3)
(41
51 61 (71 81
91)))
((4 5
6)
(7
8
9)))
(10 11
12))")
           "((((1 2
      3)
   (41
    51 61 (71 81
              91)))
  ((4 5
      6)
   (7
    8
    9)))
 (10 11
     12))")))

("Real world usages"
	("From my site"
(string= (cl-format "	(defun read-obvious-links-from-file (file)
		\"The file is supposed to contain a list of lines
that are associated 2 by 2 in the order they are written.

Return a list composed of such couples.\"
		(group 
			(with-open-file (s file)
				(loop for line = (read-line s nil)
					while line
					if (string/= \"\" line)
					collect line))
			2))")
"(defun read-obvious-links-from-file (file)
    \"The file is supposed to contain a list of lines
that are associated 2 by 2 in the order they are written.

Return a list composed of such couples.\"
    (group
     (with-open-file (s file)
         (loop for line = (read-line s nil)
               while line
               if (string/= \"\" line)
               collect line))
     2))")
	(string= (cl-format "	(let (my-stream)
		(unwind-protect
			(progn
				(setf my-stream (open file))
				body)
			(close my-stream)))")
"(let (my-stream)
    (unwind-protect
            (progn
                (setf my-stream (open file))
                body)
        (close my-stream)))")
	(string= (cl-format "; declaring a variable to store the file stream
	(let (my-stream)
		; try...
		(unwind-protect
			; ...a series of actions
			(progn
				; first store in my-stream the result of opening a stream to file
				(setf my-stream (open file))

				; then collect all the non-empty lines
				(loop for line = (read-line my-stream nil)
					while line
					if (string/= \"\" line)
					collect line))

			; finally close the stream
			(close my-stream)))")
"; declaring a variable to store the file stream
(let (my-stream)
    ; try...
    (unwind-protect
            ; ...a series of actions
            (progn
                ; first store in my-stream the result of opening a stream to file
                (setf my-stream (open file))

                ; then collect all the non-empty lines
                (loop for line = (read-line my-stream nil)
                      while line
                      if (string/= \"\" line)
                      collect line))

        ; finally close the stream
        (close my-stream)))")
	(string= (cl-format "	(let (my-stream)
		(unwind-protect
			(progn
				(setf my-stream (open file))
				(loop for line = (read-line my-stream nil)
					while line
					if (string/= \"\" line)
					collect line))
			(close my-stream)))")
"(let (my-stream)
    (unwind-protect
            (progn
                (setf my-stream (open file))
                (loop for line = (read-line my-stream nil)
                      while line
                      if (string/= \"\" line)
                      collect line))
        (close my-stream)))")
	(string= (cl-format "	(defun parse-article-name (path)
	  \"Article file name are supposed to be formatted like this:
nb-date-title
nb is a number that represent an intended order of reading.
date is the date when the article has been created. Format is yyyyMMdd.
title is self-explanatory.

Return a property list.\"
	  (let ((name (pathname-name path)))
	    (register-groups-bind
	        ((#'parse-integer nb year month day) title)
	        (\"\(\\d+\)-\(\\d\{4\}\)\(\\d\{2\}\)\(\\d\{2\}\)-\(.*\)\" name)
	      (list :order nb :year year :month month :day day :title title))))")
"(defun parse-article-name (path)
    \"Article file name are supposed to be formatted like this:
nb-date-title
nb is a number that represent an intended order of reading.
date is the date when the article has been created. Format is yyyyMMdd.
title is self-explanatory.

Return a property list.\"
    (let ((name (pathname-name path)))
        (register-groups-bind
                ((#'parse-integer nb year month day) title)
                (\"\(\\d+\)-\(\\d\{4\}\)\(\\d\{2\}\)\(\\d\{2\}\)-\(.*\)\" name)
            (list :order nb :year year :month month :day day :title title))))")
	(string= (cl-format "(defmacro with-open-file ((var path) &body body)
	`(let (,var)
			(unwind-protect
				(progn
					(setf ,var (open ,path))
					,@body)
				(close ,var))))")
"(defmacro with-open-file ((var path) &body body)
    `(let (,var)
         (unwind-protect
                 (progn
                     (setf ,var (open ,path))
                     ,@body)
             (close ,var))))")
	(string= (cl-format "	(defun trim-grades (grades)
		(let ((average (/ (reduce #'+ grades)
					(length grades))))
			(remove-if (lambda (n)
					(< n average))
				grades))")
"(defun trim-grades (grades)
    (let ((average (/ (reduce #'+ grades)
                      (length grades))))
        (remove-if (lambda (n)
                       (< n average))
                   grades))")
	(string= (cl-format "(defun make-increment-function (start)
		(let ((count start))
			(lambda ()
				(incf count))))")
"(defun make-increment-function (start)
    (let ((count start))
        (lambda ()
            (incf count))))")
	(string= (cl-format "	(defun foo (x)
	  (format t \"Parameter: ~a~%\" x)      ;  x is argument 
	  (let ((x 2))                        
	    (format t \"Outer LET: ~a~%\" x)    ;  x is 2
	    (let ((x 3))                      
	      (format t \"Inner LET: ~a~%\" x)) ;  x is 3
	    (format t \"Outer LET: ~a~%\" x))   ;  x is 2 again 
	  (format t \"Parameter: ~a~%\" x))     ; x is argument again")
"(defun foo (x)
    (format t \"Parameter: ~a~%\" x) ;  x is argument 
    (let ((x 2))
        (format t \"Outer LET: ~a~%\" x) ;  x is 2
        (let ((x 3))
            (format t \"Inner LET: ~a~%\" x)) ;  x is 3
        (format t \"Outer LET: ~a~%\" x)) ;  x is 2 again 
    (format t \"Parameter: ~a~%\" x)) ; x is argument again")
		(string= (cl-format "	(defun load (filename)
		(with-open-file (in filename)
			(with-standard-io-syntax
				(read out nil))))")
"(defun load (filename)
    (with-open-file (in filename)
        (with-standard-io-syntax
         (read out nil))))")
	(string= (cl-format "
	(defun save (data filename)
		(with-open-file (out filename
			:direction :output
			:if-exists :supersede)
			(with-standard-io-syntax
				(print data out))))")
"(defun save (data filename)
    (with-open-file (out filename
                         :direction :output
                         :if-exists :supersede)
        (with-standard-io-syntax
         (print data out))))")
	(string= (cl-format "	(:users ((:name \"Wayne\" :first-name \"Bruce\")
			(:name \"Parker\" :first-name \"Peter\"))
	:lost-objects ((:name \"Bat Soda\" :description \"Perfect for a break.\")
			(:name \"Spider panties\" description \"Stretch and wind-stopper.\")))")
"(:users ((:name \"Wayne\" :first-name \"Bruce\")
         (:name \"Parker\" :first-name \"Peter\"))
 :lost-objects ((:name \"Bat Soda\" :description \"Perfect for a break.\")
                (:name \"Spider panties\" description \"Stretch and wind-stopper.\")))")))
	
; todo missing whitespace like (list 1"test")
; quoted list on a newline