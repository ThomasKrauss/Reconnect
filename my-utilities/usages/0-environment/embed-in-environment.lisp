(""
 ("Return the code itself when there is no environment"
  (null (embed-in-environment nil nil))
  (equal 'x (embed-in-environment 'x nil)))
 ("One scope"
  (equal '(let ((x 1)) nil)
         (embed-in-environment nil '((let ((x 1))))))
  (equal '(let ((x 1)) x)
         (embed-in-environment 'x '((let ((x 1)))))))
 ("Two scopes"
  (equal '(let ((y 2)) (let ((x 1)) x))
         (embed-in-environment 'x '((let ((x 1))) (let ((y 2)))))))
 ("Three scopes"
  (equal '(let ((z 3)) (let ((y 2)) (let ((x 1)) x)))
         (embed-in-environment 'x '((let ((x 1))) (let ((y 2))) (let ((z 3))))))))
 
 