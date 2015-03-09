(in-package :my-utilities)

(defmacro push-each-new (lst place &key (test 'equal))
  (with-gensyms (item)
    `(dolist (,item ,lst)
       (pushnew ,item ,place :test #',test))))

(defmacro put (element place reference &key (test 'equal) (key 'first))
  (once-only (element reference)
    (with-gensyms (element-position)
      `(let ((,element-position ,(append `(position ,reference ,place :test #',test)
                                         (when key `(:key #',key)))))
         (if ,element-position
           (setf (elt ,place ,element-position)
                 (append (elt ,place ,element-position) (list ,element)))
           (push (list ,reference ,element) ,place))))))

(defmacro put-each (elements place reference &key (test 'equal) (key 'first))
  (once-only (elements reference)
    (with-gensyms (element-position)
      `(let ((,element-position ,(append `(position ,reference ,place :test #',test)
                                         (when key `(:key #',key)))))
         (if ,element-position
           (setf (elt ,place ,element-position)
                 (append (elt ,place ,element-position) ,elements))
           (push (cons ,reference ,elements) ,place))))))

(defmacro putnew (element place reference &key (test 'equal) (key 'first) (element-test 'equal))
  (once-only (element reference)
    (with-gensyms (element-position target)
      `(let ((,element-position ,(append `(position ,reference ,place :test #',test)
                                         (when key `(:key #',key)))))
         (if ,element-position
           (let ((,target (elt ,place ,element-position)))
             (unless (find ,element ,target :test #',element-test)
               (setf (elt ,place ,element-position)
                     (append ,target (list ,element)))))
           (push (list ,reference ,element) ,place))))))

(defmacro put-each-new (elements place reference &key (test 'equal) (key 'first) (element-test 'equal))
  (once-only (elements reference)
    (with-gensyms (element-position target new-elements element)
      `(let ((,element-position ,(append `(position ,reference ,place :test #',test)
                                         (when key `(:key #',key)))))
         (if ,element-position
           (let* ((,target (elt ,place ,element-position))
                  (,new-elements (loop for ,element in ,elements
                                       unless (find ,element ,target :test #',element-test)
                                       collect ,element)))
             (setf (elt ,place ,element-position)
                   (append (elt ,place ,element-position) ,new-elements)))
           (push (cons ,reference ,elements) ,place))))))

(defun plistp (a)
  "Identify a plist."
  (and (evenp (length a))
       (every #'keywordp
              (loop for item in a
                    for i from 1
                    if (oddp i) collect item))))

(defun plist<-list (lst)
  (when (keywordp (first lst))
    (let (result error (index 0))
      (loop while (and (not error)
                       (< index (length lst)))
            do (let ((maybe-keyword (awhen (< (1+ index) (length lst))
                                      (elt lst (1+ index)))))
                 (let-a (elt lst index)
                   (if (keywordp it)
                     (push it result)
                     (setf error t)))
                 (if (keywordp maybe-keyword)
                   (progn
                     (push nil result)
                     (incf index))
                   (progn
                     (push maybe-keyword result)
                     (setf index (+ 2 index))))))
      (unless error
        (nreverse result)))))

(defun titled-plistp (a)
  "Identify a titled plist."
  (and (listp a)
       (atom (first a))
       (not (null (rest a)))
       (plistp (rest a))))

(defun plist-keys (plist)
  (loop for item in plist
        for i from 1
        when (oddp i)
        collect item))

(defun plist-values (plist)
  (loop for item in plist
        for i from 1
        when (evenp i)
        collect item))

(defun map-plist (fn plist &key restrict-to-keys)
  (loop for elt in (group plist 2)
        collect (first elt)
        if (and restrict-to-keys
                (member (first elt) restrict-to-keys))
        collect (funcall fn (second elt))
        else
        collect (second elt)))

(defun my-mk-getf (place keys)
  (labels ((rec (keys)
             (if (null keys)
               place
               (if (keywordp (first keys))
                 (list 'getf (rec (rest keys)) (first keys))
                 (list 'get-named-plist (first keys) (rec (rest keys)))))))
    (rec (reverse keys))))

(defmacro getf+ (place &rest keys)
  (my-mk-getf place keys))

(defun rec-getf (place keys)
  (labels ((rec (place keys)
             (if keys
               (if (keywordp (first keys))
                 (rec (getf place (first keys)) (rest keys))
                 (rec (get-named-plist (first keys) place) (rest keys)))
               place)))
    (rec place keys)))

(defun get-name (plist)
  (getf plist :name))

(defun get-in (plist)
  (getf plist :in))

(defun get-named-plist (name lst)
  (awhen (position name lst :test #'equal :key #'get-name)
    (elt lst it)))

(defun plist-overwrite (initial final)
  (dolist (item (group final 2))
    (let ((key (first item))
          (value (second item)))
      (setf (getf initial key) value)))
  initial)

(defvar *setf-named-plist-indexes* nil)

(define-setf-expander get-named-plist (name lst &environment env)
  (multiple-value-bind (vars vals store-vars writer-form reader-form)
      (get-setf-expansion lst env)
    (let ((internal-name (gensym))
          (position (gensym))
          (result (gensym)))
      (values (append vars `(,internal-name ,(first store-vars) ,position))
              (append vals `(,name ,reader-form (position ,internal-name ,(first store-vars)
                                                          :test #'equal :key #'get-name)))
              `(,result)
              `(progn
                 (if ,position
                   (progn
                     (setq ,result (plist-overwrite (elt ,(first store-vars) ,position) ,result))
                     (setf (elt ,(first store-vars) ,position) ,result))
                   (progn
                     (setq ,result (append (list :name ,internal-name) ,result))
                     (push ,result ,(first store-vars))))
                 ,writer-form
                 (push ,position *setf-named-plist-indexes*)
                 ,result)
              `(get-named-plist ,internal-name ,reader-form)))))

(defun copy-plist (plist &key except only)
  (cond
   (only
    (loop for item in (group plist 2)
          when (member (first item) only)
          collect (first item)
          and
          collect (copy-tree (second item))))
   (except
    (loop for item in (group plist 2)
          unless (member (first item) except)
          collect (first item)
          and
          collect (copy-tree (second item))))
   (t
    (copy-tree plist))))

(defmacro as-plist (&rest lst)
  "Works like this on the given list: (foo :bar other (baz baz-args) ...) => (:foo foo :bar other :baz (baz baz-args) ...)"
  `(list
    ,@(let (result next-as-is)
        (dolist (item lst)
          (if next-as-is
            (setf next-as-is nil)
            (cond
             ((keywordp item)
              (setf next-as-is t))
             ((atom item)
              (push (as-keyword item) result))
             (t
              (push (as-keyword (first item)) result))))
          (push item result))
        (nreverse result))))

(defmacro as-titled-plist (&rest lst)
  "Works like this on the given list: (atom foo :bar other (baz baz-args) ...) => (atom :foo foo :bar other :baz (baz baz-args) ...)"
  `(list
    :name ,(first lst)
    ,@(let (result next-as-is)
        (dolist (item (rest lst))
          (if next-as-is
            (setf next-as-is nil)
            (cond
             ((keywordp item)
              (setf next-as-is t))
             ((atom item)
              (push (as-keyword item) result))
             (t
              (push (as-keyword (first item)) result))))
          (push item result))
        (nreverse result))))

(defmacro plist-case (symbols plist &body cases)
  (with-gensyms (item result)
    `(let (,result)
       (dolist (,item (group ,plist 2))
         (let ((,(first symbols) (first ,item))
               (,(second symbols) (second ,item)))
           (declare (ignorable ,(first symbols))
                    (ignorable ,(second symbols)))
           (push ,(first symbols) ,result)
           (push (case ,(first symbols)
                   ,@cases)
                 ,result)))
       (nreverse ,result))))