(in-package :my-utilities)

(defun make-queue ()
  (let ((q (list nil)))
    (cons q q)))

(defun queue-contents (q)
  (cdar q))

(defun empty-queue-p (q)
  (null (cdar q)))

(defun queue-front (q)
  (cadar q))

(defun enqueue (item q)
  (setf (cdr q)
        (setf (cddr q) (list item)))
  q)

(defun dequeue (q)
  (car (setf (car q) (cdar q))))