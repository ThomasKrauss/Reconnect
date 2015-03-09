("dlambda is a dispatching lambda. Much like an object with methods."
 ("Example"
  (equal (funcall (let ((x 10))
                    (dlambda
                     (:increase (val) (incf x val))
                     (:decrease (val) (decf x val))))
                  :increase 10)
         20)
  (equal (funcall (let ((x 10))
                    (dlambda
                     (:increase (val) (incf x val))
                     (:decrease (val) (decf x val))))
                  :decrease 10)
         0)))