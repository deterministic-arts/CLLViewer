
(in-package "CLL-UTILS")


(defmacro cond* (&body clauses)
  (if (null clauses) nil
      (let ((rev-clauses (reverse clauses)))
        (loop
          :with seed := 'nil
          :for clause :in rev-clauses
          :do (destructuring-bind (test &rest body) clause
                (setf seed 
                      (if (and (eql (length body) 2) (symbolp (car body)) (string= (car body) "=>"))
                          (let ((temp (gensym)))
                            `(let ((,temp ,test))
                               (if ,temp (funcall ,(second body) ,temp) ,seed)))
                          `(if ,test (progn ,@body) ,seed))))
          :finally (return seed)))))
