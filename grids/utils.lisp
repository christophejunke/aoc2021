(in-package :aoc.grids)

(defun %maybe-macrolet (vars value array body &key apply)
  (cond
    ((not vars) body)
    (apply 
     `((macrolet ((,value () `(apply #'aref ,',array ,',vars)))
         ,@body)))
    (t 
     `((macrolet ((,value () `(aref ,',array . ,',vars)))
         ,@body)))))

(defun %expand/dynamic (vars value result array body)
  (with-gensyms (c rec dims curr subs)
    (once-only (array)
      `(let* ((,dims (array-dimensions ,array))
              (,subs (fill (copy-list ,dims) nil)))
         (labels ((,rec (,dims ,curr)
                    (cond
                      (,dims
                       (series:iterate 
                         ((,c (series:scan-range :below (car ,dims))))
                         (setf (car ,curr) ,c)
                         (,rec (cdr ,dims) (cdr ,curr))))
                      (t 
                       (let ((,vars ,subs))
                         ,@(%maybe-macrolet vars value array body :apply t))))))
           (block nil
             (,rec ,dims ,subs)
             ,result))))))

