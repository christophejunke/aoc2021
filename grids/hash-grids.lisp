(in-package :aoc.grids)

(defclass infinite-grid ()
  ((default-value
    :initarg :default-value :reader default-value)
   (table
    :initarg .table
    :initform (make-hash-table :test #'equalp)
    :reader .table)))

(defvar .table '.table)

(defun make-infinite-grid (&optional default-value)
  (make-instance 'infinite-grid :default-value default-value))

(defun copy-infinite-grid (grid)
  (make-instance 'infinite-grid
                 :default-value (default-value grid)
                 .table (copy-hash-table (.table grid))))

(defun iref (grid coordinates &optional (default nil dp))
  (gethash coordinates
           (.table grid)
           (if dp default (default-value grid))))

(define-setf-expander iref (grid coords% &optional (default nil dp))
  (with-gensyms (table value def coords)
    (values `(,table
              ,def
              ,coords)
            `((.table ,grid)
              ,(if dp default `(default-value ,grid))
              ,coords%)
            `(,value)
            `(prog1 ,value
               (if (equalp ,value ,default)
                   (remhash ,coords ,table)
                   (setf (gethash ,coords ,table ,def) ,value)))
            `(gethash ,coords ,table ,def))))

(defun fold-infinite-grid (g f &optional (acc nil acc-p))
  (flet ((fold/acc (k v) (setf acc (funcall f g k v acc)))
         (fold/nil (k v) (funcall f g k v)))
    (maphash (if acc-p #'fold/acc #'fold/nil) (.table g)))
  acc)

(defun map-into-infinite-grid (target f source)
  (prog1 target
    (flet ((visit (k v) 
             (when (numberp k)
               (break))
             (setf (iref target k) (funcall f source k v))))
      (maphash #'visit (.table source)))))
