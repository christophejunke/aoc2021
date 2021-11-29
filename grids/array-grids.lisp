(in-package :aoc.grids)

(defun read-grid (stream &key (element-type t) (transform #'identity))
  (loop
    :with grid = (make-array 256
                             :adjustable t
                             :element-type element-type
                             :fill-pointer 0)
    :for line = (read-line stream nil nil)
    :while line
    :for same-width = t :then (= width (length line))
    :for width = (length line)
    :count line :into height
    :do (assert same-width
                ()
                "rows with different width as expected (~d): ~a"
                width
                line)
        (map ()
             (lambda (c)
               (vector-push-extend (funcall transform c)
                                   grid
                                   (array-total-size grid)))
             line)
    :finally (return (make-array (list height width)
                                 :element-type element-type
                                 :displaced-to grid))))

(defun map-grid-into (target function source)
  (let ((target (or target (copy-array source))))
    (prog1 target
      (destructuring-bind (rows cols) (array-dimensions source)
        (dotimes (y rows)
          (dotimes (x cols)
            (setf (aref target y x) (funcall function source y x))))))))

(defun fold-grid (grid fold-fn accumulator)
  (destructuring-bind (rows cols) (array-dimensions grid)
    (dotimes (y rows accumulator)
      (dotimes (x cols)
        (setf accumulator (funcall fold-fn accumulator grid y x))))))

(defvar *neighbour-offsets*
  '((-1 -1)(+0 -1)(+1 -1)
    (-1 +0)       (+1 +0)
    (-1 +1)(+0 +1)(+1 +1)))

(defun map-neighbours (f g y x &key (offsets *neighbour-offsets*))
  (loop :for (dx dy) :in offsets
        :do (let ((x (+ x dx)) (y (+ y dy)))
             (when (array-in-bounds-p g y x)
               (funcall f (aref g y x))))))
