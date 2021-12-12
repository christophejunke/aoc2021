(defpackage :aoc.09
  (:documentation "Smoke Basin")
  (:use :aoc)
  (:export #:test-part-1-example
           #:test-part-1
           #:test-part-2-example
           #:test-part-2))

(in-package :aoc.09)

(defvar *neighbours*
  '(       (+0 -1)
    (-1 +0)       (+1 +0)
           (+0 +1)       ))

(defun input (input)
  (with-input (in input)
    (aoc:read-grid in :element-type '(integer 0 (9))
                      :transform #'digit-char-p)))

(defun part-1 (input)
  (fold-grid (input input)
             (lambda (total g y x)
               (block nil
                 (let ((v (aref g y x)))
                   (map-neighbours (lambda (w &key)
                                     (when (<= w v)
                                       (return total)))
                                   g y x :offsets *neighbours*)
                   ;; all neighbours > to v
                   (let ((risk-factor (1+ v)))
                     (+ total risk-factor)))))
             0))

(defun flood-fill (grid y x &aux basin (todo (list (cons y x))))
  (let ((seen (make-hash-table :test #'equal)))
    (flet ((seen (coord) 
             (gethash coord seen))
           (mark (coord) 
             (push coord basin)
             (setf (gethash coord seen) t)))
      (loop
        (unless todo
          (return basin))
        (let ((coord (pop todo)))
          (unless (seen coord)
            (mark coord)
            (destructuring-bind (y . x) coord
              (let ((v (aref grid y x)))
                (map-neighbours (lambda (w &key ((:x nx)) ((:y ny)))
                                  (when (< v w 9)
                                    (push (cons ny nx) todo)))
                                grid y x :offsets *neighbours*)))))))))

(defun at-most (n list)
  (ldiff list (nthcdr n list)))

(defun fold-basins (best basin)
  (at-most 3 (merge 'list (list (length basin)) best #'>)))

(defun three-largest (input)
  (fold-grid (input input)
             (lambda (basins g y x)
               (block nil
                 (let ((v (aref g y x)))
                   (map-neighbours (lambda (w &key)
                                     (when (<= w v)
                                       (return basins)))
                                   g y x :offsets *neighbours*)
                   (fold-basins basins (flood-fill g y x)))))
             nil))

(defun part-2 (input)
  (reduce #'* (three-largest input)))

(define-test test-part-1-example
  (assert (= 15 (part-1 "09-ex"))))

(define-test test-part-1
  (assert (= 591 (part-1 "09"))))

(define-test test-part-2-example
  (assert (= 1134 (part-2 "09-ex"))))

(define-test test-part-2
  (assert (= 1113424 (part-2 "09"))))
