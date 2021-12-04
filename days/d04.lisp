(defpackage :aoc.04
  (:documentation "Giant Squid")
  (:use :aoc)
  (:export #:test-part-1-example
           #:test-part-1
           #:test-part-2-example
           #:test-part-2))

(in-package :aoc.04)

(defclass squid-game ()
  (;; Keep track of the location of a value in the grid, to make it
   ;; easier to know if a value exists in a board, and where.
   (map :initarg :map
        :initform (make-hash-table)
        :documentation "Map from bingo VALUE to position (COL . ROW)")
   ;; This value is initialized to the sum of all values in the board,
   ;; and decreases each time a value is crossed. This is sufficient
   ;; to compute the score.
   (total :initarg :total
          :documentation "Sum of all unmarked values in grid")
   ;; We know there will never be any duplicate entry, so adding a
   ;; number in grid necessarily increases the count of marks in the
   ;; respective column and row. As soon as the value reaches 5 (or,
   ;; more generally, the size of the opposite axis), we reach a
   ;; solution.
   (cols :documentation "Count marked value in each column")
   (rows :documentation "Count marked value in each row")))

(defmethod initialize-instance :after ((state squid-game) &key width height)
  (with-slots (cols rows) state
    (setf cols (make-array width))
    (setf rows (make-array height))))

(defmethod print-object ((s squid-game) stream)
  (print-unreadable-object (s stream :type t)
    (with-slots ((col cols) (row rows) (total total)) s
      (format stream "total:~d col:~a row:~a" total col row))))

(defun mark-value (state value)
  "Mark VALUE in STATE, return non-NIL score if BINGO reached"
  (with-slots (map total cols rows) state
    (when-let (entry (gethash value map))
      (destructuring-bind (col . row) entry
        (decf total value)
        (when (or (= (incf (aref cols col)) (length rows))
                  (= (incf (aref rows row)) (length cols)))
          (* value total))))))

(defun process-input (input function &optional (width 5) (height width))
  (with-input (in input)
    (let* ((numbers (all-integers (read-line in)))
           (state (make-instance 'squid-game :height height :width width)))
      (flet ((process-board (lines)
               ;; INIT MAP
               (with-slots (map total) state
                 (setf total 0)
                 (loop :with col :for row :from 0 :for line :in lines :do
                   (setf col 0)
                   (do-integers (int line)
                     (incf total int)
                     (setf (gethash int map) (cons col row))
                     (incf col))))
               ;; MARK UNTIL BINGO
               (loop
                 :for steps :from 0
                 :for number :across numbers
                 :for result := (mark-value state number)
                 :until result
                 :finally (funcall function steps result))
               ;; CLEAN FOR NEXT STEP
               (with-slots (map cols rows) state
                 (clrhash map)
                 (fill cols 0)
                 (fill rows 0))))
        (aoc:map-line-chunks in #'process-board)))))

(defun part-1 (in &aux bs br)
  (process-input in (lambda (s r)
                      (when (or (null bs) (< s bs))
                        (setf bs s br r))))
  (values br bs))

(define-test test-part-1-example
  (assert (= 4512 (part-1 "04-ex"))))

(define-test test-part-1
  (assert (= 41503 (part-1 04))))
