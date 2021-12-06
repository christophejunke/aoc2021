(defpackage :aoc.06
  (:documentation "Lanternfish")
  (:use :aoc)
  (:export #:test-part-1-update
           #:test-part-1-example
           #:test-part-1
           #:test-part-2-example
           #:test-part-2))

(in-package :aoc.06)

(defun make-state (seq &aux (state (make-array 9)))
  (prog1 state
    (map () (lambda (i) (incf (aref state i))) seq)))

(defun make-state-from-string (string)
  (make-state (all-integers string)))

(defun make-state-from-input (input)
  (with-input (in input)
    (make-state-from-string (read-line in))))

(defun update (state)
  (declare (type (vector integer 9) state))
  (prog1 state
    (let ((zero (shiftf (aref state 0)
                        (aref state 1)
                        (aref state 2)
                        (aref state 3)
                        (aref state 4)
                        (aref state 5)
                        (aref state 6)
                        (aref state 7)
                        (aref state 8)
                        0)))
      (incf (aref state 6) zero)
      (incf (aref state 8) zero))))

(defun all-states (input)
  (map-input input :transform #'make-state-from-string :type 'list))

(define-test test-part-1-update
  (loop
    for expected in (all-states "06-ex")
    for test = (copy-seq expected) then (update test)
    do (assert (equalp expected test))))

(defun simulate (in days)
  (let ((state (make-state-from-input in)))
    (loop repeat days do (update state))
    (reduce #'+ state)))

(define-test test-part-1-example
  (assert (= 5934 (simulate "06-ex" 80))))

(define-test test-part-1
  (assert (= 363101 (simulate 06 80))))

(define-test test-part-2-example
  (assert (= 26984457539 (simulate "06-ex" 256))))

(define-test test-part-2
  (assert (= 1644286074024 (simulate 06 256))))
