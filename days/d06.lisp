(defpackage :aoc.06
  (:documentation "Lanternfish")
  (:use :aoc)
  (:export #:test-part-1-update
           #:test-part-1-example
           #:test-part-1
           #:test-part-2-example
           #:test-part-2))

(in-package :aoc.06)

(defun make-state-from-string (string)
  (make-state (all-integers string)))

(defun make-state-from-input (input)
  (with-input (in input)
    (make-state-from-string (read-line in))))

(defun make-state (seq &aux (state (make-array 9)))
  (map () (lambda (i) (incf (aref state i))) seq)
  (let ((list (coerce state 'list)))
    (nconc list list)))

(defun update (state)
  (let* ((zero (pop state))
         (sixth (nthcdr 6 state)))
    (incf (car sixth) zero)
    (setf (caddr sixth) zero)
    state))

(defun finite (infinite)
  (subseq infinite 0 9))

(defun simulate (in days &aux (time (get-internal-real-time)))
  (loop
    :for state = (make-state-from-input in) :then (update state)
    :for d :below days
    :do (when (= 0 (rem d 1000))
          (let ((now (get-internal-real-time)))
            (when (> (- now time)
                     #.(* 5 internal-time-units-per-second))
              (setf time now)
              (print d)
              (finish-output))))
    :finally (return (reduce #'+ (finite state)))))

(defun all-states (input)
  (map-input input :transform #'make-state-from-string :type 'list))

(defun copy-state (state)
  (let ((finite (finite state)))
    (nconc finite finite)))

(define-test test-part-1-update
  (loop
    for expected in (all-states "06-ex")
    for test = (copy-state expected) then (update test)
    do (assert (equalp (finite expected) (finite test)))))

(define-test test-part-1-example
  (assert (= 5934 (simulate "06-ex" 80))))

(define-test test-part-1
  (assert (= 363101 (simulate 06 80))))

(define-test test-part-2-example
  (assert (= 26984457539 (simulate "06-ex" 256))))

(define-test test-part-2
  (assert (= 1644286074024 (simulate 06 256))))
