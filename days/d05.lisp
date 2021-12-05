(defpackage :aoc.05
  (:documentation "Hydrothermal Venture")
  (:use :aoc)
  (:export #:test-interpol
           #:test-part-1-example
           #:test-part-1
           #:test-part-2-example
           #:test-part-2))

(in-package :aoc.05)

(defun map-vent-lines (input function)
  (do-input-lines (line input)
    (scanner-bind ("%d,%d -> %d,%d" x1 y1 x2 y2) line
      (funcall function (complex x1 y1) (complex x2 y2)))))

(defun line-points (from to &optional (vec (- to from)))
  (loop
    :with d = (max (abs (imagpart vec))
                   (abs (realpart vec)))
    ;; due to puzzle constraints, U has only integer components
    :with u = (/ vec d)
    ;; we can interpolate without loss of precision
    :for n :from 0 :upto d :collect (+ from (* n u))))

(defun part-1 (input &aux (map (make-infinite-grid 0)) (total 0))
  (map-vent-lines input
                  (lambda (from to)
                    (let ((vec (- to from)))
                      (when (or (= 0 (imagpart vec))
                                (= 0 (realpart vec)))
                        (dolist (p (line-points from to vec))
                          (when (= 2 (incf (iref map p)))
                            (incf total)))))))
  total)

(defun part-2 (input &aux (map (make-infinite-grid 0)) (total 0))
  (map-vent-lines input
                  (lambda (from to)
                    (dolist (p (line-points from to))
                      (when (= 2 (incf (iref map p)))
                        (incf total)))))
  total)

(define-test test-interpol
  (assert (equal (line-points #C(10 0) #C(10 6))
                 '(#C(10 0) #C(10 1) #C(10 2) #C(10 3) #C(10 4) #C(10 5) #C(10 6))))
  (assert (equal (line-points #C(3 3) #C(7 7))
                 '(#C(3 3) #C(4 4) #C(5 5) #C(6 6) #C(7 7))))
  (assert (equal (line-points #C(3 3) #C(7 -1))
                 '(#C(3 3) #C(4 2) #C(5 1) #C(6 0) #C(7 -1))))
  (assert (equal (line-points #C(10 0) #C(10 1))
                 '(#C(10 0) #C(10 1))))
  (assert (equal (line-points #C(10 0) #C(10 1))
                 '(#C(10 0) #C(10 1)))))

(define-test test-part-1-example
  (assert (= 5 (part-1 "05-ex"))))

(define-test test-part-1
  (assert (= 7473 (part-1 05))))

(define-test test-part-2-example
  (assert (= 12 (part-2 "05-ex"))))

(define-test test-part-2
  (assert (= 24164 (part-2 05))))
