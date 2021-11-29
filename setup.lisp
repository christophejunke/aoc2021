(in-package :aoc)

(defpackage :z
  (:use :series)
  (:export . #.(external-symbols :series)))

(unless *kernel*
  (setf *kernel*
        (make-kernel 8
                     :name "aoc"
                     :bindings nil)))
