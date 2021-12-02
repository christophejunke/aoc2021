(in-package :aoc)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'series::defun :series)
  (export 'series::let :series)
  (export 'series::let* :series))

(defpackage :z
  (:use :series)
  (:export . #.(external-symbols :series)))

(unless *kernel*
  (setf *kernel*
        (make-kernel 8
                     :name "aoc"
                     :bindings nil)))

(rename-package :coalton :coalton (list :c))
