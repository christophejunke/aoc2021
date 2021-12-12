(defpackage :aoc.10
  (:documentation "Syntax Scoring")
  (:use :aoc)
  (:export #:test-part-1-example
           #:test-part-1
           #:test-part-2-example
           #:test-part-2))

(in-package :aoc.10)

(define-condition has-char ()
  ((char :reader .char :initarg :char)))

(define-condition corrupted (serious-condition has-char) ())
(define-condition incomplete (serious-condition has-char) ())

(defun call-with-modified-readtable (fn &key (chars '("{}" "[]" "<>" "()")))
  (let ((*readtable* (copy-readtable)))
    (labels ((bad-character (end)
               (lambda (&rest rest)
                 (declare (ignore rest))
                 (error 'corrupted :char end)))
             (read-until (end)
               (lambda (stream &rest rest)
                 (declare (ignore rest))
                 (handler-bind ((end-of-file
                                  (lambda (c)
                                    (declare (ignore c))
                                    (signal 'incomplete :char end))))
                   (read-delimited-list end stream))))
             (update (s)
               (assert (= 2 (length s)))
               (let ((beg (aref s 0))
                     (end (aref s 1)))
                 (set-macro-character beg (read-until end))
                 (set-macro-character end (bad-character end)))))
      (map () #'update chars)
      (funcall fn))))

(defmacro with-modified-readtable ((&rest args) &body body)
  `(call-with-modified-readtable (lambda () ,@body) ,@args))

(defun part-1-points (character)
  (ecase character
    (#\) 3)
    (#\] 57)
    (#\} 1197)
    (#\> 25137)))

(defun part-1 (input &aux (total 0))
  (with-modified-readtable ()
    (do-input-lines (line input total)
      (handler-case (read-from-string line)
        (incomplete () #| ignore |#)
        (corrupted (c)
          (incf total (part-1-points (.char c))))))))

(defun middle-of (vector &aux (size (length vector)))
  (assert (not (= 0 (mod size 2))) () "Expects a vector of odd size")
  (aref (sort vector #'<)
        (floor (/ (length vector) 2))))

(define-test test-middle-of
  (assert (= 1 (middle-of #(1))))
  (assert (= 2 (middle-of #(1 2 3))))
  (assert (= 3 (middle-of #(1 2 3 4 5))))
  (assert (not (ignore-errors (middle-of #(1 2 3 4))))))

(defun part-2-points (character)
  (1+ (or (position character ")]}>")
          (error "Invalid char ~a" character))))

(defun part-2 (input)
  (with-modified-readtable ()
    (middle-of
     (with-buffer (buffer)
       (do-input-lines (line input)
         (block :current-line
           (let ((score 0))
             (handler-bind
                 ((incomplete
                    (lambda (c)
                      (setf score
                            (+ (* score 5)
                               (part-2-points (.char c))))))
                  (end-of-file
                    (lambda (c)
                      (declare (ignore c))
                      (buffer score)
                      (return-from :current-line)))
                  (corrupted
                    (lambda (c)
                      (declare (ignore c))
                      (return-from :current-line))))
               (read-from-string line)))))))))

(define-test test-part-1-example
  (assert (= 26397 (part-1 "10-ex"))))

(define-test test-part-1
  (assert (= 362271 (part-1 "10"))))

;; PART 2

(define-test test-part-2-example
  (assert (= 288957 (part-2 "10-ex"))))

(define-test test-part-2
  (assert (= 1698395182 (part-2 "10"))))
