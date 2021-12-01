(defpackage :aoc.01
  (:documentation "Sonar Sweep")
  (:use :aoc)
  (:export #:test-part-1-example
           #:test-solve-1
           #:test-part-2-example
           #:test-solve-2))

(in-package :aoc.01)

(defvar *example* '(199 200 208 210 200 207 240 269 260 263)
  "Example input for this puzzle")

;;;; PART 1

;;; First define a function over streams of integers that count how
;;; many times two successive values show an increase of value.  This
;;; uses the Z:DEFUN macro, which is necessary to accept SERIES as
;;; inputs.

(z:defun count-increasing-values (integers)
  (declare (z:optimizable-series-function)
           (type (z:series integer) integers)
           (z:off-line-port integers))
  ;; count how many elements satisfy the test
  (z:collect-length
   ;; retain only non-nil values
   (z:choose
    ;; test if the next value is greater than the current
    ;; for chunks of streams of size 2 sliding by 1 item each
    (z:mapping (((item next) (z:chunk 2 1 integers)))
               (> next item)))))

(define-test test-part-1-example
  (assert (= 7 (count-increasing-values
                (z:scan '(list integer) *example*)))))

(defun solve-1 ()
  (with-input (input 01)
    (count-increasing-values (z:scan-stream input))))

(define-test test-solve-1
  (assert (= 1466 (solve-1))))

;;;; PART 2

;;; SUM-OF-WINDOW-OF-THREE is another SERIES function that compute a
;;; CHUNK of size 3, sliding by a step of 1, over the input INTEGERS,
;;; and sum the values of each chunk to build another series of
;;; integers.

(z:defun sum-of-window-of-three (integers)
  (declare (z:optimizable-series-function)
           (type (z:series integer) integers)
           (z:off-line-port integers))
  (z:mapping (((a b c) (z:chunk 3 1 integers)))
             (+ a b c)))

;; (sum-of-window-of-three (z:scan 'list *example*))
;;  => #Z(607 618 618 617 647 716 769 792)


;; We can now produce first the stream of sums, and call
;; COUNT-INCREASING-VALUES on the resulting stream.

(define-test test-part-2-example
  (assert (= 5 (count-increasing-values
                (sum-of-window-of-three
                 (z:scan 'list *example*))))))

(defun solve-2 ()
  (with-input (input 01)
    (count-increasing-values
     (sum-of-window-of-three
      (z:scan-stream input)))))

(define-test test-solve-2
  (assert (= 1491 (solve-2))))
