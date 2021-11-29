(defpackage :aoc.00
  (:use :aoc)
  (:export #:test))

(in-package :aoc.00)

(in-readtable :fare-quasiquote)

(define-test test
  ;; pattern matching
  (assert (= 6 (match '(1 2 3) (`(,a ,b ,c) (+ a b c)))))

  ;; testing basic input
  (do-input-lines (line "00-test")
    (assert (string= line "test")))

  ;; testing fold-input-lines
  (labels ((direction (string)
             (aref #(#c(0 -1) #c(1 0) #C(0 1) #C(-1 0))
                   (position (char string 0) "NESW")))
           (fold-line (line position)
             (register-groups-bind ((#'direction direction)
                                    (#'parse-integer steps))
                 ('(:sequence letter int) line)
               (check-type direction number)
               (+ position (* direction steps)))))
    (assert (= (fold-input-lines "00-fold" #'fold-line 0)
               #C(30 20)))))
