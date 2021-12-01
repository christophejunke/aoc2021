(cl:defpackage :aoc.00.coalton
  (:use #:coalton #:coalton-library)
  (:export #:test-coalton))

(cl:in-package #:aoc.00.coalton)

(coalton-toplevel
  (define-type (Tree :a)
    (Node (Tree :a) (Tree :a) :a)
    (Leaf :a))

  (declare fold-tree ((Tree :a) -> (:b -> :a -> :b) -> :b -> :b))
  (define (fold-tree tree red-fn init)
    (match tree
      ((Node left right value)
       (fold-tree right
                  red-fn
                  (red-fn
                   (fold-tree left red-fn init)
                   value)))
      ((Leaf value)
       (red-fn init value))))

  (define example (Node (Leaf 3)
                        (Leaf 5)
                        4))
  (define (items acc v)
    (Cons v acc))

  (define test-coalton
    (fold-tree example items Nil)))

(cl:defpackage :aoc.00
  (:use :aoc :aoc.00.coalton)
  (:export #:test))

(cl:in-package :aoc.00)

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
               #C(30 20)))

    (assert test-coalton)))
