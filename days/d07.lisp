(defpackage :aoc.07
  (:documentation "The Treachery of Whales")
  (:use :aoc)
  (:export #:test-part-1-example
           #:test-part-1
           #:test-part-2-example
           #:test-part-2))

(in-package :aoc.07)

(defun input (in)
  (etypecase in
    ((or number string) (all-integers (slurp-line in)))
    (vector (copy-seq in))))

(defvar *example* (all-integers "16,1,2,0,4,2,7,1,2,14"))

(defun analyze-input (seq)
  ;; compute min, max and total number of crabs in input
  (loop :for v :across seq
        :minimize v :into min
        :maximize v :into max
        :count v :into total
        :finally (return (values min max total))))

(defun prepare-input (seq)
  ;; build a vector of crab frequencies between min and max positions
  ;; including the empty water between crabs, as well as the total
  ;; count of crabs and the total fuel necessary for all crabs to
  ;; reach the leftmost position.
  (multiple-value-bind (min max total) (analyze-input seq)
    (let ((map (make-array (- (1+ max) min) :initial-element 0)))
      (loop :for pos :across seq :do (incf (aref map pos))
            :sum (- pos min) :into fuel-to-first
            :finally (return (values map total fuel-to-first))))))

(defun update-fuel (fuel nb-crabs-before nb-crabs-after)
  ;; update by implicit distance of 1 to the right.
  ;; all crabs on the left consume more fuel than before
  ;; all crabs on the right consume less fuel than before
  (+ fuel nb-crabs-before (- nb-crabs-after)))

(defun all-fuels (in)
  ;; in one pass from left to right, compute fuel for each position in
  ;; the input map based on the initial fuel necessary to reach the
  ;; leftmost position.
  (multiple-value-bind (map total fuel-to-first) (prepare-input in)
    (loop
      for last-amount = nil then amount
      for last-pos = nil then pos
      for pos below (length map)
      for amount across map
      for crabs-on-left = 0 then (+ crabs-on-left last-amount)
      for crabs-on-right = total then (- crabs-on-right last-amount)
      for fuel = fuel-to-first then (update-fuel fuel crabs-on-left crabs-on-right)
      minimize fuel into min-fuel
      finally (return min-fuel))))

(defun part-1 (in)
  (reduce #'min (all-fuels in)))

(define-test test-part-1-example
  (assert (= 37 (part-1 *example*))))

(define-test test-part-1
  (assert (= 333755 (part-1 (input 07)))))
