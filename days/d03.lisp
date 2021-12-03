(defpackage :aoc.03
  (:documentation "Binary Diagnostic")
  (:use :aoc)
  (:export #:test-part-1-example
           #:test-solve-1
           #:test-keep
           #:test-part-2-example
           #:test-solve-2))

(in-package :aoc.03)

;; The question is tricky w.r.t. what is the "FIRST" bit
;;
;; First bit is leftmost one ----.
;;                               |
;; INPUT                        "11110"
;;
;; This is also the most significant bit when decoded. If we reverse
;; the input string, the decoded value has the first bit in position
;; zero.
;;
;; REVERSE                      "01111"
;; DECODE                      #b01111 (15)
;;                                   |
;; First ----------------------------.
;;
;; This simplifies some computations when accessing bits from low to
;; high positions.
;;
;; For the output, let's assume that this "first" bit is mostly 1
;; across input numbers.
;;
;; The expected output binary number has a leftmost digit 1 (e.g. for gamma)
;;
;; EXPECTED                     "1....."
;;
;; In other words, the output binary number has its most significant bit set.
;; So it means the resulting bits are set in reverse order:
;;
;;    (SETF (LOGBITP (- SIZE <position> 1) OUT) <is-set>)
;;

;; COMMON

(defun input (in &aux size state)
  (labels ((initial-state (s)
             "Get length of string and decode"
             (setf size (length s))
             (setf state #'decode)
             (decode s))
           (decode (s)
             "Decode from right to left as number"
             (assert (= (length s) size))
             ;; reverse: lowest position is leftmost digit
             (parse-integer (nreverse s) :radix 2))
           (transform (s)
             "Call current state function"
             (funcall state s)))
    ;; state holds the current state function to call
    (setf state #'initial-state)
    (let ((bits (map-input in
                           :transform #'transform
                           :type '(vector fixnum *))))
      (values bits size))))

(defun fold-bitp-at-index (numbers index fold base)
  (reduce (lambda (a n) (funcall fold a (logbitp index n)))
          numbers
          :initial-value base))

(defun freq+ (freq bitp)
  (+ freq (if bitp 1 -1)))

(defun negate-bits (bits size)
  (mask-field (byte size 0)
              (lognot bits)))

(defun reverse-bits (s i)
  (loop
    with o = 0
    for > below s for < downfrom (1- s)
    do (setf (logbitp < o) (logbitp > i))
       finally (return o)))

(defun most-common-value-at-index (numbers index)
  "1, 0 or NIL if zeroes and ones in same quantity"
  (case (signum (fold-bitp-at-index numbers index #'freq+ 0))
    (+1 1)
    (-1 0)))

(defun least-common-value-at-index (numbers index)
  "1, 0 or NIL if zeroes and ones in same quantity"
  (case (signum (fold-bitp-at-index numbers index #'freq+ 0))
    (+1 0)
    (-1 1)))

;;;; PART 1

(defun part-1 (in)
  (multiple-value-bind (numbers size) (input in)
    (loop
      :with γ = 0
      :for i :below size
      :for o :downfrom (1- size)
      :for v := (most-common-value-at-index numbers i)
      :do (setf (logbitp o γ) (= v 1))
      :finally (let ((ε (negate-bits γ size)))
                 (return
                   (list (* γ ε) γ ε))))))

(define-test test-part-1-example
  (assert (equal '(198 22 9) (part-1 "03-ex"))))

(define-test test-solve-1
  (assert (equal '(3148794 3069 1026) (part-1 03))))

;;;; PART 2

(defun keep (index value numbers)
  (declare (type bit value))
  (let ((mask (ash 1 index)))
    (flet ((mask (v) (logand v mask)))
      (case value
        (0 (remove mask numbers :key #'mask))
        (1 (remove 0    numbers :key #'mask))))))

(defun apply-bit-criteria (numbers width bit-criteria)
  (declare (type vector numbers))
  (flet ((filter (n i) (keep i (funcall bit-criteria n i) n)))
    (loop
      for n = numbers then (filter n i)
      for i below width
      while (> (length n) 1)
      finally
         (assert (= (length n) 1))
         (return (aref n 0)))))

(defun O₂-bit-criteria (numbers index)
  (or (most-common-value-at-index numbers index) 1))

(defun CO₂-bit-criteria (numbers index)
  (or (least-common-value-at-index numbers index) 0))

(defun rating (n w criteria)
  (reverse-bits w (apply-bit-criteria n w criteria)))

(defun life-support-rating (input)
  (multiple-value-bind (n w) (input input)
    (* (rating n w #'O₂-bit-criteria)
       (rating n w #'CO₂-bit-criteria))))

(define-test test-part-2-example
  (assert (= 230 (life-support-rating "03-ex"))))

(define-test test-solve-2
  (assert (= 2795310 (life-support-rating 03))))

(define-test test-keep
  (flet ((expect (n v &rest r)
           (assert (equal r (keep n v '(#b010 #b100 #b101))))))
    (expect 2 1 #b100 #b101)
    (expect 2 0 #b010)
    (expect 1 1 #b010)
    (expect 1 0 #b100 #b101)
    (expect 0 1 #b101)
    (expect 0 0 #b010 #b100)))
