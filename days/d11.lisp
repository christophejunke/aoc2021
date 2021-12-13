(defpackage :aoc.11
  (:documentation "Dumbo Octopus")
  (:use :aoc)
  (:export #:test-part-1-example
           #:test-part-1
           #:test-part-2-example
           #:test-part-2))

(in-package :aoc.11)

(defmacro just (&body body)
  "Lambda that ignores all arguments and executes body"
  (with-gensyms (args)
    `(lambda (&rest ,args)
       (declare (ignore ,args))
       ,@body)))

(defun grid (in)
  (with-input (input in)
    (read-grid input
               :element-type '(integer 0 (9))
               :transform #'digit-char-p)))

;; Define a signal named FLASH that indicates a FLASH event happened
;; at position X, Y in the current grid. The fact that signals can
;; broadcast an even across the call stack is being exploited here to
;; both update the grid and count how many total flashes were
;; happening during multiple steps.

(define-condition flash ()
  ((x :initarg :x :reader .x)
   (y :initarg :y :reader .y)))

(defun increment-energy (v y x)
  (let ((w (mod (1+ v) 10)))
    (prog1 w
      (when (= w 0)
        ;; When reaching 0, emit a FLASH signal
        (signal 'flash :x x :y y)))))

(defun update-energy-at (grid y x)
  (increment-energy (aref grid y x) y x))

(defun update-energy* (grid-in grid-out)
  "Increment energy across all cells of the grid"
  (map-grid-into grid-out #'update-energy-at grid-in))

(defun update-step (in out)
  (let ((seen (make-hash-table :test #'equal)))
    (with-buffer* (todo)
      ;; on-flash, mark coords as visited and them to todo vector
      (flet ((on-flash (&rest yx) (setf (gethash yx seen) t) (todo yx)))
        ;; call on-flash every time a FLASH condition happens
        (handler-bind ((flash (lambda (c) (on-flash (.y c) (.x c)))))
          ;; first, do an UPDATE-ENERGY* step to make the first flashes
          (let ((out (update-energy* in out)))
            ;; This is our output, by the way
            (prog1 out
              ;; Propagate flashes until TODO is empty
              (loop :while (> (length todo) 0) :do
                (destructuring-bind (y x) (vector-pop todo)
                  (flet ((spread (v &key x y)
                           (unless (gethash (list y x) seen)
                             ;; unless already done, increment energy
                             ;; this, too, may signal a FLASH condition
                             (setf (aref out y x) (increment-energy v y x)))))
                    ;; For all neighbourghs of Y, X, call SPREAD
                    (map-neighbours #'spread out y x)))))))))))

(defun N-steps (N grid)
  "Run N steps of UPDATE-STEP with double-buffering"
  (labels ((update (N grid-in grid-out)
             (cond ((= 0 N) grid-in)
                   ((> N 0) (update (1- N)
                                    ;; output is the new input
                                    (update-step grid-in
                                                 grid-out)
                                    ;; input is the new output
                                    grid-in)))))
    ;; First output is NIL, which allocates a fresh output grid
    (update N grid nil)))

;; PART 1

(defun part-1 (in &aux (counter 0))
  ;;
  ;; "decline v. (of a handler) to return normally without having
  ;; handled the condition being signaled, permitting the signaling
  ;; process to continue as if the handler had not been present."
  ;;
  ;; UPDATE-STEP's handler declines, so the FLASH signal propagates up
  ;; the call stack. This is a way of broadcasting data, which is used
  ;; here to increment a global counter of FLASH events that bubbles
  ;; up from N-steps.
  ;;
  (handler-bind ((flash (just (incf counter))))
    (N-steps 100 (grid in))
    counter))

(define-test test-part-1-example
  (assert (= 1656 (part-1 "11-ex"))))

(define-test test-part-1
  (assert (= 1634 (part-1 "11"))))

;; PART 2

(define-condition start-step () ())

(defun update-forever (grid)
  ;; We do the same here for part 2, but here UPDATE-FOREVER never
  ;; terminates by itself, a caller has to handle one of START-STEP or
  ;; FLASH signals by altering the control flow.
  (labels ((up (i o) (signal 'start-step) (up (update-step i o) i)))
    (up grid nil)))

(defun part-2 (in &aux (grid (grid in)))
  (let ((total (array-total-size grid)) (step 0) (flashes))
    (handler-bind ((start-step
                     ;; on new steps, reinitialize the total FLASH
                     ;; counter, and increment STEP
                     (just
                       (setf flashes total)
                       (incf step)))
                   (flash
                     ;; on a FLASH condition, decreases FLASHES, and
                     ;; if all cells did flash at the current step,
                     ;; return from the current function with
                     ;; STEP. This is how the handler is said to
                     ;; "handle" the condition, but altering the
                     ;; control flow.
                     (just
                       (when (= 0 (decf flashes))
                         (return-from part-2 step)))))
      ;; run forever while reacting on signals
      (update-forever grid))))

(define-test test-part-2-example
  (assert (= 195 (part-2 "11-ex"))))

(define-test test-part-2
  (assert (= 210 (part-2 "11"))))
