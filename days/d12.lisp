(cl:defpackage :aoc.12.lisp
  (:documentation "Passage Pathing")
  (:intern #:start #:end)
  (:use :aoc)
  (:export #:node))

(cl:defpackage :aoc.12.coalton
  (:documentation "Passage Pathing")
  (:use :coalton :coalton-library)
  (:export #:cave-graph
           #:walk-graph
           #:big-cave
           #:small-cave
           #:start
           #:end
           #:edges
           #:edge
           #:node
           #:part-1
           #:input))

(cl:defpackage :aoc.12
  (:documentation "Passage Pathing")
  (:use :aoc)
  (:import-from #:aoc.12.coalton
                #:walk-graph
                #:part-1
                )
  (:export #:test-part-1-example
           #:test-part-1
           #:test-part-2-example
           #:test-part-2))

(cl:in-package :aoc.12.coalton)

(coalton-toplevel
  (define-type cave
    (start)
    (end)
    (small-cave char)
    (big-cave char))
  (define-type edge
    (edge cave cave))
  (define-type cave-graph
    (edges (list edge))))

(cl:in-package :aoc.12.lisp)

(defun node (string)
  (cond
    ((string= string "start")
     (aoc.12.coalton:start))
    ((string= string "end")
     (aoc.12.coalton:end))
    ((upper-case-p (char string 0))
     (aoc.12.coalton:big-cave (char-downcase
                               (char string 0))))
    (t
     (aoc.12.coalton:small-cave (char string 0)))))

(cl:in-package :aoc.12.coalton)

(coalton-toplevel
  (declare input (string -> aoc.12.coalton:cave-graph))
  (define (input s)
    ;; bravely building a coalton object from lisp
    ;; (this is sound I promise)
    (lisp aoc.12.coalton:cave-graph (s)
      (cl:let (edges)
        (aoc:do-input-lines (line s (aoc.12.coalton:edges edges))
          (cl:destructuring-bind (from to) (split #\- line)
            (cl:push (aoc.12.coalton:edge (aoc.12.lisp:node from)
                                          (aoc.12.lisp:node to))
                     edges)
            (cl:push (aoc.12.coalton:edge (aoc.12.lisp:node to)
                                          (aoc.12.lisp:node from))
                     edges)))))))

(coalton-toplevel
  (define-instance (eq cave)
    (define (== c1 c2)
      (match (tuple c1 c2)
        ((tuple (start) (start)) true)
        ((tuple (end) (end)) true)
        ((tuple (small-cave a) (small-cave b)) (== a b))
        ((tuple (big-cave a) (big-cave b)) (== a b))
        (_ false))))
  (define (edge-from? f e)
    (match e ((edge from _to) (== f from)))))

(coalton-toplevel
  (define (successors graph origin)
    (match graph
      ((edges edges%)
       (fold (fn (edge successors)
               (match edge
                 ((edge from to)
                  (if (== from origin)
                      (cons to successors)
                      successors))
                 (_ successors))) 
             nil
             edges%))))
  (define (unvisited-neighbours graph node seen)
    (list-difference (successors graph node) seen)))

;; (coalton (successors (input "12-ex") start))
;; --> (#.(BIG-CAVE #\A) #.(SMALL-CAVE #\b))

;; (coalton (unvisited-neighbours (input "12-ex") start (make-list (big-cave #\a))))
;; --> (#.(SMALL-CAVE #\b))

(coalton-toplevel
  (define (maybe-mark seen node)
    (match node 
      ((big-cave _) seen)
      (_ (cons node seen))))
  (define (walk-graph graph node seen path all-paths)
    (let ((seen-0 (maybe-mark seen node)))
      (match node
        ((end) (reverse (cons (reverse (cons end path)) all-paths)))
        (_ (let ((around (unvisited-neighbours graph node seen-0)))
             (if (null around)
                 all-paths
                 (concatmap 
                  (fn (next)
                    (walk-graph graph next seen-0 (cons node path) all-paths))
                  around))))))))

(coalton-toplevel
  (define (part-1 in)
    (length (walk-graph (input in) start nil nil nil))))

(cl:in-package :aoc.12)

(defun make-interner (builder)
  (let ((hash (make-hash-table :test #'equal)))
    (lambda (name)
      (etypecase name
        ((eql :all) (alexandria:hash-table-values hash))
        (string
         (or (gethash name hash)
             (setf (gethash name hash) (funcall builder name))))))))

(defclass cave ()
  ((name :initarg :name :reader .name)
   (neighbours :initform nil :accessor .neighbours)))

(defclass small-cave (cave) ())
(defclass big-cave (cave) ())

(defun make-cave (name)
  (make-instance (if (every #'upper-case-p name)
                     'big-cave
                     'small-cave)
                 :name name))

(defmacro with-interner ((fn &rest args) &body body)
  (with-gensyms (fn-var arg)
    `(let ((,fn-var (make-interner ,@args)))
       (flet ((,fn (,arg) (funcall ,fn-var ,arg)))
         ,@body))))

(defun parse-caves (input)
  (with-interner (cave-intern #'make-cave)
    (do-input-lines (line input)
      (destructuring-bind (a b) (mapcar #'cave-intern (split #\- line))
        (push a (.neighbours b))
        (push b (.neighbours a))))
    (values (cave-intern "start")
            (cave-intern "end"))))

(defun walk-caves (start end &key (nearby #'.neighbours))
  (labels ((forbiddenp (cave path) 
             (typecase cave
               (big-cave nil)
               (small-cave (member cave path))))
           (nearby (cave) 
             (funcall nearby cave)))
    (let ((path-stack (list (list start))) (result 0))
      (loop
        (unless path-stack
          (return result))
        (let* ((path (pop path-stack)) (most-recent (first path)))
          (if (eql most-recent end)
              (incf result)
              (dolist (n (nearby most-recent))
                (unless (forbiddenp n path)
                  (push (cons n path)
                        path-stack)))))))))

(defmethod print-object ((o cave) stream)
  (print-unreadable-object (o stream :type t)
    (format stream "~s" (.name o))))

(define-test test-part-1-example
  (assert (= 10 (part-1 "12-ex"))))

;; control stack exhausted for part 1 using the naive coalton
;; implementation, works fine with imperative version

(define-test test-part-1
  (assert (= 3563 (multiple-value-bind (start end) (parse-caves 12)
                    (walk-caves start end)))))

;; PART 2

(defstruct env path once twice)

(defun walk-caves-bis (start end &key (nearby #'.neighbours))
  (flet
      ((nearby (cave)
         (funcall nearby cave))
       (maybe-augment-env (cave env)
         (with-accessors ((once env-once) (twice env-twice) (path env-path)) env
           (etypecase cave
             ;; big caves are always visited
             (big-cave
              (make-env :path (cons cave path) :once once :twice twice))
             ;; small caves are special
             (small-cave
              (and 
               ;; forbid if cave is already visited twice
               (not (eql cave twice))
               ;; otherwise, check the "once" list
               (let ((exists (member cave once))
                     (specialp (or (eql cave start) (eql cave end))))
                 ;; return nil, except in those cases:
                 (cond
                   ;; cave already visited, but a second visit is
                   ;; possible and cave is neither start or end
                   ((and exists (null twice) (not specialp))
                    (make-env :path (cons cave path)
                              :once once
                              :twice cave))
                   ;; cave not found, mark it as visited once
                   ((not exists)
                    (make-env :path (cons cave path)
                              :once (cons cave once)
                              :twice twice))))))))))
    (let ((env-stack (list (maybe-augment-env start (make-env)))) (result 0))
      (loop
        (unless env-stack
          (return result))
        (let* ((env (pop env-stack))
               (path (env-path env))
               (most-recent (first path)))
          (if (eql most-recent end)
              (incf result)
              (dolist (n (nearby most-recent))
                (when-let (env (maybe-augment-env n env))
                  (push env env-stack)))))))))

(defun part-2 (in)
  (multiple-value-bind (start end) (parse-caves in)
    (walk-caves-bis start end)))

(define-test test-part-2-example
  (assert (= 36 (part-2 "12-ex"))))

(define-test test-part-2
  (assert (= 105453 (part-2 "12"))))
