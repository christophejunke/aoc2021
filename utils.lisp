(defpackage #:aoc.utils
  (:use :cl :alexandria)
  (:export #:external-symbols
           #:defpackage/enum
           #:make-window
           #:adjust-window
           #:make-buffer
           #:buffer-push
           #:with-buffer
           #:with-buffer*
           #:run-length-encoding
           #:fold-hash-values
           #:rank
           #:push-to))

(in-package aoc.utils)

(defun find-package/error (package)
  (or (find-package package)
      (error "package not found: ~a" package)))

(defun external-symbols (&rest packages &aux symbols)
  (dolist (package packages symbols)
    (do-external-symbols (s (find-package/error package))
      (push s symbols))))

(defmacro defpackage/enum (enum-name &body body)
  (assert body)
  (destructuring-bind (doc . body) body
    (check-type doc string)
    (loop
      with p = (gensym (string enum-name))
      for index from 0
      for clause in body
      for (name doc . plist) = (ensure-list clause)
      for symb = (gensym "S")
      collect name into to-export
      collect `(let ((,symb (intern ,(string name) ,p)))
                 (setf (symbol-value ,symb) ,index)
                 (setf (documentation ,symb 'variable) ,doc)
                 (setf (symbol-plist ,symb) (list ,@plist)))
      into actions
      finally (return
                `(progn
                   (defpackage ,enum-name
                     (:documentation ,doc)
                     (:use)
                     (:export ,@to-export))
                   (let ((,p (find-package ',enum-name)))
                     (assert ,p)
                     ,@actions))))))

(defun make-buffer (&optional (element-type t) (size 128))
  (make-array (max 1 size)
              :element-type element-type
              :fill-pointer 0
              :adjustable t))

(defmacro push-to (place &aux (o (gensym)))
  `(lambda (,o) (push ,o ,place)))

(defun buffer-push (buffer value)
  (vector-push-extend value buffer (array-total-size buffer)))

(defmacro with-buffer ((b &rest make-buffer-args) &body body)
  (with-gensyms (v)
    `(let ((,b (make-buffer ,@make-buffer-args)))
       (prog1 ,b
         (flet ((,b (,v) (buffer-push ,b ,v)))
           (declare (inline ,b))
           ,@body)))))

(defmacro with-buffer* ((b &rest make-buffer-args) &body body)
  (with-gensyms (v)
    `(let ((,b (make-buffer ,@make-buffer-args)))
       (flet ((,b (,v) (buffer-push ,b ,v)))
         (declare (inline ,b))
         ,@body))))

(defun make-window (source &key (size 0) (offset 0))
  (make-array size
              :element-type (array-element-type source)
              :displaced-to source
              :displaced-index-offset offset))

(defun adjust-window (window &key (size 0 sp) (offset 0 op))
  (multiple-value-bind (source %offset) (array-displacement window)
    (assert source)
    (adjust-array window
                  (if sp size (length window))
                  :element-type (array-element-type source)
                  :displaced-to source
                  :displaced-index-offset (if op offset %offset))))

(defun run-length-encoding (seq &key (test #'eql))
  (with-buffer (buffer)
    (let ((sentinel (vector)))
      (declare (dynamic-extent sentinel))
      (let ((last sentinel) (last-count 0))
        (flet ((visit (node)
                 (cond
                   ((or (eq last sentinel)
                        (eq node sentinel)
                        (not (funcall test last node)))
                    (when (plusp last-count)
                      (buffer (cons last-count last)))
                    (setf last node)
                    (setf last-count 1))
                   (t (incf last-count)))))
          (map () #'visit seq)
          (visit sentinel))))))

(defun fold-hash-values (hash function accumulator)
  (flet ((fold (v) (setf accumulator (funcall function accumulator v))))
    (maphash-values #'fold hash)
    accumulator))

(defun rank (x list-designators)
  "First position where X can be found in LIST-DESIGNATORS.

   Each element in LIST-DESIGNATORS is
   either a list of values, or a single
   value. RANK returns the position of the
   first element that is either EQL to X or
   contains X.

   For example:

      (rank '^ '((+ -) (* /) ^)) => 2
      (rank '+ '((+ -) (* /) ^)) => 0
 "
  (or (position-if (lambda (v)
                     (typecase v
                       (list (find x v))
                       (t (eql x v))))
                   list-designators)
      (error "~a not found in ~a" x list-designators)))
