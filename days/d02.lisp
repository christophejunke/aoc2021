(defpackage :aoc.02
  (:documentation "Dive!")
  (:use :aoc)
  (:intern up down forward)
  (:import-from #:coalton
                #:->)
  (:export #:test-part-1-example
           #:test-solve-1
           #:test-part-2-example
           #:test-solve-2
           #:test-coalton-1
           #:test-coalton-2))

(in-package :aoc.02)

(defvar *example*
  "forward 5
down 5
forward 8
up 3
down 8
forward 2")


;; Pure Lisp

(defun part-1 (&optional (in 02) &aux (horiz 0) (depth 0))
  (do-input-lines (line in (* horiz depth))
    (scanner-bind ("%S %d" direction delta) line
      (ecase direction
        (forward (incf horiz delta))
        (up      (decf depth delta))
        (down    (incf depth delta))))))

(define-test test-part-1-example
  (assert (= 150 (with-input-from-string (in *example*)
                   (part-1 in)))))

(define-test test-part-1
  (assert (= 2215080 (part-1))))

(defun part-2 (&optional (in 02) &aux (aim 0) (horiz 0) (depth 0))
  (do-input-lines (line in (* horiz depth))
    (scanner-bind ("%S %d" direction delta) line
      (ecase direction
        (forward 
         (incf horiz delta)
         (incf depth (* aim delta)))
        (up
         (decf aim delta))
        (down
         (incf aim delta))))))

(define-test test-part-2-example
  (assert (= 900 (with-input-from-string (in *example*)
                   (part-2 in)))))

(define-test test-part-2
  (assert (= 1864715580 (part-2))))

;; Coalton

(c:coalton-toplevel
  (c:define-type piloting-command
    (up c:Integer)
    (down c:Integer)
    (forward c:Integer)))

(c:coalton-toplevel
  (c:define-type coord-1
    (hor-dep c:integer c:integer)))

(c:coalton-toplevel
  (c:define-type coord-2
    (hor-dep-aim c:integer c:integer c:integer)))

(c:coalton-toplevel
  (c:define (move-1 position command)
    (c:match position
      ((hor-dep horizontal depth)
       (c:match command
         ((up n)      (hor-dep horizontal (coalton-library:- depth n))) 
         ((down n)    (hor-dep horizontal (coalton-library:+ depth n)))
         ((forward n) (hor-dep (coalton-library:+ horizontal n) depth)))))))

(c:coalton-toplevel
  (c:define (move-2 position command)
    (c:match position
      ((hor-dep-aim horizontal depth aim)
       (c:match command
         ((up n)      (hor-dep-aim horizontal depth (coalton-library:- aim n))) 
         ((down n)    (hor-dep-aim horizontal depth (coalton-library:+ aim n)))
         ((forward n) (hor-dep-aim (coalton-library:+ horizontal n)
                                   (coalton-library:+ depth 
                                                      (coalton-library:* aim n))
                                   aim)))))))

(defun %fold-commands (folder accumulator)
  (do-input-lines (line 02 accumulator)
    (scanner-bind ("%S %d" direction distance) line
      (let ((data (funcall direction distance)))
        (setf accumulator (funcall folder accumulator data))))))

(c:coalton-toplevel
  (c:define (test-fold a c) "a"))

;; okay, so interop

;; this works
;; (%fold-commands #'test-fold "") => "a"

;; but, calling (c:coalton (fold-commands test-fold "")) doesn't with
;; a naive definition, because (c:coalton test-fold) evaluates to a
;; structure (FUNCTION-ENTRY).
;;
;; Fortunately, #'test-fold is also a valid function, so I can pass
;; the Lisp object, wrapped in a type that keeps track of the desired
;; type

(c:coalton-toplevel
  (c:define-type (lisp-fn :a)
    (lfn :a)))

(c:coalton-toplevel
  ;; accept a wrapper for a folder function
  (c:declare fold-commands
             ((lisp-fn (:a -> piloting-command -> :a)) -> :a -> :a))
  (c:define (fold-commands fn acc)
    (c:match fn
      ((lfn lisp-fun)
       (c:lisp :a (lisp-fun acc)
         (%fold-commands lisp-fun acc))))))

;; btw, this does not work because c:type-of is not a type expression
;; suitable for lisp, otherwise I could write (make-lfn test-fold) and
;; have the type associated with the lisp-fun wrapping #'test-fold be
;; the same as the type of the coalton function test-fold.
;;
;; (defmacro make-lfn (function)
;;   `(c:coalton (lfn (c:lisp ,(c:type-of function) () (function ,function)))))

;; (c:coalton (fold-commands
;;             (lfn (c:lisp (:a -> :b -> c:String) () #'test-fold))
;;             "a"))
;; => "a"

;; okay, so now, define coord-X types

(c:coalton-toplevel
  ;; a typeclass for things that can be converted to coord-1
  (c:define-class (Res :a)
    (as-coord-1 (:a -> coord-1)))
  
  ;; trivial implementation for coord-1
  (c:define-instance (Res coord-1)
    (c:define (as-coord-1 x) x))
  
  ;; projection implementation for coord-2
  (c:define-instance (Res coord-2)
    (c:define (as-coord-1 x)
      (c:match x
        ((hor-dep-aim h d _) (hor-dep h d)))))
  
  ;; compute result for any type matching the typeclass
  (c:define (extract-result result)
    (c:match (as-coord-1 result)
      ((hor-dep h d) (coalton-library:* h d)))))

(define-test test-coalton-1
  (assert (= 2215080
             (c:coalton
              (extract-result
               (fold-commands
                (lfn (c:lisp (coord-1 -> piloting-command -> coord-1) () #'move-1))
                (hor-dep 0 0)))))))


(define-test test-coalton-2
  (assert (= 1864715580
             (c:coalton
              (extract-result
               (fold-commands
                (lfn (c:lisp (coord-2 -> piloting-command -> coord-2) () #'move-2))
                (hor-dep-aim 0 0 0)))))))

