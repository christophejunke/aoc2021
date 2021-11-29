(defpackage :aoc.grids
  (:use :cl :alexandria)
  (:export
   ;; LISP ARRAY UTILITIES
   #:read-grid
   #:map-grid-into
   #:fold-grid
   #:*neighbour-offsets*
   #:map-neighbours
   #:do-array

   ;; INFINITE GRIDS
   #:make-infinite-grid
   #:copy-infinite-grid
   #:fold-infinite-grid
   #:map-into-infinite-grid
   #:iref))

