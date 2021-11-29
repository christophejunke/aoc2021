(defpackage :aoc
  (:use . #1=(:cl :alexandria :ppcre :trivia
              :named-readtables :esrap :lparallel
                  :aoc.fetch :aoc.utils :aoc.grids))
  (:export #:fold-input-lines
           #:do-input-lines
           #:with-input
           #:slurp-line
           #:map-line-chunks
           #:map-input
           #:int
           #:word
           #:letter
           #:decode-format
           #:scanner-bind
           #:scan-as-values
           #:scan-as-list
           #:read-grid
           #:map-grid-into
           #:fold-grid
           #:*neighbour-offsets*
           #:map-neighbours
           #:define-test
           #:test-all
           .
           #.(aoc.utils:external-symbols . #1#)))
