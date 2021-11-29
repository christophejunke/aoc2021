(in-package :aoc.grids)

(defmacro do-array ((vars &optional value result) array &body body)
  (if (symbolp vars)
      (%expand/dynamic vars value result array body)
      (%expand/static vars value result array body)))

