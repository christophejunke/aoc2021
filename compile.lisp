(require 'asdf)

(let ((asd (merge-pathnames "aoc.asd" *load-pathname*)))
  (unless (probe-file asd)
    (error "cannot find asd?: ~s" asd))
  (asdf:load-asd asd)
  (asdf:compile-system :aoc :verbose t :force t))

