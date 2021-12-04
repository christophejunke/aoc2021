(defsystem :aoc
  :depends-on (:aoc.requirements)
  :serial nil
  :components ((:file "utils")
               (:file "fetch")
               (:module #:GRIDS
                :depends-on ()
                :pathname "grids"
                :serial nil
                :components ((:file "package")
                             (:file "array-grids"
                              :depends-on ("package"))
                             (:file "utils" 
                              :depends-on ("package"))
                             (:file "macros")
                             (:file "hash-grids"
                              :depends-on ("macros" "package"))))
               (:file "package" :depends-on ("utils" "fetch" #:grids))
               (:module #:PARSING
                :depends-on ("package")
                :pathname ""
                :serial t
                :components ((:file "lexer")
                             (:file "scanner")))
               (:file "inputs" :depends-on ("package" "setup"))
               (:file "tests" :depends-on ("package"))
               (:file "setup" :depends-on ("package" "tests"))
               (:module #:DAYS
                :depends-on ("setup" "inputs" #:parsing)
                :pathname "days"
                :serial nil
                :components ((:file "d00")
                             (:file "d01")
                             (:file "d02")
                             (:file "d03")
                             (:file "d04")))))
