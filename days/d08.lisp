(defpackage :aoc.08
  (:documentation "Seven Segment Search")
  (:use :aoc)
  (:export #:test-part-1-example
           #:test-part-1
           #:test-part-2-example
           #:test-part-2))

(in-package :aoc.08)

;; PARSING

(defun parse-line (line)
  (mapcar (lambda (c)
            (mapcar (lambda (w) (sort w #'char<))
                    (all-matches-as-strings "\\w+" c)))
          (split #\| line)))

;; PART 1

(defun 1478-digit-p (s)
  (member (length s) '#.(mapcar #'length '("dgebacf" "cgb" "gcbe" "gc"))))

(defun part-1 (in)
  (let ((total 0))
    (do-input-lines (line in total)
      (destructuring-bind (patterns output) (parse-line line)
        (declare (ignore patterns))
        (incf total (count-if #'1478-digit-p output))))))

(define-test test-part-1-example
  (assert (= 26 (part-1 "08-ex"))))

(define-test test-part-1
  (assert (= 476 (part-1 "08"))))

;; PART 2

(defvar *abc-pattern*
  ;;
  ;; Original segments are named as follows:
  ;;
  ;;       a
  ;;      ---
  ;;   b |   | c
  ;;      -d-
  ;;   e |   | f
  ;;      ---
  ;;       g
  ;;
  ;; Each digit from 0 to 9 is encoded in the table below, with
  ;; character # marking a segment as being set and a space character
  ;; as unset:
  ;;
  ;; ·------------- digits
  ;; |  abcdefg <-- segments
  ;; v  |||||||
  '((9 "#### ##") ;                        a
    (8 "#######") ;                       ###
    (7 "# #  # ") ;                    b #     c
    (6 "## ####") ;--> 6 for example:     #d#
    (5 "## # ##") ;                    e #   # f
    (4 " ### # ") ;                       ###
    (3 "# ## ##") ;                        g
    (2 "# ### #") ;
    (1 "  #  # ") ;
    (0 "### ###")))

;; Each above pattern has an associated byte pattern on 7 bits: for
;; example the pattern for digit 6 is "## ####", and its associated
;; byte is #b1111011 (notice this is flipped horizontally, index 0
;; corresponds to least significand bit).

(defun build-digit-table (patterns)
  "From the digit table above, build an array, indexed by the number
   of segments being set, of alists: each entry maps a digit to a byte
   pattern"
  (loop
    :with buckets = (make-array (1+ (length "ABCDEFG")) :initial-element ())
    :for (digit pattern) in patterns
    :for byte = (loop with byte = 0 for i from 0 for c across pattern
                      do (setf (logbitp i byte) (char= c #\#))
                      finally (return byte))
    :do (push (cons digit byte)
              (aref buckets (logcount byte)))
    :finally (return buckets)))

(defvar *digits-by-size* (build-digit-table *abc-pattern*)
  "Vector, indexed by number of segments in digits,
   of digits and their byte patterns")

(defun digit-from-word-and-byte (word byte)
  (car (rassoc byte (aref *digits-by-size* (length word)))))

(defun char-variable (char)
  "Variables are represented by a number (0 -> A, 1 -> B, etc)"
  (or (position char "abcdefg")
      (error "invalid char ~a" char)))

(defun string-variables (word)
  "Map each char to a number representing a variable"
  (sort (map 'list #'char-variable word) #'<))

(defun other-variables (variables)
  "Complementary set of segment variables"
  (set-difference '(0 1 2 3 4 5 6) variables))

(defgeneric filter (cstr env)
  (:documentation "Filtering constraint, return a new env"))

(defparameter *dbg* nil "Debugging allowed")

(defmethod filter :around (c e)
  (let ((n (call-next-method)))
    (prog1 n
      (when *dbg*
        (print `(filter :cstr ,c :in ,e :out ,n))))))

(defgeneric vars (cstr)
  (:documentation "Variables constrained by constraint"))

(defgeneric split-constraint (cstr env)
  (:documentation
   "Partition constraint into one or more environments,
    all of them covering a sub-problem to solve.
    Return NIL if constraint cannot be expanded.")
  (:method (c e) nil))

(defun byte-array (size)
  (make-array size
              :element-type `(unsigned-byte ,size)
              :initial-element (mask-field (byte size 0) -1)))

;; The state is only stored in a single type of object, the
;; environment (env), in a purely immutable way: new environments are
;; created whenever state changes, and slots share structure with past
;; environments when possible.

(defclass env ()
  (;; Array mapping variables to their domains. We can afford for this
   ;; problem to represent variables with just numbers: 0 stands for
   ;; the segment variable A, 1 is variable B, etc. Here, we also only
   ;; consider the "segment domain" for each variable, ie. a byte that
   ;; indicates, for each bit position N, if the associated variable
   ;; can be segment N.
   (v-dom :initarg :v-dom
          :initform (byte-array 7)
          :reader .v-dom
          :documentation "Domains for all variables")
   ;; Suspended constraints are constraints that will be filtered on
   ;; some condition: for example in our problem, when any variable
   ;; associated with the constraint is instantiated to a ground
   ;; value, the constraint is woken up and added to list of next
   ;; goals (see below). In a real system we could wake up on
   ;; variables being constrained (their domain shrink), or on
   ;; triggers.
   ;;
   ;; Here CSTRS is an array mapping each variable to all the
   ;; constraints waiting for its value to be known: the same
   ;; (identical) constraint may belong to multiple buckets, they are
   ;; however woken only once.
   (cstrs :initarg :cstrs
          :initform (make-array 9 :initial-element nil)
          :reader .cstrs
          :documentation "Suspension lists for each variables")
   ;; Facts gathered during constraint solving, in particular this is
   ;; where we store the mapping from an input word to its digit, when
   ;; we know definitely which one it is.
   (facts :initarg :facts
          :initform nil
          :reader .facts
          :documentation "Facts added when solving")
   ;; Temporary informations gathered when filtering, events like
   ;; "such variable was instantiated to a ground value". Infos are
   ;; not retained from one environment to another.
   (infos :initarg :infos
          :initform nil
          :reader .infos
          :documentation "Events collected when solving")
   ;; New goals (constraints) to filter
   (goals :initarg :goals
          :initform nil
          :reader .goals
          :documentation "Remaining goals to execute")
   ;; When *dbg* is non-nil, keep track of the past environment, this
   ;; helped a lot when trying to understand how the state
   ;; evolves. When *dbg* is nil, this slot is unbound.
   (%from :initarg :from
          :reader .from
          :documentation "From what current env evolved")))

(defparameter *env* (make-instance 'env)
  "Initial environment")

(defmethod print-object ((e env) stream)
  (print-unreadable-object (e stream :type t :identity nil)
    (format stream "~a" (.v-dom e))))

(defun dom   (var env) (aref (.v-dom env) var))
(defun cstrs (var env) (aref (.cstrs env) var))

(defun update-vec (vec fn &aux (out (copy-seq vec)))
  "Call FN with arguments (POS VEC) for all positions POS and value VAL in VEC"
  (prog1 out
    (loop for x across vec for i from 0 do
      (setf (aref out i) (funcall fn i x)))))

(defun update-v-dom (env fn)
  "Call FN with arguments (VAR DOM) for all vars VAR and its domain DOM in ENV.
   FN shall return the new domain for VAR, and possibly a FLAG as a
   secondary return value. When a flag is returned, it is noted in the
   new environment's INFOS field."
  (let* ((infos (.infos env))
         (v-dom (update-vec
                 (.v-dom env)
                 (lambda (v d)
                   (multiple-value-bind (nd flag) (funcall fn v d)
                     (prog1 nd
                       (when flag
                         (push `(:var ,v :is ,flag) infos))))))))
    (update-env env
                :v-dom v-dom
                :infos infos)))

(defun unify-dom (d1 d2)
  "Intersection of segment domains D1 with D2.
   Return as a secondary value NIL, :INSTANTIATED or :CONSTRAINED
   depending on whether the domain is respectively the same as D1,
   represents a single (ground) value, or has a smaller size than D1."
  (let* ((lg0 (logcount d1))
         (new (logand d1 d2))
         (lg1 (logcount new)))
    (values new
            (cond
              ((= lg0 lg1) nil)
              ((= 0 lg1)   (fail :inst d1 d2))
              ((= 1 lg1)   :instantiated)
              (t           :constrained)))))

(defun make-unifier (vars domain)
  (lambda (v d) (if (member v vars) (unify-dom d domain) d)))

(defun fail (&rest d)
  (throw 'fail d))

(defun update-env (env &key
                         (v-dom (.v-dom env)) (cstrs (.cstrs env))
                         (facts (.facts env)) (infos (.infos env))
                         (goals (.goals env)))
  "Copy and change some fields in ENV to build new environment."
  (make-instance 'env
                 :v-dom v-dom
                 :cstrs cstrs
                 :facts facts
                 :infos infos
                 :goals goals
                 :from (and *dbg* env)))

(defun env () (update-env *env*))

(defun add-fact (fact env)
  (update-env env :facts (cons fact (.facts env))))

(defun add-info (info env)
  (update-env env :infos (cons info (.infos env))))

(defun add-goals (env &rest goals)
  (update-env env :goals (append goals (.goals env))))

(defun suspend (constraint env)
  "Suspend CONSTRAINT in ENV, return new ENV"
  (let ((vars (vars constraint)))
    (update-env env
                :cstrs (update-vec (.cstrs env)
                                   (lambda (v old)
                                     (if (member v vars)
                                         (cons constraint old)
                                         old))))))

(defun wake (var env)
  "Wake all constraints attached to VAR in ENV, return new ENV."
  (let ((var-constrants (cstrs var env)))
    (flet ((already-woken (c) (member c var-constrants)))
      (update-env env
                  :cstrs (update-vec
                          (.cstrs env)
                          (lambda (v suspended)
                            (unless (= v var)
                              (remove-if #'already-woken suspended))))
                  :goals (append var-constrants (.goals env))))))

(defun %remove-constraint (constraint env)
  ;; remove suspended constraint from env
  (let ((vars (vars constraint)))
    (update-env env
                :cstrs (update-vec (.cstrs env)
                                   (lambda (v suspended)
                                     (if (member v vars)
                                         (remove constraint suspended)
                                         suspended))))))

(defun all-suspensions (env)
  "All current suspended constraints in ENV"
  (delete-duplicates
   (loop for suspensions across (.cstrs env) append suspensions)))

;; FILTER CONSTRAINTS

(defun filter* (env &aux todo)
  "Repeatedly filter ENV until reaching a fixpoint where no further
   reduction can happen"
  (loop
    (with-accessors ((goals .goals)
                     (v-dom .v-dom)
                     (infos .infos)
                     (cstrs .cstrs))
        env
      (cond
        ;; SOME CONSTRAINTS EXISTS IN GOALS
        (goals
         ;; move goals to todo list (new goals happen before older todo items)
         (setf todo (append goals todo))
         (setf env (update-env env :goals nil)))
        ;; SOME CONSTRAINTS REMAIN IN TODO LIST
        (todo
         ;; filter one constraint
         (setf env (filter (pop todo) env)))
        ;; THERE ARE SOME INFOS TO PROCESS
        (infos
         (dolist (info infos)
           (ematch info
             ((list :var var :is flag)
              (ecase flag
                ;; wake up constraints attached to VAR
                (:instantiated (setf env (wake var env)))
                (:constrained)))))
         ;; clean infos
         (setf env (update-env env :infos nil)))
        (t
         ;; nothing to do, return
         (return env))))))

(defun solutionp (env)
  "True if ENV represents a final state with a solution"
  (and (not (.goals env))
       (notany #'identity (.cstrs env))
       (not (.infos env))
       (every (lambda (d) (= 1 (logcount d))) (.v-dom env))))

;; SPLIT SEARCH SPACE

(defgeneric split-score (constraint)
  (:documentation "Score to pick the next constraint to split")
  (:method (c) 0))

(defun select-constraint (env)
  "Select the next constraint in ENV to split"
  (caar (sort (loop
                for constraint in (all-suspensions env)
                collect (cons constraint (split-score constraint)))
              #'>
              :key #'cdr)))

;; SOLVER

(defun solve (env &key (fuel 200))
  "Solver loop: filter and search for solutions"
  (loop
    (unless (plusp (decf fuel))
      ;; safety measure to avoid infinte loops
      (break))
    ;; FILTER* ENV UNTIL NO CONSTRAINT CAN BE FILTERED
    (setf env (filter* env))
    ;; RETURN IF WE FOUND A SOLUTION
    (when (solutionp env)
      (return env))
    ;; SELECT A SUSPENDED CONSTRAINT
    (let ((constraint (select-constraint env)))
      (when constraint
        ;; REMOVE IT, AND SPLIT SPACE INTO SUB-PROBLEMS
        ;; A solution for the sub-problem is also a solution for the
        ;; current problem.
        (setf env (%remove-constraint constraint env))
        (let ((new-envs (split-constraint constraint env)))
          (unless new-envs (fail :split-empty))
          ;; TRY EACH POSSIBLE ENVIRONMENT, ON FAILURE BACKTRACK
          (dolist (env new-envs)
            (catch 'fail
              (return-from solve (solve env)))))
        ;; NO ENV WAS A SUITABLE SOLUTION, FAIL HERE TOO
        (fail)))))

;; ALL-VARS-MATCH-ONE-OF

(defstruct (all-vars-match-one-of
            (:constructor all-vars-match-one-of (string vars domains))
            (:conc-name avm-))
  string vars domains)

(defmethod vars ((c all-vars-match-one-of))
  (avm-vars c))

(defmethod filter ((c all-vars-match-one-of) env)
  (with-accessors ((avmv avm-vars) (avmd avm-domains) (word avm-string)) c
    (labels ((unsatp (v d) (= 0 (logand (dom v env) d)))
             (remove-unsat (avmd avmv)
               (remove-if (lambda (d) (some (lambda (v) (unsatp v d)) avmv))
                          avmd))
             (single-dom (dom)
               ;; only domain possible
               (add-fact `(:word-digit ,word ,(digit-from-word-and-byte word dom))
                         (add-goals (update-v-dom env (make-unifier avmv dom))
                                    (none-belongs-to (other-variables avmv) dom)))))
      (case (length avmd)
        (0 (fail c 'empty))
        ;; only one domain possible
        (1 (single-dom (aref avmd 0)))
        ;; check if all vars still sat
        (t (let ((navmd (remove-unsat avmd avmv)))
             (case (length avmd)
               (0 (fail c 'any-incompatible-var))
               (1 (single-dom (aref navmd 0)))
               (t
                ;; or suspend on subset of vars
                (suspend (all-vars-match-one-of word avmv navmd) env)))))))))

(defmethod split-score ((c all-vars-match-one-of))
  (- 10 (length (avm-domains c))))

(defmethod split-constraint ((c all-vars-match-one-of) env)
  "Consider each possible domain separately"
  (with-accessors ((vars avm-vars) (word avm-string)) c
    (map 'list
         (lambda (d)
           (add-goals env (all-vars-match-one-of word vars (vector d))))
         (avm-domains c))))

;; ALL-DIFFERENT

(defstruct (all-different (:constructor all-different (vars))) vars)
(defmethod vars ((c all-different)) (all-different-vars c))

(defmethod filter ((c all-different) env &aux (vars (vars c)))
  "Remove instantiated values from set of possible values in remaining variables"
  (cond
    ((rest vars)
     (loop
       :with mask = 0
       :for v :in vars :for d = (dom v env)
       :if (= (logcount d) 1) :do
         (setf mask (logior mask d))
       :else
         :collect v :into rest
       :finally
          (return
            (let ((env (update-v-dom env (make-unifier rest (lognot mask)))))
              (suspend (all-different rest) env)))))
    (t env)))

;; NONE-BELONGS-TO

(defstruct (none-belongs-to (:constructor none-belongs-to (vars domains)))
  vars domains)
(defmethod vars ((c none-belongs-to)) (none-belongs-to-vars c))

(defmethod filter ((c none-belongs-to) env)
  "Filter out a domain from a set of variables"
  (with-accessors ((mask none-belongs-to-domains) (vars vars)) c
    (update-v-dom env (make-unifier vars (lognot mask)))))

;; MORE SPECIFIC DEFINITIONS FOR OUR PROBLEM

(defun candidate-domains (word)
  "All possible segment domains for a word, given its size"
  (map 'vector #'cdr (aref *digits-by-size* (length word))))

(defun word-constraint (w)
  "Constraints associated with a word"
  (let ((vars (string-variables w))
        (doms (candidate-domains w)))
    (list
     (all-vars-match-one-of w vars doms))))

(defun constraints (words)
  "Environment for our problem, given a list of words"
  (update-env *env*
              :goals
              (list* (all-different '(0 1 2 3 4 5 6))
                     (mapcan #'word-constraint words))))

(defun decode-number (solution words)
  "Given a solution, decode list of words as digits"
  (let ((map (make-hash-table :test #'equal)))
    ;; Build mapping from words to digits (from FACTS in solution)
    (dolist (f (.facts solution))
      (match f ((list :word-digit w d) (setf (gethash w map) d))))
    ;; Convert words, build a number
    (loop
      for w in words
      for d = (or (gethash w map)
                  (error "not found!: ~a in ~a" w map))
      for n = d then (+ d (* 10 n))
      finally (return n))))

(defun solve-line (line)
  "Solve a line of the puzzle"
  (destructuring-bind (in out) (parse-line line)
    (decode-number (solve (constraints in)) out)))

(defun part-2 (in &aux (sum 0))
  (do-input-lines (line in sum)
    (incf sum (solve-line line))))

(define-test test-part-2-example
  (assert (= (part-2 "08-ex") 61229)))

(define-test test-part-2
  (assert (= (part-2 "08") 1011823)))
