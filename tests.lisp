(in-package :aoc)

(defmacro define-test (name &body body)
  `(progn
     (defun ,name ()
       ,@body)
     (eval-when (:compile-toplevel :execute)
       (setf (get ',name 'test) t)
       (setf (get ',name 'dirty) t))))

(defun unit-test (i s f)
  (list i s (nth-value 1 (ignore-errors (funcall f)))))

(defun format-result (res)
  (with-standard-io-syntax
    (let ((*print-right-margin* most-positive-fixnum))
      (loop
        for (s e) in (mapcar #'cdr (sort res #'> :key #'car))
        for n = (if (string= s "TEST") "" (format nil " > ~a" s))
        for p = (package-name (symbol-package s))
        for d = (or (documentation (symbol-package s) t) "")
        when e collect s into symbols
        collect n into snames
        collect p into pnames
        collect e into errors
        collect d into docstr
        maximize (length n) into max-s
        maximize (length p) into max-p
        maximize (length d) into max-d
        finally
           (flet ((fmt (s p e d)
                    (format *trace-output*
                            "~&> ~v@a (~va)~va~:[~; > ~:*~a~]~%"
                            max-d d
                            max-p p
                            max-s s
                            e)))
             (map () #'fmt snames pnames errors docstr)
             (return
               (and symbols (lambda () (map () #'funcall symbols)))))))))

(defun %test-all-in-packages (packages &key (force nil) (all nil))
  (let ((packages (ensure-list packages))
        (channel (make-channel))
        (res)
        (pass t)
        (submitted 0))
    (with-package-iterator (next packages :internal :external)
      (loop
        (multiple-value-bind (continue symbol access) (next)
          (unless continue (return))
          (when-let ((function (and (or all (eq access :external))
                                    (get symbol 'test)
                                    (fboundp symbol)
                                    (symbol-function symbol))))
            (when (or force (get symbol 'dirty))
              (submit-task channel
                           #'unit-test
                           (incf submitted)
                           symbol
                           function))))))
    (do-fast-receives (result channel submitted)
      (push result res)
      (destructuring-bind (symbol error) (rest result)
        (if error
            ;; some error
            (setf pass nil)
            ;; do not test it until it is redefined
            (setf (get symbol 'dirty) nil))))
    (values pass res)))

;; AoC specific code

(defun aoc-package (day)
  (find-package (format nil "AOC.~2,'0d" day)))

(defun aoc-packages ()
  (loop
    for day from 25 downto 0
    for pkg = (aoc-package day)
    when pkg collect pkg))

(defun test-all (&key
                 (force nil)
                 (all nil)
                 (time nil)
                 (packages (aoc-packages))
                 (result-cb #'format-result))
  (let ((lock (load-time-value (bt:make-lock))))
    (bt:with-lock-held (lock)
      (flet ((test () (%test-all-in-packages packages :force force :all all)))
        (multiple-value-bind (pass res) (if time (time (test)) (test))
          (values pass (funcall result-cb res)))))))

