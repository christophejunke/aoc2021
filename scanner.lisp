(in-package :aoc)

;; for simplicity this relies on a regex but this kind of format could
;; be implemented differently.

(defun %scanner-body (safe variables decoders body format tree input)
  (unless safe
    (assert (= (length variables) (length decoders))
            (variables)
            "Invalid number of variables ~a for given format ~s"
            variables format))
  (flet ((bind (decoder variable) `((function ,decoder) ,variable)))
    (let ((pairs (mapcar #'bind decoders variables)))
      `(register-groups-bind ,pairs ((load-time-value
                                      (let ((*use-bmh-matchers* t))
                                        (create-scanner ',tree)))
                                     ,input
                                     :sharedp t)
         ,@body))))

(defmacro scanner-bind ((format &rest variables) input &body body)
  (multiple-value-bind (tree decoders) (decode-format format)
    (%scanner-body nil variables decoders body format tree input)))

(flet ((scan-expand (format input body-fn)
         (multiple-value-bind (tree decoders) (decode-format format)
           (let* ((variables (loop for _ in decoders collect (gensym)))
                  (body (funcall body-fn variables)))
             (%scanner-body t variables decoders body format tree input)))))

  (defmacro scan-as-values (format input)
    (scan-expand format input (lambda (v) `((values ,@v)))))

  (defmacro scan-as-list (format input)
    (scan-expand format input (lambda (v) `((list ,@v))))))
