(in-package :aoc)

(defun call-with-input (in function)
  (typecase in
    (stream (funcall function in))
    (t (with-open-file (stream (fetch-input in nil))
         (funcall function stream)))))

(defmacro with-input ((stream name) &body body)
  `(call-with-input ,name (lambda (,stream) ,@body)))

(defmacro do-input-lines ((line name &optional result) &body body)
  (with-gensyms (stream)
    `(with-input (,stream ,name)
       (loop :for ,line := (read-line ,stream nil nil)
             :while ,line
             :do (progn ,@body)
             :finally (return ,result)))))

(defmacro map-input (in &key (transform nil) (reader '#'read-line) (type ''vector))
  (with-gensyms (s)
    (let ((scanner `(z:scan-stream ,s ,reader)))
      `(with-input (,s ,in)
         (z:collect ,type
           ,(if transform
                `(z:map-fn t ,transform ,scanner)
                scanner))))))

(defun fold-input-lines (input function &optional accumulator)
  (do-input-lines (line input accumulator)
    (setf accumulator (funcall function line accumulator))))

(defun slurp-line (input)
  (with-input (s input)
    (read-line s)))

(defmacro do-integers ((var string &optional result) &body body)
  (with-gensyms (Z S E)
    `(let ((,Z ,string))
       (do-matches (,S ,E '(:greedy-repetition 1 () :digit-class) ,Z ,result)
         (let ((,var (parse-integer ,Z :start ,S :end ,E)))
           ,@body)))))

(defun map-all-integers (string function)
  (do-integers (integer string)
    (funcall function integer)))

(defun all-integers (string)
  (with-buffer (b)
    (map-all-integers string #'b)))

(defun map-line-chunks (in function &aux stack)
  "Read consecutive non-empty lines and call FUNCTION on their concatenation."
  (flet ((emit ()
           (when stack
             (let ((lines (nreverse (shiftf stack nil))))
               (funcall function lines)))))
    (do-input-lines (line in (emit))
      (if (string= line "")
          (emit)
          (push line stack)))))
