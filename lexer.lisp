(in-package :aoc)

(define-parse-tree-synonym int
    (:register
     (:sequence
      (:greedy-repetition 0 1 (:char-class #\- #\+))
      (:greedy-repetition 1 nil :digit-class))))

(defun %int (d)
  `(:register
    (:sequence
     (:greedy-repetition ,d ,d :digit-class))))

(define-parse-tree-synonym letter
    (:register :word-char-class))

(define-parse-tree-synonym word
    (:register
     (:sequence
      :word-boundary
      (:greedy-repetition 1 nil :word-char-class)
      :word-boundary)))

(defun %word (d)
  `(:register
    (:sequence
     :word-boundary
     (:greedy-repetition 1 ,d :word-char-class))))

(defun map-tokens (callback stream &key (sharedp nil) (debugp nil))
  (declare (type (function (t)) callback)
           (type stream stream))
  (let ((buffer (make-array 256
                            :fill-pointer 0
                            :element-type 'character
                            :adjustable t))
        (length-param 0)
        (state-fn #'values)
        (buffer-copy (if sharedp #'identity #'copy-seq)))
    (declare (type function state-fn)
             (type string buffer)
             (type function buffer-copy))
    ;; helper functions
    (labels ((feed-fsm (c)
               (when debugp
                 (print `(:feed ,state-fn ,c) *trace-output*))
               (funcall state-fn c))
             (switch-to (s &optional c)
               (setf state-fn s)
               (when c
                 (feed-fsm c)))
             (clear ()
               (setf length-param 0)
               (setf (fill-pointer buffer) 0))
             (buffer (c)
               (vector-push-extend c buffer (array-total-size buffer)))
             (emit (token-type &rest components)
               (apply callback token-type components))
             (finish-literal-token ()
               (when (> (length buffer) 0)
                 (emit :literal (funcall buffer-copy buffer)))
               (clear)))
      ;; fsm state functions
      (labels ((dispatch (c)
                 (case c
                   (#\%
                    (switch-to #'maybe-escape))
                   (t (buffer c))))
               (maybe-escape (c)
                 (case c
                   (#\% (buffer c)
                        (switch-to #'dispatch))
                   (t (finish-literal-token)
                      (switch-to #'await-type c))))
               (await-length-param (c)
                 (if-let (d (digit-char-p c))
                   (setf length-param (+ (* length-param 10) d))
                   (switch-to #'await-type c)))
               (await-type (c)
                 (flet ((emit/dispatch (&rest token)
                          (apply #'emit token)
                          (switch-to #'dispatch)))
                   (case c
                     ((#\i #\d) (emit/dispatch :integer length-param))
                     (#\c (emit/dispatch :character))
                     (#\s (emit/dispatch :word length-param))
                     (#\S
                      (if (eql #\( (peek-char nil stream nil nil))
                          (destructuring-bind (package) (read stream)
                            (emit/dispatch :symbol length-param package))
                          (emit/dispatch :symbol length-param *package*)))
                     (t (cond
                          ((digit-char-p c)
                           (switch-to #'await-length-param c))
                          (t (error "unexpected %~a sequence" c))))))))
        ;; lexer
        (loop
           :initially (switch-to #'dispatch)
           :for c := (read-char stream () ())
           :while c
           :do (feed-fsm c)
           :finally (finish-literal-token))))))

(defun find-symbol! (package)
  (let ((package (etypecase package
                   (string (find-package package))
                   (package package))))
    `(lambda (name)
       (let ((name (string-upcase name)))
         (or (find-symbol name ,package)
             (error "symbol not found in package ~a: ~a" 
                    ,package
                    name))))))

(defun as-regex-node (token)
  (ematch token
    ((list :integer d)
     (if (= d 0)
         (values 'int 'parse-integer)
         (values (%int d) `(lambda (s) (parse-integer s :end ,d)))))
    ((list :character)      (values 'letter 'first-elt))
    ((list :word d)
     (values (if (= d 0) 'word (%word d)) 'identity))
    ((list :symbol d p)
     (values (if (= d 0) 'word (%word d)) (find-symbol! p)))
    ((list :literal string) string)))

(defun decode-format (format)
  (let (regex-tree decoders)
    (with-input-from-string (stream format)
      (map-tokens (lambda (&rest token)
                    (multiple-value-bind (node decoder) (as-regex-node token)
                      ;; decoder iff a variable is needed
                      (push node regex-tree)
                      (when decoder
                        (push decoder decoders))))
                  stream))
    (values `(:sequence ,@(nreverse regex-tree))
            (nreverse decoders))))
