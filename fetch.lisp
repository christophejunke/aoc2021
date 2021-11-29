(defpackage :aoc.fetch
  (:use :cl :drakma)
  (:import-from #:local-time-duration
                local-time-duration:duration
                local-time-duration:timestamp-duration+)
  (:import-from #:local-time
                local-time:timestamp>=
                local-time:now
                local-time:timestamp-difference)
  (:export #:fetch-input))

(in-package :aoc.fetch)
  
(defun input-url (index)
  (format nil "https://adventofcode.com/2021/day/~d/input" index))

;; Once logged with the browser, it is possible to get the
;; necessary cookies (e.g. "Open as cURL", Network tab in
;; Chrome). My cookie file looks like this:
;;
;; _ga=....; _gid=....; _gat=...; session=...

(defvar *cookie-file*
  (merge-pathnames ".aoc-cookies" (user-homedir-pathname)))

(defvar *headers*
  `(("authority" . "adventofcode.com")
    ("cache-control" . "max-age=0")
    ("dnt" . "1")
    ("upgrade-insecure-requests" . "1")
    ("sec-fetch-site" . "same-origin")
    ("sec-fetch-mode" . "navigate")
    ("sec-fetch-user" . "?1")
    ("sec-fetch-dest" . "document")
    ("cookie" . ,(with-open-file (i *cookie-file*) (read-line i)))))

(defun aoc-input-stream (day-number)
  (http-request (input-url day-number)
                :additional-headers *headers*
                :want-stream t))

(defvar *input-base*
  (merge-pathnames "inputs/*.txt"
                   (asdf:system-source-directory "aoc")))

(defun input-pathname (in)
  (typecase in
    (number (input-pathname (format nil "~2,'0d" in)))
    (string (input-pathname (make-pathname :name in)))
    (pathname (merge-pathnames in *input-base*))))

(defparameter *next-fetch-time* nil)

(defparameter *fetch-limit* (duration :minute 15))

(defun check-fetch-time (now)
  (or (null *next-fetch-time*)
      (let ((dt (timestamp-difference *next-fetch-time* now)))
        (or (<= dt 0)
            (error "Next fetch allowed in ~a seconds" (ceiling dt))))))

(defun check-update-fetch-time (now)
  (when (check-fetch-time now)
    (setf *next-fetch-time* (timestamp-duration+ now *fetch-limit*))))

(defun check-if-file-exists/warn (path warnp)
  (when (probe-file path)
    (prog1 path
      (when warnp
        (warn "File already exists: ~a" path)))))

(defun fetch-input (day &optional (warnp nil))
  (let ((path (input-pathname day)))
    (prog1 path
      (unless (check-if-file-exists/warn path warnp)
        (when (check-update-fetch-time (now))
          (with-open-stream (i (aoc-input-stream day))
            (with-open-file (o path :direction :output)
              (uiop:copy-stream-to-stream i o))))))))
