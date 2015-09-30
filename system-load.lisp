#|
 This file is a part of system-load
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:system-load
  (:nicknames #:org.shirakumo.system-load)
  (:use #:cl)
  (:export
   #:cpu-usages
   #:cpu-usage
   #:ram-total
   #:ram-free
   #:ram-usage
   #:swap-total
   #:swap-free
   #:swap-usage
   #:mem-total
   #:mem-free
   #:mem-usage))
(in-package #:org.shirakumo.system-load)

(defun split (string splitter)
  (let ((parts ())
        (stream (make-string-output-stream)))
    (loop for char across string
          do (if (char= char splitter)
                 (let ((string (get-output-stream-string stream)))
                   (when (< 0 (length string))
                     (push string parts)))
                 (write-char char stream))
          finally (push (get-output-stream-string stream) parts))
    (nreverse parts)))

(defun /proc/stat ()
  (with-open-file (stream "/proc/stat" :direction :input)
    (loop for line = (read-line stream NIL NIL)
          for parts = (when line (split line #\ ))
          while line
          collect (destructuring-bind (part . args) parts
                    (cons (intern (string-upcase part) :keyword)
                          (mapcar #'parse-integer args))))))

(defun /proc/meminfo ()
  (with-open-file (stream "/proc/meminfo" :direction :input)
    (loop for line = (read-line stream NIL NIL)
          for parts = (when line (split line #\ ))
          while line
          collect (let ((name (subseq (first parts) 0 (1- (length (first parts)))))
                        (size (parse-integer (second parts))))
                    (cons (intern (string-upcase name) :keyword) size)))))

(defun cpustats (&optional (stats (/proc/stat)))
  (flet ((parse-cpustats (args)
           (loop for arg in args
                 for type in '(:user :nice :system :idle :iowait :irq :softirq :steal :guest :guest-nice)
                 collect (cons type arg))))
    (loop for (type . args) in stats
          when (and (<= 3 (length (string type)))
                    (string-equal "CPU" (string type) :end2 3))
          collect (cons type (parse-cpustats args)))))

(defun calculate-cpu-usage (a b)
  (flet ((fa (name) (cdr (assoc name a)))
         (fb (name) (cdr (assoc name b))))
    (let* ((aidle (+ (fa :idle) (fa :iowait)))
           (bidle (+ (fb :idle) (fb :iowait)))
           (anoidle (+ (fa :user) (fa :nice) (fa :system) (fa :irq) (fa :softirq) (fa :steal)))
           (bnoidle (+ (fb :user) (fb :nice) (fb :system) (fb :irq) (fb :softirq) (fb :steal)))
           (atotal (+ aidle anoidle))
           (btotal (+ bidle bnoidle))
           (total (- btotal atotal))
           (idle (- bidle aidle)))
      (if (= 0 total)
          0
          (float (/ (- total idle) total 0.01))))))

(defun cpu-usages (&key (sample 0.1) &allow-other-keys)
  (let ((a (cpustats))
        (b (progn (sleep sample) (cpustats))))
    (loop for (cpu0 . args0) in a
          for (cpu1 . args1) in b
          collect (cons cpu0 (calculate-cpu-usage args0 args1)))))

(defun cpu-usage (&optional core)
  (if core
      (cdr (assoc (princ-to-string core) (cpu-usages)
                  :key (lambda (a) (subseq (string a) 3)) :test #'string=))
      (cdr (assoc :cpu (cpu-usages)))))

(defun ram-total (&optional (stats (/proc/meminfo)))
  (cdr (assoc :memtotal stats)))

(defun ram-free (&optional (stats (/proc/meminfo)))
  (let* ((free (cdr (assoc :memfree stats)))
         (available (cdr (assoc :memavailable stats)))
         (cached (cdr (assoc :cached stats))))
    (if available available (+ free cached))))

(defun ram-usage (&optional (stats (/proc/meminfo)))
  (let* ((total (ram-total stats))
         (free (ram-free stats)))
    (float
     (/ (- total free) total 0.01))))

(defun swap-total (&optional (stats (/proc/meminfo)))
  (cdr (assoc :swaptotal stats)))

(defun swap-free (&optional (stats (/proc/meminfo)))
  (let* ((free (cdr (assoc :swapfree stats)))
         (cached (cdr (assoc :swapcached stats))))
    (+ free cached)))

(defun swap-usage (&optional (stats (/proc/meminfo)))
  (let* ((total (swap-total stats))
         (free (swap-free stats)))
    (float
     (/ (- total free) total 0.01))))

(defun mem-total (&optional (stats (/proc/meminfo)))
  (+ (swap-total stats) (ram-total stats)))

(defun mem-free (&optional (stats (/proc/meminfo)))
  (+ (swap-free stats) (ram-free stats)))

(defun mem-usage (&optional (stats (/proc/meminfo)))
  (let* ((total (mem-total stats))
         (free (mem-free stats)))
    (float
     (/ (- total free) total 0.01))))
