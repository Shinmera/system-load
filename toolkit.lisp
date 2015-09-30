#|
 This file is a part of system-load
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

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
