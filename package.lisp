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
