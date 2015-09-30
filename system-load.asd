#|
 This file is a part of system-load
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem system-load
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Getting at system load information from Lisp"
  :homepage "https://github.com/Shinmera/system-load"
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               #+linux (:file "linux")
               #-(or linux) (:file "unsupported")
               (:file "documentation"))
  :depends-on ())
