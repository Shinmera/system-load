#|
 This file is a part of system-load
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem system-load
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Getting at system load information from Lisp"
  :homepage "https://Shinmera.github.io/system-load/"
  :bug-tracker "https://github.com/Shinmera/system-load/issues"
  :source-control (:git "https://github.com/Shinmera/system-load.git")
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               #+linux (:file "linux")
               #-(or linux) (:file "unsupported")
               (:file "documentation"))
  :depends-on ())
