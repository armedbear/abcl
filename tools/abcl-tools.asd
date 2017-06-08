;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
(defsystem abcl-tools
  :version "0.1.0"
  :components ((:module src
                        :pathname ""
                        :components ((:file "digest")
                                     (:file "code-grapher")))))
                     
