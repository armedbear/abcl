;;;; -*- Mode: LISP -*-
(defsystem quicklisp-abcl
  :description "Load Quicklisp, installing from network if necessary."
  :long-name "<urn:abcl.org/release/1.8.0/contrib/quicklisp-abcl#>"
  :version "0.6.0"
  :components ((:file "quicklisp-abcl"))
  :perform (load-op :after (o c)
             (uiop:symbol-call :quicklisp-abcl 'ensure-installation)))







        
    
  


