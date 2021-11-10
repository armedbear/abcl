;;;; -*- Mode: LISP -*-
(defsystem asdf-osgi-bundle
    :author "Alan Ruttenberg"
    :version "1.0.0"
    :depends-on (abcl-asdf)
    :long-description "<urn:abcl.org/release/1.8.0/contrib/abcl-asdf/asdf-osgi-bundle#>"
    :version "1.0.0"
    :components ((:module source :pathname ""
                  :components ((:file "osgi")
                               (:file "asdf-osgi-bundle")))))
