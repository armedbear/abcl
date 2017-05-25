;;;; -*- Mode: LISP -*-
(defsystem asdf-mvn-module
    :author "Alan Ruttenberg"
    :version "1.0.0"
    :depends-on (jss abcl-asdf)
    :description "Handles Maven artifact exclusions via the ADSF:MVN-MODULE component."
    :long-description "<urn:abcl.org/release/1.5.0/contrib/abcl-asdf/mvn-module#>"
    :version "1.0.0"
    :in-order-to ((test-op (test-op asdf-mvn-module-tests)))
    :components ((:module source
                          :pathname ""
                          :components ((:file "asdf-mvn-module")))))


