(defpackage #:abcl.test.ansi
  (:use :cl :cl-user)
  (:nicknames #:ansi-tests #:abcl-ansi-tests #:gcl-ansi)
  (:export #:run 
           #:verify-ansi-tests
           #:do-tests-matching
           #:load-tests
           #:clean-tests
           #:full-report
           #:report #:parse)
  ;; This should be REGRESSION-TEST included with the ANSI-TESTS, but
  ;; it is possible that the user may have included a slightly
  ;; different version from say Quicklisp.  
  (:import-from #:rt 
                #:pend #:name
                #:*entries* 
                #:do-test #:do-tests 
                #:do-entries))
