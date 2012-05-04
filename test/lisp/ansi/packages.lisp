(defpackage #:abcl.test.ansi
  (:use :cl :cl-user)
  (:nicknames #:ansi-tests #:abcl-ansi-tests #:gcl-ansi)
  (:export #:run 
           #:verify-ansi-tests
	   #:load-tests
	   #:clean-tests
           #:full-report
	   #:report #:parse)
  (:import-from #:rt #:do-test #:do-test #:do-tests))



		   
	     

