(in-package :abcl.test.lisp)

;;; When these bugs get fixed, they should be moved elsewhere in the
;;; testsuite so they remain fixed.

(deftest bugs.logical-pathname.1
    #|
Date: Mon, 18 Jan 2010 10:51:07 -0500
Message-ID: <29af5e2d1001180751l7cf79a3ay929cef1deb9ed063@mail.gmail.com>
Subject: Re: [armedbear-devel] translate-logical-pathname and :wild-inferiors 
regression
From: Alan Ruttenberg <alanruttenberg@gmail.com>
    |#
    (progn
      (setf (logical-pathname-translations "ido") 
            '(("IDO:IDO-CORE;**;*.*" 
               "/Users/alanr/repos/infectious-disease-ontology/trunk/src/ontology/ido-core/**/*.*") 
              ("IDO:IMMUNOLOGY;**;*.*"
               "/Users/alanr/repos/infectious-disease-ontology/trunk/src/ontology/immunology/**/*.*") 
              ("IDO:TOOLS;**;*.*" 
               "/Users/alanr/repos/infectious-disease-ontology/trunk/src/tools/**/*.*") 
              ("IDO:LIB;**;*.*"
               "/Users/alanr/repos/infectious-disease-ontology/trunk/lib/**/*.*")))
      (translate-pathname "IDO:IMMUNOLOGY;" "IDO:IMMUNOLOGY;**;*.*" 
                          "/Users/alanr/repos/infectious-disease-ontology/trunk/src/ontology/**/*.*"))
  #P"/users/alanr/repos/infectious-disease-ontology/trunk/src/ontology/")

(deftest bugs.logical.pathname.2
    #|
Message-Id: <BBE9D0E5-5166-4D24-9A8A-DC4E766976D1@ISI.EDU>
From: Thomas Russ <tar@ISI.EDU>
To: armedbear-devel@common-lisp.net
Subject: [armedbear-devel] Bug in translate-logical-pathname.
    |#
    (progn 
      (setf (logical-pathname-translations "L")
            '(("L:NATIVE;**;*.*" "/usr/lisp/abcl/native/**/*.*")))
      (translate-logical-pathname "L:NATIVE;TEST;FOO.FASL"))
  #p"/usr/lisp/abcl/native/test/foo.fasl")

      
