(in-package :abcl.test.lisp)

;;; When these bugs get fixed, they should be moved elsewhere in the
;;; testsuite so they remain fixed.

(deftest bugs.translate-logical-pathname
    #|
    Date: Mon, 18 Jan 2010 10:51:07 -0500
    Message-ID: <29af5e2d1001180751l7cf79a3ay929cef1deb9ed063@mail.gmail.com>
    Subject: Re: [armedbear-devel] translate-logical-pathname and :wild-inferiors 
    regression
    From: Alan Ruttenberg <alanruttenberg@gmail.com>
    |#
    (progn
      (setf (logical-pathname-translations "ido") 
            '((#P"IDO:IDO-CORE;**;*.*" 
               #P"/Users/alanr/repos/infectious-disease-ontology/trunk/src/ontology/ido-core/**/*.*") 
              (#P"IDO:IMMUNOLOGY;**;*.*"
               #P"/Users/alanr/repos/infectious-disease-ontology/trunk/src/ontology/immunology/**/*.*") 
              (#P"IDO:TOOLS;**;*.*" 
               #P"/Users/alanr/repos/infectious-disease-ontology/trunk/src/tools/**/*.*") 
              (#P"IDO:LIB;**;*.*"
               #P"/Users/alanr/repos/infectious-disease-ontology/trunk/lib/**/*.*")))
      (translate-pathname #P"IDO:IMMUNOLOGY;" #P"IDO:IMMUNOLOGY;**;*.*" 
                          #P"/Users/alanr/repos/infectious-disease-ontology/trunk/src/ontology/**/*.*"))
  #P"/users/alanr/repos/infectious-disease-ontology/trunk/src/ontology/immunology/")