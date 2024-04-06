;;;; <https://github.com/armedbear/abcl/issues/656>
#|
(parse-namestring "./foo") evaluates to #p"foo", instead of #p"./foo".

Most Lisp implementation evaluate to #p"./foo", but I don't think this is in the standard.

ABCL supports current directory in paths though, using make-pathname:

(make-pathname :directory '(:relative ".") :name "foo") => #P"./foo".

So it is only parse-namestring that would need to be modified.
|#

(prove:plan 1)

(prove:is 
 (pathname-directory (parse-namestring "./foo"))
 '(:relative ".")
 "Whether the namestring of current directory explicitly roundtrips")


;; (describe #p"./foo")
#| ABCL
#P"foo" is an object of type PATHNAME:
  HOST         NIL
  DEVICE       NIL
  DIRECTORY    NIL
  NAME         "foo"
  TYPE         NIL
  VERSION      NIL
; No value                              ;
|# 

#| SBCL
#P"./foo"
  [pathname]

  HOST       = #<SB-IMPL::UNIX-HOST {70031676C3}>
  DIRECTORY  = (:RELATIVE ".")
  NAME       = "foo"
  VERSION    = :NEWEST
; No value
|#

#| CCL
P"./foo"
Type: PATHNAME
Class: #<BUILT-IN-CLASS PATHNAME>
TYPE: (PATHNAME . #<CCL::CLASS-WRAPPER PATHNAME #x30004035E6DD>)
%PATHNAME-DIRECTORY: (:RELATIVE ".")
%PATHNAME-NAME: "foo"
%PATHNAME-TYPE: NIL
%PHYSICAL-PATHNAME-VERSION: :NEWEST
%PHYSICAL-PATHNAME-DEVICE: NIL
|#

#| ECL
#<pathname  0x107138140>
--------------------
A pathname.
Namestring: "foo"
Host: NIL
Device: NIL
Directory: (:RELATIVE)
Name: "foo"
Type: NIL
Version: :NEWEST

|#

(prove:finalize)
