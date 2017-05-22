;;; Copy this file to "customizations.lisp"

;;; User customizations for the build.

;;; This file is LOADed by INITIALIZE-BUILD (in build-abcl.lisp).

;;; The variable *PLATFORM-IS-WINDOWS* should be true on Windows platforms. You
;;; can, of course, substitute your own test for this in the code below, or add
;;; a section for OS X, or Solaris, or whatever...

;;; You MUST set *JDK* to the location of the JDK you want to use. Remove or
;;; comment out settings that don't apply to your situation.

;;; You don't really need to specify anything but *JDK*. *JAVA-COMPILER* and
;;; *JAR* default to javac and jar, respectively, from the configured JDK.

;;; Directories should be specified with a trailing slash (or, on Windows, a
;;; trailing backslash).

(in-package :abcl/build)

;; Standard compiler options.
(defparameter *javac-options*
  "-g")
(defparameter *jikes-options*
  "+D -g")

(defparameter *jdk*
  (cond
    ((uiop:os-macosx-p) 
     "/usr/")
    (t
     (introspect-path-for "javac"))))





