;; $Id: jl-config.cl,v 1.2 2005-10-25 14:52:53 piso Exp $

(in-package :cl-user)

(setf javatools.jlinker:*jlinker-java-home*
      #+linux "/home/peter/blackdown/j2sdk1.4.2"
      #+mswindows "C:\\Program Files\\Java\\jdk1.5.0_05")

(setf javatools.jlinker:*jlinker-run-java* 'javatools.jlinker::run-java)
