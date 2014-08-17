;;; This file is part of ABCL contrib
;;;
;;; Copyright 2011 Mark <evenson@panix.com>

(defpackage #:asdf-jar
  (:use :cl)
  (:export #:package 
           ;; "Si vis pacem, para bellum" -- Publius Flavius Vegetius Renatus
           #:prepare-for-war 
           #:add-to-asdf))

(in-package #:asdf-jar)

(defvar *debug* nil)

(defun add-system-files-to-mapping! (system
                                     mapping
                                     system-base
                                     system-name
                                     &optional root verbose)
  "Auxiliary procedure that adds all the files of a SYSTEM to the
MAPPING with a given SYSTEM-BASE and SYSTEM-NAME. The whole idea of
this procedure is to modify MAPPING, so a NIL is returned."
  (let ((abcl-file-type "abcl"))
    (loop :for component :in (all-files system) 
       :for source = (slot-value component 'asdf::absolute-pathname)
       :for source-entry = (archive-relative-path system-base system-name source)
       :do (setf (gethash source mapping)
                 (if root 
                     (merge-pathnames source-entry (make-pathname :directory root))
                     source-entry))
       :do (format verbose "~&~A~& => ~A" source source-entry)
       :when (and (typep component 'asdf::source-file)
                  (not (typep component 'asdf::static-file)))
       :do (let ((output 
                  (make-pathname
                   :defaults (asdf:apply-output-translations source)
                   :type abcl-file-type))
                 (output-entry 
                  (make-pathname :defaults source-entry 
                                 :type abcl-file-type
                                 :directory
                                 (append root
                                         (cadr (pathname-directory source-entry))))))
             (format verbose "~&~A~& => ~A" output output-entry)
             (setf (gethash output mapping)
                   output-entry)))))

(defun systems->hash-table (systems &optional root verbose)
  "Auxiliary function that, given a list of SYSTEMS, builds a hash
table mapping absolute file names to of these systems into relative
path names. This mapping will be used to zip the files of the system
into a JAR file."
  (let ((mapping (make-hash-table :test 'equal)))
    (dolist (system systems)
      (let ((base (slot-value system 'asdf::absolute-pathname))
            (name (slot-value system 'asdf::name))
            (asdf (slot-value system 'asdf::source-file)))
        (setf (gethash asdf mapping)
              (let ((relative-path (archive-relative-path base name asdf)))
                (if root
                    (merge-pathnames
                     relative-path
                     (make-pathname :directory root))
                    relative-path)))
        (add-system-files-to-mapping! system mapping base name root verbose)))
    mapping))

(defun package (system &key 
                         (out #p"/var/tmp/") 
                         (recursive t)          ; whether to package dependencies
                         (force nil)            ; whether to force ASDF compilation
                         (root nil)
                         (verbose t))
"Compile and package the asdf SYSTEM in a jar.

When RECURSIVE is true (the default), recursively add all asdf
dependencies into the same jar.

Place the resulting packaged jar in the OUT directory.

If FORCE is true, force asdf to recompile all the necessary fasls.

VERBOSE controls how many messages will be logged to
*standard-output*.

ROOT controls if the relative pathnames will be appended to something
before being added to the mapping. The purpose of having this option
is to add the paths to an internal directory, such as (list :relative
\"META-INF\" \"resources\") for generating WAR files.

Returns the pathname of the packaged jar archive.
"
  (when (not (typep system 'asdf:system))
             (setf system (asdf:find-system system)))
  (let* ((name 
          (slot-value system 'asdf::name))
         (version (let ((v (slot-value system 'asdf:version)))
                    (when v
                      v)))
         (package-jar-name 
          (format nil "~A~A~A" name (if recursive "-all" "")
                  (if version 
                      (format nil "-~A" version)
                      "")))
         (package-jar
          (make-pathname :name package-jar-name
                         :type "jar"
                         :defaults out)))
    (when verbose 
      (format verbose "~&Packaging ASDF definition of ~A" system))
    (when (and verbose force)
      (format verbose "~&Forcing recursive compilation of ~A." package-jar))
    (asdf:compile-system system :force force)
    (when verbose
      (format verbose "~&Packaging contents in ~A" package-jar))
    (system:zip package-jar
                (systems->hash-table 
                 (append (list system) 
                         (when recursive
                           (let ((dependencies (dependent-systems system)))
                             (when (and verbose dependencies) 
                               (format verbose
                                       "~&  with recursive dependencies~{ ~A~^, ~}."
                                       dependencies)
                               (mapcar #'asdf:find-system dependencies)))))
                 root
                 verbose))))

(defun all-files (component)
  (loop :for c 
     :being :each :hash-value :of (slot-value component 'asdf::children-by-name)
     :when (typep c 'asdf:module)
     :append (all-files c)
     :when (typep c 'asdf:source-file)
     :append (list c)))

(defun dependent-systems (system)
  (when (not (typep system 'asdf:system))
             (setf system (asdf:find-system system)))
  (let* ((dependencies (asdf::component-load-dependencies system))
         (sub-depends
          (loop :for dependency :in dependencies
             :for sub = (dependent-systems dependency)
             :when sub :append sub)))
    (remove-duplicates `(,@dependencies ,@sub-depends))))

(defun archive-relative-path (base dir file) 
  (let* ((relative 
          (nthcdr (length (pathname-directory base)) (pathname-directory file)))
         (entry-dir `(:relative ,dir ,@relative)))
    (make-pathname :device nil
                   :directory entry-dir
                   :defaults file)))

(defun tmpdir (name)
  "Return temporary directory."
  (let* ((temp-file (java:jcall "getAbsolutePath" 
                               (java:jstatic "createTempFile" "java.io.File" "foo" "tmp")))
         (temp-path (pathname temp-file)))
    (make-pathname 
     :directory (nconc (pathname-directory temp-path)
                       (list name)))))

(defun add-to-asdf (jar &key (use-jar-fasls t))
  "Make a given JAR output by the package mechanism loadable by asdf.

The parameter passed to :USE-JAR-FASLS determines whether to instruct
asdf to use the fasls packaged in the jar.  If this is nil, the fasls
will be compiled with respect to the usual asdf output translation
conventions."
  (when (not (typep jar 'pathname))
    (setf jar (pathname jar)))
  (when (null (pathname-device jar))
    (setf jar (make-pathname :device (list jar))))

  ;;; Inform ASDF of all the system definitions in the jar
  (loop :for asd 
     :in (directory (merge-pathnames "*/*.asd" jar))
     :do (pushnew (make-pathname :defaults asd
                                 :name nil :type nil)
                  asdf:*central-registry*))

  ;;; Load the FASLs directly from the jar
  (when use-jar-fasls                    
    (asdf:initialize-output-translations
     `(:output-translations (,(merge-pathnames "/**/*.*" jar)) 
                            :inherit-configuration))))

(defun prepare-for-war (system &key 
                                 (out #p"/var/tmp/") 
                                 (recursive nil)          ; whether to package dependencies
                                 (force nil)            ; whether to force ASDF compilation
                                 (root (list :relative "META-INF" "resources"))
                                 (verbose t))
  "Package named asdf SYSTEM for deployment in a Java Servlet container war file. 

c.f. PACKAGE for further options."

  (package system :out out :recursive recursive :force force :verbose verbose
           :root root))


(provide :asdf-jar)
