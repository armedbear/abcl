;;; This file is part of ABCL contrib
;;;
;;; Copyright 2011 Mark <evenson@panix.com>

(in-package #:asdf-jar)

(defun add-system-files-to-mapping! (system
                                     mapping
                                     system-base
                                     system-name
                                     root
                                     &key
                                       (fasls t)
                                       (verbose nil))
  "Add all the files of a SYSTEM to the MAPPING with a given
SYSTEM-BASE and SYSTEM-NAME. 

This function destructively modifies MAPPING returning nil."
  (let ((abcl-file-type "abcl"))
    (loop
      :for component :in (all-files system) 
      :for source = (asdf/component:component-pathname component)
      :for source-entry = (merge-pathnames
                           (archive-relative-path system-base system-name source)
                           (make-pathname :directory root))
      :do (setf (gethash source mapping)
                source-entry)
      :do (format verbose "~&~A~%~T=>~A~%" source source-entry)
      :when (and fasls
                 (typep component 'asdf::source-file)
                 (not (typep component 'asdf::static-file)))
        :do (let ((output 
                    (make-pathname
                     :defaults (asdf:apply-output-translations source)
                     :type abcl-file-type))
                  (output-entry 
                    (make-pathname :defaults source-entry 
                                   :type abcl-file-type)))
              (format verbose "~&~A~% => ~A~%" output output-entry)
              (setf (gethash output mapping)
                    output-entry)))))

(defun systems->hash-table (systems root &key (fasls t) (verbose nil))
  "Build a hash table from a list of SYSTEMS mapping absolute file
names to of these systems into relative path names under the pathname
directory component ROOT.

This mapping will be used to zip the files of the system
into a jar file."
  (let ((mapping (make-hash-table :test 'equal)))
    (dolist (system systems)
      (let ((base (slot-value system 'asdf::absolute-pathname))
            (name (slot-value system 'asdf::name))
            (asdf (slot-value system 'asdf::source-file)))
        ;; For the purposes of locating their ASDF file, subsystems
        ;; use the name of their parent.
        (let ((position (position #\/ name)))
          (when position
            (setf name
                  (subseq name 0 position)))
          (setf (gethash asdf mapping)
                (let ((relative-path (archive-relative-path base name asdf)))
                  (merge-pathnames
                   relative-path
                   (make-pathname :directory root))))
          (add-system-files-to-mapping! system mapping base name root
                                        :fasls fasls
                                        :verbose verbose))))
    mapping))

(defun package (system &key 
                         (out #p"/var/tmp/") 
                         (recursive t)          ; whether to package dependencies
                         (force nil)            ; whether to force ASDF compilation
                         (fasls nil)  
                         (root '(:relative))
                         (verbose nil))
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

Returns the pathname of the packaged jar archive as the first value,
and the hash of its members source to destination locations as the
second.
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
      (format verbose "~&Packaging ASDF definition of ~A~%" system))
    (when verbose
      (format verbose "~&Performing ~a compilation of ~A.~%"
              (if force
                  "forced"
                  "unforced")
              package-jar))
    (asdf:compile-system system :force force)
    (when verbose
      (format verbose "~&Packaging contents in '~A'.~%" package-jar))
    (let ((hash-table
            (systems->hash-table 
             (append (list system) 
                     (when recursive
                       (let* ((dependencies
                                (dependent-systems system))
                              (washed-dependencies
                                (remove-if-not
                                 (lambda (s)
                                   (if (asdf/component:component-pathname s)
                                       t
                                       (progn 
                                         (when verbose
                                           (format verbose
                                                   "~&Ignoring dependency ~a without associated pathname.~%"
                                                   s))
                                         nil)))
                                 dependencies)))
                         (when (and verbose washed-dependencies) 
                           (format verbose
                                   "~&Packaging with recursive dependencies~{ ~A~^, ~}.~%"
                                   washed-dependencies))
                         (mapcar #'asdf:find-system washed-dependencies))))
             root
             :fasls fasls :verbose verbose)))
      (values
       (system:zip package-jar hash-table)
       hash-table))))

(defun all-files (component)
  (loop :for c 
     :being :each :hash-value :of (slot-value component 'asdf::children-by-name)
     :when (typep c 'asdf:module)
     :append (all-files c)
     :when (typep c 'asdf:source-file)
       :append (list c)))

(defun resolve-system-or-feature (system-or-feature)
  "Resolve SYSTEM-OR-FEATURE to an asdf system"
  (cond
    ((null system-or-feature)
     nil)
    ((and (consp system-or-feature)
          (= (length system-or-feature) 1))
     (asdf:find-system (first system-or-feature)))
    ((and (consp system-or-feature)
          (= (length system-or-feature) 3))
     (destructuring-bind (keyword expression system)
         system-or-feature
       (unless (equalp keyword :feature)
         (error "~a is not a feature expression" system-or-feature))
       (when (uiop/os:featurep expression)
         (asdf:find-system system))))
    ((typep system-or-feature 'asdf:system)
     system-or-feature)
    (t
     (asdf:find-system system-or-feature))))

(defun dependent-systems (system-or-feature)
  (let ((system
          (resolve-system-or-feature system-or-feature)))
    (when system
      (remove-duplicates
       (loop :for dependency
               :in (asdf/component:component-sideway-dependencies system)
             :for resolved-dependency = (resolve-system-or-feature dependency)
             :for dependents = (dependent-systems resolved-dependency)
             :when resolved-dependency
               :collect resolved-dependency
             :when dependents
               :append dependents)))))

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
                                (java:jstatic "createTempFile" "java.io.File"
                                              (symbol-name (gensym)) "tmp")))
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
