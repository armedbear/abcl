(defpackage #:asdf-jar
  (:use :cl)
  (:export #:package))

(in-package :asdf-jar)

(defvar *debug* nil)

(defun package (system-name 
                &key (out #p"/var/tmp/") 
                     (recursive t)          ; whether to package dependencies
                     (force nil)              ; whether to force ASDF compilation
                     (verbose t))
"Compile and package the asdf SYSTEM-NAME in a jar.

When RECURSIVE is true (the default), recursively add all asdf
dependencies into the same jar.

Place the resulting packaging in the OUT directory.

Returns the pathname of the created jar archive.
"
  (let* ((system 
          (asdf:find-system system-name))
	 (name 
          (slot-value system 'asdf::name))
         (version 
          (handler-case (slot-value system 'asdf:version)
            (unbound-slot () "unknown")))

         (package-jar-name 
          (format nil "~A~A-~A.jar" name (if recursive "-all" "") version))
         (package-jar
          (make-pathname :directory (pathname-directory out) :defaults package-jar-name))
         (mapping (make-hash-table :test 'equal))
         (dependencies (dependent-systems system)))
    (when verbose 
      (format verbose "~&Packaging ASDF definition of ~A~&  as ~A." system package-jar))
    (when (and verbose force)
      (format verbose "~&Forcing recursive compilation of ~A." package-jar))
    (asdf:compile-system system :force force)
    (when verbose
      (format verbose "~&Packaging contents in ~A" package-jar))
    (when (and verbose recursive) 
      (format verbose "~&  with recursive dependencies~{ ~A~^, ~}." dependencies))
    (dolist (system (append (list system) 
                            (when recursive 
                              (mapcar #'asdf:find-system dependencies))))
      (let ((base (slot-value system 'asdf::absolute-pathname))
            (name (slot-value system 'asdf::name))
            (asdf (slot-value system 'asdf::source-file)))
        (setf (gethash asdf mapping) (relative-path base name asdf))
        (loop :for component :in (all-files system) 
           :for source = (slot-value component 'asdf::absolute-pathname)
           :for source-entry = (relative-path base name source)
           :do (setf (gethash source mapping)
                     source-entry)
           :do (when *debug*
                 (format verbose "~&~A~& => ~A" source source-entry))
           :when (and (typep component 'asdf::source-file)
                      (not (typep component 'asdf::static-file)))
           :do (let ((output 
                      (make-pathname
                       :defaults (asdf:apply-output-translations source)
                       :type "abcl"))
                     (output-entry 
                      (make-pathname :defaults source-entry
                                     :type "abcl")))
                 (when *debug*
                   (format verbose "~&~A~& => ~A" output output-entry))
                 (setf (gethash output mapping)
                       output-entry)))))
    (system:zip package-jar mapping)))

(defun all-files (component)
  (loop :for c 
     :being :each :hash-value :of (slot-value component 'asdf::components-by-name)
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

(defun relative-path (base dir file) 
  (let* ((relative 
          (nthcdr (length (pathname-directory base)) (pathname-directory file)))
         (entry-dir `(:relative ,dir ,@(when relative relative))))
    (make-pathname :directory entry-dir
                   :defaults file)))

(defun tmpdir (name)
  "Return temporary directory."
  (let* ((temp-file (java:jcall "getAbsolutePath" 
                               (java:jstatic "createTempFile" "java.io.File" "foo" "tmp")))
         (temp-path (pathname temp-file)))
    (make-pathname 
     :directory (nconc (pathname-directory temp-path)
                       (list name)))))
















    
	

  
  
