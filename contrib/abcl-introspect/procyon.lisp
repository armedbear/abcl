(defpackage :abcl-introspect/jvm/tools/procyon
    (:use :cl)
    (:export
     #:disassemble-class-bytes))
(in-package :abcl-introspect/jvm/tools/procyon)


(defun bytes->temp-file (bytes)
  (let* ((create-temp-file-method
           (java:jmethod "java.io.File" "createTempFile"
                         "java.lang.String" "java.lang.String" "java.io.File"))
         (write-method (java:jmethod "java.io.FileOutputStream" "write" "byte[]"))
         (temp-file (java:jstatic create-temp-file-method "java.io.File"
                                  (symbol-name (gensym "classfile")) ".class" java:+null+))
         (temp-file-output-stream (java:jnew "java.io.FileOutputStream" temp-file))
         (to-string-method (java:jmethod "java.lang.Object" "toString")))
    (java:jcall write-method temp-file-output-stream bytes)
    (java:jcall to-string-method temp-file)))

(defun disassemble-class-bytes (object)
  #|
  <https://bitbucket.org/mstrobel/procyon/wiki/Decompiler%20API>

    final DecompilerSettings settings = DecompilerSettings.javaDefaults();

    try (final FileOutputStream stream = new FileOutputStream("path/to/file");
         final OutputStreamWriter writer = new OutputStreamWriter(stream)) {

        Decompiler.decompile(
            "java/lang/String",
            new PlainTextOutput(writer),
            settings
        );
    }
    catch (final IOException e) {
        // handle error
    }
  |#
  (let ((decompile-method (java:jmethod "com.strobel.decompiler.Decompiler"
                                        "decompile" "java.lang.String"
                                        "com.strobel.decompiler.ITextOutput"))
        (string-writer (java:jnew "java.io.StringWriter")))
    (java:jstatic decompile-method
                  "com.strobel.decompiler.Decompiler"
                  (bytes->temp-file object)
                  (java:jnew "com.strobel.decompiler.PlainTextOutput" string-writer))
    (java:jcall (java:jmethod "java.io.StringWriter" "toString") string-writer)))



(eval-when (:load-toplevel :execute)
  (pushnew `(:procyon . abcl-introspect/jvm/tools/procyon::disassemble-class-bytes)
           sys::*disassemblers*)
  (format cl:*load-verbose* "~&; ~a: Successfully added procyon disassembler.~%" *package*))
