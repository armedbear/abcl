(defpackage :abcl-introspect/jvm/tools/procyon
    (:use :cl)
    (:export
     #:disassemble-class-bytes))
(in-package :abcl-introspect/jvm/tools/procyon)

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
  (let* ((settings
           (#"javaDefaults" 'DecompilerSettings))
         (writer
           (jss:new 'StringWriter)))
    (#"decompile" 'Decompiler
                  ;;; !!! need to reference as a type in the current VM
                  ;;; c.f.<https://github.com/Konloch/bytecode-viewer/blob/master/src/the/bytecode/club/bytecodeviewer/decompilers/ProcyonDecompiler.java>
                  object
                  (jss:new 'PlainTextOutput writer)
                  settings)
    (write (#"toString writer"))))

(eval-when (:load-toplevel :execute)
  (pushnew `(:procyon . abcl-introspect/jvm/tools/procyon::disassemble-class-bytes)
           sys::*disassemblers*)
  (format cl:*load-verbose* "~&; ~a: Successfully added procyon disassembler.~%" *package*))






  
  
