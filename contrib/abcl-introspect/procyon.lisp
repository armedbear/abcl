(in-package :cl-user)
(defpackage :abcl-introspect/jvm/tools/procyon
    (:use :cl)
    (:export
     #:disassemble-class-bytes))

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
(in-package :abcl-introspect/jvm/tools/procyon)

(in-package :abcl-introspect/jvm/tools/procyon)
(defun disassemble-class-bytes (object)
  (error "Unimplemented use of procyon dissassembler."))
