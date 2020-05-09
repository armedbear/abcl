(in-package :cl-user)
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
  (with-temp-file (p)
    (alexandria:with-output-to-file (o p)
      (write (get-bytes object) :stream o)
    (let* ((settings
             (#"javaDefaults" 'DecompilerSettings))
           (stream
             (jss:new 'FileOutputStream (namestring p)))
           (writer
             (jss:new 'OutputStreamWriter stream)))
      (#"decompile" 'Decompiler
                    "java/lang/String"
                    (jss:new 'PlainTextOutput writer)
                    settings))))

                    
  (error "Unimplemented use of procyon dissassembler."))

