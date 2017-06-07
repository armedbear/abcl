ABCL User Manual
================

With a suitable TexLive installed, to build simply run `make`.  If you
cannot run make, the following sequence of commands also gets you a pdf
of the manual:

    cmd$ pdflatex abcl.tex && bibtex abcl &&  makeindex abcl && pdflatex abcl.tex && pdflatex abcl.tex


## Generating docstrings

1. Ensure that the toplevel 'abcl.asd' is loadable by ASDF.

   If the ABCL source resides in "~/work/abcl/", this can be
   accomplished by creating the file
   <file:~/.config/common-lisp/source-registry.conf.d/abcl.conf> with
   the contents:

    (:tree (:home "work/abcl/"))

2.  Execute the following code from the ABCL REPL:

    (require :abcl-contrib)
    (require :jss)
    (asdf:load-system :abcl/documentation)
    (dolist (package '(:java :ext :sys :jss :mop :threads))
        :doing (abcl/documentation:grovel-docstrings-as-tex :package package))    
    
