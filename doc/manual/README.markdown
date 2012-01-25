ABCL User Manual
================

With a suitable TexLive installed, to build simply run `make`.  If you
cannot run make, the following sequence of commands also gets you a pdf
of the manual:

    cmd$ pdflatex abcl.tex && bibtex abcl &&  makeindex abcl && pdflatex abcl.tex && pdflatex abcl.tex
