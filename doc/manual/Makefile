
all: abcl.pdf

abcl.pdf: abcl.tex abcl.bib java.tex extensions.tex threads.tex
	pdflatex abcl.tex
	bibtex abcl
	makeindex abcl
	pdflatex abcl.tex
	pdflatex abcl.tex

#ABCL   = abcl
ABCL	= ../../abcl
#ABCL	= /Users/evenson/work/abcl-1.1.x/abcl

# XXX TODO 1) use more compact iterated form, 2) pare down to using --noinit
grovel:
	$(ABCL) --batch --noinform --load "grovel.lisp" \
	  --eval '(grovel-docstrings-as-tex (find-package :java)' \
	  --eval  '(grovel-docstrings-as-tex (find-package :extensions))' \
	--eval  '(grovel-docstrings-as-tex (find-package :threads))' \
	--eval  '(grovel-docstrings-as-tex (find-package :system))' \
	--eval '(progn (require :abcl-contrib) (require :jss) (grovel-docstrings-as-tex (find-package :jss)))'

clean:
	rm -f *.aux *.bbl *.blg *.idx *.ilg *.ind *.log *.out *.toc abcl.pdf
