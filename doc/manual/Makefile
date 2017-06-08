
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
	$(ABCL) --batch --noinform \
	--eval '(require :abcl-contrib)' \
	--eval '(require :jss)' \
	--eval '(asdf:load-system :abcl/documentation)' \
	--eval '(in-package :abcl/documentation)' \
	--eval '(grovel-docstrings-as-tex :package :java)' \
	--eval '(grovel-docstrings-as-tex :package :extensions)' \
	--eval '(grovel-docstrings-as-tex :package :threads)' \
	--eval '(grovel-docstrings-as-tex :package :system)' \
	--eval '(grovel-docstrings-as-tex :package :jss)'

clean:
	rm -f *.aux *.bbl *.blg *.idx *.ilg *.ind *.log *.out *.toc abcl.pdf
