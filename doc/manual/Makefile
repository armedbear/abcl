
all: abcl.pdf

abcl.pdf: abcl.tex abcl.bib java.tex extensions.tex threads.tex
	pdflatex abcl.tex
	bibtex abcl
	makeindex abcl
	pdflatex abcl.tex
	pdflatex abcl.tex

#ABCL   = abcl
ABCL	= ../../abcl

# XXX TODO pare down to using --noinit
grovel:
	$(ABCL) --batch --noinform \
	--eval '(require :abcl-contrib)' \
	--eval '(asdf:make :abcl/documentation)' \
	--eval '(abcl/documentation:index)'

clean:
	rm -f *.aux *.bbl *.blg *.idx *.ilg *.ind *.log *.out *.toc abcl.pdf
