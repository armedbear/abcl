
all: abcl.pdf

abcl.pdf: abcl.tex abcl.bib java.tex extensions.tex threads.tex
	pdflatex abcl.tex
	bibtex abcl
	makeindex abcl
	pdflatex abcl.tex
	pdflatex abcl.tex

grovel:
	abcl --batch --noinform --load "grovel.lisp" --eval '(progn (grovel-docstrings-as-tex (find-package :java)) (grovel-docstrings-as-tex (find-package :extensions)) (grovel-docstrings-as-tex (find-package :threads)))'

clean:
	rm -f *.aux *.bbl *.blg *.idx *.ilg *.ind *.log *.out *.toc abcl.pdf
