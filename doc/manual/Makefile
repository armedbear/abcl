
all: abcl.pdf

abcl.pdf: abcl.tex abcl.bib
	pdflatex abcl.tex
	bibtex abcl
	makeindex abcl
	pdflatex abcl.tex
	pdflatex abcl.tex

clean:
	rm -f *.aux *.bbl *.blg *.idx *.ilg *.ind *.log *.out *.toc abcl.pdf
