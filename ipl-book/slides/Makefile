# Makefile for slides (pdf) from txt

.PHONY: 1 2 3 4 5 6 7 8

slides: 1 2 3 4 5 6 7 8

% : %-slides-ipl-book.pdf

%-slides-ipl-book.pdf : %-slides-ipl-book.txt
	txt2tags -ttex $*-slides-ipl-book.txt
	cat slide-prelude $*-slides-ipl-book.tex >tmp.tex
#	runghc SlideMassage
	mv tmp.tex $*-slides-ipl-book.tex
	pdflatex $*-slides-ipl-book.tex

# EOF
