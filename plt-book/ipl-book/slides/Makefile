# Makefile for slides (pdf) from txt

pdflatex ?= pdflatex

.PHONY: slides
slides: \
  0-slides-ipl-book.pdf \
  1-slides-ipl-book.pdf \
  2-slides-ipl-book.pdf \
  3-slides-ipl-book.pdf \
  4-slides-ipl-book.pdf \
  5-slides-ipl-book.pdf \
  6-slides-ipl-book.pdf \
  7-slides-ipl-book.pdf \
  8-slides-ipl-book.pdf

%-slides-ipl-book.pdf : %-slides-ipl-book.txt Makefile
	txt2tags -ttex $*-slides-ipl-book.txt
	cat slide-prelude $*-slides-ipl-book.tex >tmp.tex
#	runghc SlideMassage
	mv tmp.tex $*-slides-ipl-book.tex
	$(pdflatex) $*-slides-ipl-book.tex

.PHONY: clean
clean:
	rm *.aux *.log *.out

# EOF
