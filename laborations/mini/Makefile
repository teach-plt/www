# John, 2015-11-09 Makefile for mini

.PHONY : all www

all : www

www : mini.html
	(cd haskell ; txt2tags --style=../../../style.css -t html index.txt)
	(cd java1.5 ; txt2tags --style=../../../style.css -t html index.txt)

%.html : %.txt
	txt2tags --style=../../style.css -t html $<

# EOF
