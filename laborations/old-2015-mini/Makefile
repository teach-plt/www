# John, 2015-11-09 Makefile for mini

# Andreas, 2016-11-16 "mini" not maintained in PLT 2016 LP2

.PHONY : all www

all : www

www : mini.html
	(cd haskell ; txt2tags --style=../../../style.css -t html index.txt)
	(cd java1.5 ; txt2tags --style=../../../style.css -t html index.txt)

%.html : %.txt
	txt2tags --style=../../style.css -t html $<

clean :
	rm -f mini.html haskell/index.html java1.5/index.html

# EOF
