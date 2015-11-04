# John, 2015-11-04 Makefile for lab4

.PHONY : all www testsuite

all : www testsuite

testsuite :
	make -C testsuite

www : lab4.html

%.html : %.txt
	txt2tags --style=../../style.css -t html $<

# EOF
